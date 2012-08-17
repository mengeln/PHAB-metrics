###Read in requested observations
setwd("L:/Bioassessment Data Management Tools_RM/R Scripts")
rowinput <- read.csv("input.csv", header=T)
options(useFancyQuotes = F)
rowinput$StationCode <- sQuote(rowinput$StationCode)
SC <- paste(rowinput$StationCode, collapse="")
SC <- gsub("''", "', '", SC)

###Construct Query
select <- "SELECT Sample_Entry.StationCode, Sample_Entry.SampleDate, Location_Entry.LocationCode, AnalyteLookUp.AnalyteName, HabitatResult_Entry.Result, HabitatResult_Entry.VariableResult, HabitatResult_Entry.ResQualCode, HabitatResult_Entry.QACode"
from <- "FROM AnalyteLookUp INNER JOIN (((Sample_Entry INNER JOIN Location_Entry ON Sample_Entry.SampleRowID = Location_Entry.SampleRowID) INNER JOIN HabitatCollection_Entry ON Location_Entry.LocationRowID = HabitatCollection_Entry.LocationRowID) INNER JOIN (ConstituentLookUp INNER JOIN HabitatResult_Entry ON ConstituentLookUp.ConstituentRowID = HabitatResult_Entry.ConstituentRowID) ON HabitatCollection_Entry.HabitatCollectionRowID = HabitatResult_Entry.HabitatCollectionRowID) ON AnalyteLookUp.AnalyteCode = ConstituentLookUp.AnalyteCode"
if(length(rowinput$StationCode) < 50){
where <- paste("WHERE (((Sample_Entry.StationCode) IN (", SC, "))  AND ((AnalyteLookUp.AnalyteName) In ('Substrate Size Class', 'Embeddedness', 'CPOM')) AND ((Sample_Entry.EventCode)='ba'))")}else
  {where <- "WHERE (AnalyteLookUp.AnalyteName) In ('Substrate Size Class', 'Embeddedness', 'CPOM') AND ((Sample_Entry.EventCode)='ba')"}
orderby <- "ORDER BY Sample_Entry.StationCode, Sample_Entry.SampleDate, AnalyteLookUp.AnalyteName"

###Connect to DB
library(RODBC)
mydsn <- odbcConnect("SMCreporter", uid ="GisUser", pwd = "")

###Query

substrate1 <- data.frame(sqlQuery(mydsn, paste(select, from, where, orderby)))
odbcClose(mydsn)
substrate <- substrate1
###Format
substrate$VariableResult[substrate$ResQualCode=="NR"] <- NA
colnames(substrate) <- c("StationCode", "SampleDate", "LocationCode",
                       "AnalyteName", "Result", "VariableResult", "ResultQualifierCode", "QACode")
substrate$id <- do.call(paste, c(substrate[c("StationCode", "SampleDate")]))
substrate$VariableResult <- as.character(substrate$VariableResult)
substrate$VariableResult[substrate$VariableResult=="a"]<-"SA"
substrate$VariableResult[substrate$VariableResult=="as"]<-"SA"
substrate$VariableResult[substrate$VariableResult=="ws"]<-"SA"
substrate$VariableResult[substrate$VariableResult=="sn"]<-"SA" 
substrate$VariableResult[substrate$VariableResult=="N"]<-"Not Recorded"
substrate$VariableResult[substrate$VariableResult=="n"]<- "Not Recorded"
substrate$VariableResult[substrate$VariableResult=="LB"]<-"XB"
substrate$VariableResult[substrate$VariableResult=="G"]<- "GC"
substrate$VariableResult[substrate$VariableResult=="FM"]<-"FN"
substrate$VariableResult[substrate$VariableResult=="fd"]<-"SB"
substrate$VariableResult[substrate$VariableResult=="fb"]<-"SB"

sub <- substrate[substrate$AnalyteName =="Substrate Size Class",]

sub$VariableResult[sub$Result>= 1000 & sub$Result<4000] <- "XB"
sub$VariableResult[sub$Result>= 250 & sub$Result<1000] <- "SB"
sub$VariableResult[sub$Result>= 64 & sub$Result<250] <- "CB"
sub$VariableResult[sub$Result>= 16 & sub$Result<64] <- "GC"
sub$VariableResult[sub$Result>= 2 & sub$Result<16] <- "GF"
sub$VariableResult[sub$Result>= 0.06 & sub$Result<2] <- "SA"

###Compute

metric <- c('RS', 'RR', 'RC', 'XB', 'SB', 'CB', 'GC', 'GF', 'SA', 'FN', 'HP', 'WD', 'OT')

sub$VariableResult <- lapply(sub$VariableResult, toupper)

lengths <- function(data){
  length(which(((data != "NOT RECORDED") &(data != "NA"))&(data != "FNOT RECORDED")))}
totals <- tapply(sub$VariableResult, sub$id, lengths)
tnames <- as.vector(dimnames(totals))
qq <-unlist(tnames)
l <- matrix(NA, ncol=length(metric), nrow=length(totals))

for(j in 1:length(qq))
  for(i in 1:length(metric)){
    l[j, i] <- length(which(sub$VariableResult[sub$id == qq[j]] == metric[i]))
}
divd <- function(data) data*100/totals
result <- as.data.frame(apply(l, 2, divd))
if(length(colnames(result))==1){
  result <- t(result)
  rownames(result) <- unique(sub$id)
  result <-as.data.frame(result)
}
colnames(result) <- c("PCT_RS.result","PCT_RR.result","PCT_RC.result","PCT_XB.result","PCT_SB.result","PCT_CB.result",
                      "PCT_GC.result","PCT_GF.result","PCT_SA.result","PCT_FN.result","PCT_HP.result",
                      "PCT_WD.result","PCT_OT.result")

result$PCT_BDRK.result <- result$PCT_RS + result$PCT_RR
result$PCT_BIGR.result <- result$PCT_RS + result$PCT_RR + result$PCT_XB + result$PCT_SB + result$PCT_CB + result$PCT_GC
result$PCT_SFGF.result <- result$PCT_GF + result$PCT_SA + result$PCT_FN
result$PCT_SAFN.result <- result$PCT_SA + result$PCT_FN

###Second set of computation
sub$value <- rep(NA, length(sub$id))
sub$value[which(!is.na(sub$Result))] <- sub$Result[which(!is.na(sub$Result))]
sub$value[which(sub$VariableResult == "RR")] <- 5660.0
sub$value[which(sub$VariableResult == "RS")] <- 5660.0
sub$value[which(sub$VariableResult == "XB")] <- 2500.0
sub$value[which(sub$VariableResult == "SB")] <- 625.0
sub$value[which(sub$VariableResult == "CB")] <- 157.0
sub$value[which(sub$VariableResult == "GC")] <- 40.0
sub$value[which(sub$VariableResult == "GF")] <- 9.0
sub$value[which(sub$VariableResult == "SA")] <- 1.03
sub$value[which(sub$VariableResult == "FN")] <- 0.03
sub$value[which(sub$VariableResult == "HP")] <- 5660.0
sub$value[which(sub$VariableResult == "RC")] <- 5660.0



sub$log <- log10(sub$value)

sumna <- function(data){sum(data, na.rm=T)}
lengthna <- function(data){sum(!is.na(data))}

XSDGM_sum <- tapply(sub$log, sub$id, sumna)
XSDGM_count <- tapply(sub$log, sub$id, lengthna)
XSDGM <- 10^(XSDGM_sum/XSDGM_count)
result$XSDGM.result <- XSDGM

sub$value2 <- rep(NA, length(sub$id))
sub$value2[which(!is.na(sub$Result))] <- sub$Result[which(!is.na(sub$Result))]
sub$value2[which(sub$VariableResult == "RR")] <- NA
sub$value2[which(sub$VariableResult == "RS")] <- NA
sub$value2[which(sub$VariableResult == "XB")] <- 2500.0
sub$value2[which(sub$VariableResult == "SB")] <- 625.0
sub$value2[which(sub$VariableResult == "CB")] <- 157.0
sub$value2[which(sub$VariableResult == "GC")] <- 40.0
sub$value2[which(sub$VariableResult == "GF")] <- 9.0
sub$value2[which(sub$VariableResult == "SA")] <- 1.03
sub$value2[which(sub$VariableResult == "FN")] <- 0.03
sub$value2[which(sub$VariableResult == "HP")] <- NA
sub$value2[which(sub$VariableResult == "RC")] <- NA



sub$log2 <- log10(sub$value2)

XSPDGM_sum <- tapply(sub$log2, sub$id, sumna)
XSPDGM_count <- tapply(sub$log2, sub$id, lengthna)
XSDPGM <- 10^(XSPDGM_sum/XSPDGM_count)
result$XSPGM.result <- XSDPGM

qant <- function(data)quantile(data, c(.5, .1, .25, .75, .9), na.rm=T)
otwd <- which(!(sub$VariableResult %in% c("OT", "WD")))
temp <-tapply(sub$value[otwd], sub$id[otwd], qant)
for(i in 1:length(temp)){
result$SB_PT_D50.result[i] <- temp[[i]][1]
result$SB_PT_D10.result[i] <- temp[[i]][2]
result$SB_PT_D25.result[i] <- temp[[i]][3]
result$SB_PT_D75.result[i] <- temp[[i]][4]
result$SB_PT_D90.result[i] <- temp[[i]][5]
}
OTWDHP <- which(!(sub$VariableResult %in% c("OT", "WD", "HP", "RS", "RR", "RC")))
temp2 <-tapply(sub$value[OTWDHP], sub$id[OTWDHP], qant)
for(i in 1:length(temp2)){
  result$SB_PP_D50.result[i] <- temp2[[i]][1]
  result$SB_PP_D10.result[i] <- temp2[[i]][2]
  result$SB_PP_D25.result[i] <- temp2[[i]][3]
  result$SB_PP_D75.result[i] <- temp2[[i]][4]
  result$SB_PP_D90.result[i] <- temp2[[i]][5]
}
sdna <- function(data) sd(data, na.rm=T)
embed <- substrate[which((substrate$AnalyteName=="Embeddedness")&
  (!(substrate$LocationCode=="X"))),]
XEMBED_sum <- tapply(embed$Result, embed$id, sumna)
XEMBED_count <- tapply(embed$Result, embed$id, lengthna)
result$XEMBED.result <- XEMBED_sum/XEMBED_count
result$XEMBED.count <- XEMBED_count
result$XEMBED.sd <- tapply(embed$Result, embed$id, sdna)

cpom <- substrate[substrate$AnalyteName=="CPOM",]
present <- function(data){
  sum(data == "Present")
}
cpomtotal <- function(data){
  sum((data == "Present") | (data == "Absent"))
}
cpresent <- tapply(cpom$VariableResult, cpom$id, present)
ctotal <- tapply(cpom$VariableResult, cpom$id, cpomtotal)
result$PCT_CPOM.result <- cpresent*100/ctotal

###Write to file

fc <- file("substrate_size_and_composition_metrics.csv", open="w")
write.csv(result, fc)
close(fc)
print("Substrate done")