###Read in requested observations
setwd("L:/Bioassessment Data Management Tools_RM/R Scripts")
rowinput <- read.csv("input.csv", header=T)
options(useFancyQuotes = F)
rowinput$StationCode <- sQuote(rowinput$StationCode)
SC <- paste(rowinput$StationCode, collapse="")
SC <- gsub("''", "', '", SC)

###Construct Query
select <- "SELECT Sample_Entry.StationCode, Sample_Entry.SampleDate, Location_Entry.LocationCode, AnalyteLookUp.AnalyteName, HabitatResult_Entry.Result, HabitatResult_Entry.ResQualCode, HabitatResult_Entry.QACode"
from <- "FROM AnalyteLookUp INNER JOIN (((Sample_Entry INNER JOIN Location_Entry ON Sample_Entry.SampleRowID = Location_Entry.SampleRowID) INNER JOIN HabitatCollection_Entry ON Location_Entry.LocationRowID = HabitatCollection_Entry.LocationRowID) INNER JOIN (ConstituentLookUp INNER JOIN HabitatResult_Entry ON ConstituentLookUp.ConstituentRowID = HabitatResult_Entry.ConstituentRowID) ON HabitatCollection_Entry.HabitatCollectionRowID = HabitatResult_Entry.HabitatCollectionRowID) ON AnalyteLookUp.AnalyteCode = ConstituentLookUp.AnalyteCode"
if(length(rowinput$StationCode) < 50){
where <- paste("WHERE (((Sample_Entry.StationCode) IN (", SC, "))  AND ((AnalyteLookUp.AnalyteName) In ('Bankfull Height', 'Bankfull Width', 'StationWaterDepth', 'Wetted Width')) AND ((Sample_Entry.EventCode)='ba'))")}else
  {where <- "WHERE (AnalyteLookUp.AnalyteName) In ('Bankfull Height', 'Bankfull Width', 'StationWaterDepth', 'Wetted Width') AND ((Sample_Entry.EventCode)='ba')"}
orderby <- "ORDER BY Sample_Entry.StationCode, Sample_Entry.SampleDate, AnalyteLookUp.AnalyteName"

###Connect to DB
library(RODBC)
mydsn <- odbcConnect("SMCreporter", uid ="GisUser", pwd = "")

###Query

bankmorph <- data.frame(sqlQuery(mydsn, paste(select, from, where, orderby)))
odbcClose(mydsn)

###Format data table###
bankmorph$Result[bankmorph$ResQualCode=="NR"] <- NA
colnames(bankmorph) <- c("StationCode", "SampleDate", "LocationCode",
                         "AnalyteName", "Result", "ResultQualifierCode", "QACode")
bankmorph$id <- do.call(paste, c(bankmorph[c("StationCode", "SampleDate")]))
bankmorph <- bankmorph[which(!(bankmorph$LocationCode == "X")),]
bankmorph$Result[bankmorph$Result == -88] <- NA
###Slice Bankfull Height Data ###

heightindex <- which(bankmorph$AnalyteName == "Bankfull Height")
bankfullheight <- data.frame(cbind(bankmorph$id[heightindex], as.character(bankmorph$AnalyteName[heightindex]),
as.character(bankmorph$Result[heightindex])))
colnames(bankfullheight) <- c("id", "AnalyteName", "result")
bankfullheight$result <- as.numeric(as.character(bankfullheight$result))
head(bankfullheight)

###Compute XBKF_H###
sumna <- function(data){
	sum(as.numeric(as.character(data)), na.rm = T)
}
XBKF_sum <- tapply(bankfullheight$result, bankfullheight$id, sumna)
lengthna <- function(data){
	sum(!is.na(data))
}
XBKF_H.count <- tapply(bankfullheight$result, bankfullheight$id, lengthna)
XBKF_H.result <- XBKF_sum/XBKF_H.count
sdna <- function(data){
	sd(data, na.rm = T)
}
XBKF_H.sd <- tapply(bankfullheight$result, bankfullheight$id, sdna)
XBKF_H <- data.frame(cbind(XBKF_H.result, XBKF_H.count, XBKF_H.sd))
XBKF_H

###Slice Bankfull Width###

widthindex <- which(bankmorph$AnalyteName == "Bankfull Width")
bankfullwidth <- data.frame(cbind(bankmorph$id[widthindex], as.character(bankmorph$AnalyteName[widthindex]),
as.character(bankmorph$Result[widthindex])))
colnames(bankfullwidth) <- c("id", "AnalyteName", "result")
bankfullwidth$result <- as.numeric(as.character(bankfullwidth$result))
head(bankfullwidth)

###Compute XKBF_W###

XKBF_W_sum <- tapply(bankfullwidth$result, bankfullwidth$id, sumna)
XKBF_W.count <- tapply(bankfullwidth$result, bankfullwidth$id, lengthna)
XKBF_W.result <- XKBF_W_sum/XKBF_W.count
XKBF_W.sd <- tapply(bankfullwidth$result, bankfullwidth$id, sdna)
XKBF_W <- data.frame(cbind(XKBF_W.result, XKBF_W.count, XKBF_W.sd))
XKBF_W

###XWDEPTH###

XWDEPTHdata <- data.frame(cbind(bankmorph$id[which(bankmorph$AnalyteName == "StationWaterDepth")], 
                                as.numeric(as.character(bankmorph$Result[which(bankmorph$AnalyteName == "StationWaterDepth")]))))
colnames(XWDEPTHdata) <- c("id", "result")
XWDEPTH_sum <- tapply(XWDEPTHdata$result, XWDEPTHdata$id, sumna)
XWDEPTH.count <- tapply(XWDEPTHdata$result, XWDEPTHdata$id, lengthna)
XWDEPTH.result <- XWDEPTH_sum/XWDEPTH.count
XWDEPTH.sd <- tapply(XWDEPTHdata$result, XWDEPTHdata$id, sdna)

###XWIDTH###

XWIDTHdata <- data.frame(cbind(bankmorph$id[which(bankmorph$AnalyteName == "Wetted Width")], 
                               as.numeric(as.character(bankmorph$Result[which(bankmorph$AnalyteName == "Wetted Width")]))))
colnames(XWIDTHdata) <- c("id", "result")
XWIDTH_sum <- tapply(XWIDTHdata$result, XWIDTHdata$id, sumna)
XWIDTH.count <- tapply(XWIDTHdata$result, XWIDTHdata$id, lengthna)
XWIDTH.result <- XWIDTH_sum/XWIDTH.count
XWIDTH.sd <- tapply(XWIDTHdata$result, XWIDTHdata$id, sdna)

###XWDR###

XWDR.result <- (XWIDTH.result/XWDEPTH.result)*100
XWDR.count<- XWIDTH.count
###XWDA###

XWDA.result <- XWDEPTH.result/(XWIDTH.result*10)
XWDA.count <- XWDEPTH.count
###XWDM###
library(reshape)
bankmorph$LocationCode2 <- gsub("Tran.(\\w)", "\\1", bankmorph$LocationCode)

ll <- c( ", LCtr" , ", Ctr" , ", RCtr",
         ", Right", ", Left")
for(i in 1:length(ll)){
  bankmorph$LocationCode2 <-gsub(ll[i], "", bankmorph$LocationCode2)}
XWDM_max <- cast(bankmorph[bankmorph$AnalyteName == "StationWaterDepth", ], 
                 id ~ LocationCode2, value = "Result", fun.aggregate =max)
XWDM_max[XWDM_max == -Inf] <- NA
XWDM_max<- XWDM_max[,which(!(1:length(colnames(XWDM_max)) %in% grep("Float", colnames(XWDM_max))))]
dim <- XWDM_max[[1]]
XWDM_max<-XWDM_max[, which(!(1:(length(XWDM_max)-1) %in% grep("Section", (colnames(XWDM_max)))))]
XWDM_max <- XWDM_max[, 2:length(XWDM_max)]
XWDM.result <- rowSums(XWDM_max, na.rm = T)/apply(XWDM_max, 1, lengthna)
names(XWDM.result) <- dim
XWDM.count <-apply(XWDM_max, 1, lengthna)

###Write to file###
results <- cbind(XBKF_H.result, XBKF_H.count, XBKF_H.sd, XKBF_W.result, XKBF_W.count, XKBF_W.sd, XWDEPTH.result, 
                 XWDEPTH.count, XWDEPTH.sd, XWIDTH.result, XWIDTH.count, XWIDTH.sd, XWDR.result, XWDR.count, 
                 XWDA.result, XWDA.count, XWDM.result , XWDM.count)
fc <- file("Bank_morphology_metrics.csv", open="w")
write.csv(results, fc)
close(fc)
print("Bankmorphology done")
