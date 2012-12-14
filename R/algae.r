###Read in requested observations
setwd("L:/Bioassessment Data Management Tools_RM/R Scripts")
rowinput <- read.csv("L:/Bioassessment Data Management Tools_RM/R Scripts/input.csv", header=T)
options(useFancyQuotes = F)
rowinput$StationCode <- sQuote(rowinput$StationCode)
SC <- paste(rowinput$StationCode, collapse="")
SC <- gsub("''", "', '", SC)

###Construct Query
select <- "SELECT Sample_Entry.StationCode, Sample_Entry.SampleDate, Location_Entry.LocationCode, AnalyteLookUp.AnalyteName, HabitatResult_Entry.VariableResult, HabitatResult_Entry.ResQualCode, HabitatResult_Entry.QACode"
from <- "FROM AnalyteLookUp INNER JOIN (((Sample_Entry INNER JOIN Location_Entry ON Sample_Entry.SampleRowID = Location_Entry.SampleRowID) INNER JOIN HabitatCollection_Entry ON Location_Entry.LocationRowID = HabitatCollection_Entry.LocationRowID) INNER JOIN (ConstituentLookUp INNER JOIN HabitatResult_Entry ON ConstituentLookUp.ConstituentRowID = HabitatResult_Entry.ConstituentRowID) ON HabitatCollection_Entry.HabitatCollectionRowID = HabitatResult_Entry.HabitatCollectionRowID) ON AnalyteLookUp.AnalyteCode = ConstituentLookUp.AnalyteCode"
if(length(rowinput$StationCode) < 50){
where <- paste("WHERE (((Sample_Entry.StationCode) IN (", SC, "))  AND ((AnalyteLookUp.AnalyteName) In ('Microalgae Thickness', 'Macrophyte Cover', 'Macroalgae Cover, Attached', 'Macroalgae Cover, Unattached')) AND ((Sample_Entry.EventCode)='ba'))")}else
  {where <- "WHERE (AnalyteLookUp.AnalyteName) In ('Microalgae Thickness', 'Macrophyte Cover', 'Macroalgae Cover, Attached', 'Macroalgae Cover, Unattached') AND ((Sample_Entry.EventCode)='ba')"}
orderby <- "ORDER BY Sample_Entry.StationCode, Sample_Entry.SampleDate, AnalyteLookUp.AnalyteName"

###Connect to DB
library(RODBC)
#mydsn <- odbcConnect("SMCreporter", uid ="GisUser", pwd = "")
mydsn <- odbcConnectAccess("T:/SMCStreamMonitoringProgram/CurrentDatabases/SMCReporter1.0.1d.mdb")

###Query

algae <- data.frame(sqlQuery(mydsn, paste(select, from, where, orderby)))
odbcClose(mydsn)

###Format data table###
algae$VariableResult[algae$ResQualCode=="NR"] <- NA
algae$VariableResult[which(is.na(algae$VariableResult))] <- "Not Recorded"
colnames(algae) <- c("StationCode", "SampleDate", "LocationCode",
                      "AnalyteName", "VariableResult", "ResultQualifierCode", "QACode")
algae$id <- do.call(paste, c(algae[c("StationCode", "SampleDate")]))
algae <- as.data.frame(algae)

###Slice for microalgae###
microalgae <- data.frame(cbind(algae$id[which(algae$AnalyteName == 'Microalgae Thickness')], as.character(algae$VariableResult[which(algae$AnalyteName == 'Microalgae Thickness')])))
colnames(microalgae) <- c("id", "VariableResult")


###Compute PCT_MIATP###

FUN_PCT_MIATP <- function(data){
x <- {1:length(data)}
present_calculation <- function(number){
for(i in 1:length(data)){
	if(data[i] == number ){
		x[i] <- 1
	}else{
		x[i] <- 0
	}
}
present <- sum(x)
return(present) 
}
present_subtotals <- tapply(1:5, as.factor(1:5), present_calculation)
present <- sum(present_subtotals)

total_calculation <- function(number){
for(i in 1:length(data)){
	if(data[i] == number ){
		x[i] <- 1
	}else{
		x[i] <- 0
	}
}
total <- sum(x)
return(total) 
}
total_subtotals <- tapply(0:5, as.factor(0:5), total_calculation)
total <- sum(total_subtotals)
result <- (present/total)*100
return(result)
}
PCT_MIATP.result <- tapply(microalgae$VariableResult, microalgae$id, FUN_PCT_MIATP)


###Compute PCT_MIAT1###

FUN_PCT_MIAT1 <- function(data){
x <- {1:length(data)}
present_calculation <- function(number){
for(i in 1:length(data)){
	if(data[i] == number ){
		x[i] <- 1
	}else{
		x[i] <- 0
	}
}
present <- sum(x)
return(present) 
}
present_subtotals <- tapply(3:5, as.factor(3:5), present_calculation)
present <- sum(present_subtotals)

total_calculation <- function(number){
for(i in 1:length(data)){
	if(data[i] == number ){
		x[i] <- 1
	}else{
		x[i] <- 0
	}
}
total <- sum(x)
return(total) 
}
total_subtotals <- tapply(0:5, as.factor(0:5), total_calculation)
total <- sum(total_subtotals)
result <- (present/total)*100
return(result)
}
PCT_MIAT1.result <- tapply(microalgae$VariableResult, microalgae$id, FUN_PCT_MIAT1)


###Compute PCT_MIAT1P###

FUN_PCT_MIAT1P <- function(data){
x <- {1:length(data)}
present_calculation <- function(number){
for(i in 1:length(data)){
	if(data[i] == number ){
		x[i] <- 1
	}else{
		x[i] <- 0
	}
}
present <- sum(x)
return(present) 
}
present_subtotals <- tapply(3:5, as.factor(3:5), present_calculation)
present <- sum(present_subtotals)

total_calculation <- function(number){
for(i in 1:length(data)){
	if(data[i] == number ){
		x[i] <- 1
	}else{
		x[i] <- 0
	}
}
total <- sum(x)
return(total) 
}
total_subtotals <- tapply(1:5, as.factor(1:5), total_calculation)
total <- sum(total_subtotals)
result <- (present/total)*100
return(result)
}
PCT_MIAT1P.result <- tapply(microalgae$VariableResult, microalgae$id, FUN_PCT_MIAT1P)
PCT_MIAT1P.result[is.na(PCT_MIAT1P.result)] <- 0


###Convert data values for XMIAT and XMIATP###

XMIAT_data <- microalgae$VariableResult
XMIAT_data <- as.character(XMIAT_data)
for(i in 1:length(XMIAT_data)){
	if(XMIAT_data[i] == 1){XMIAT_data[i] <- .25} else
	if(XMIAT_data[i] == 2){XMIAT_data[i] <- .5} else
	if(XMIAT_data[i] == 4){XMIAT_data[i] <- 12.5} else
	if(XMIAT_data[i] == 5){XMIAT_data[i] <- 20}
}
XMIAT_data <- as.numeric(XMIAT_data)
XMIAT_frame <- microalgae
XMIAT_frame$result <- XMIAT_data

###Compute XMIAT###

XMIAT_countss <- function(data){
	XMIAT_count <- sum(!is.na(data))
	return(XMIAT_count)	
}
XMIAT_meanss <- function(data){
	XMIAT_count <- sum(!is.na(data))
	XMIAT_mean <- mean(data, na.rm=TRUE)
	return(XMIAT_mean)	
}
XMIAT_SDSs <- function(data){
	XMIAT_SD <- sd(data, na.rm=TRUE)
	return(XMIAT_SD)
}
XMIAT_countst <- tapply(XMIAT_frame$result, XMIAT_frame$id, XMIAT_countss)
XMIAT_meanst <- tapply(XMIAT_frame$result, XMIAT_frame$id, XMIAT_meanss)
XMIAT_sdst <- tapply(XMIAT_frame$result, XMIAT_frame$id, XMIAT_SDSs)

XMIAT <- cbind(XMIAT_meanst, XMIAT_countst, XMIAT_sdst)
colnames(XMIAT) <- c("XMIAT.result", "XMIAT.count", "XMIAT.sd")

###Compute XMIATP###

XMIATP_countss <- function(data){
	XMIATP_count<- sum(!is.na(data))-length(which(data == 0))
	return(XMIATP_count)	
}
XMIATP_meanss <- function(data){
	XMIATP_count <- sum(!is.na(data))-length(which(data == 0))
	XMIATP_mean<- sum(data, na.rm=TRUE)/XMIATP_count
	if(XMIATP_mean == "NaN"){XMIATP_mean <- 0}
	return(XMIATP_mean)	
}
XMIATP_SDSs <- function(data){
	XMIATP_SD <- sd(data[which(data != 0)], na.rm=TRUE)
	return(XMIATP_SD)
}
XMIATP_countst <- tapply(XMIAT_frame$result, XMIAT_frame$id, XMIATP_countss)
XMIATP_meanst <- tapply(XMIAT_frame$result, XMIAT_frame$id, XMIATP_meanss)
XMIATP_sdst <- tapply(XMIAT_frame$result, XMIAT_frame$id, XMIATP_SDSs)

XMIATP <- cbind(XMIATP_meanst, XMIATP_countst, XMIATP_sdst)
colnames(XMIATP) <- c("XMIATP.result", "XMIATP.count", "XMIATP.sd")
XMIATP

###Slice for macrophyte cover data###

macrophyte_cover <- data.frame(cbind(algae$id[which(algae$AnalyteName == 'Macrophyte Cover')], as.character(algae$VariableResult[which(algae$AnalyteName == 'Macrophyte Cover')])))
colnames(macrophyte_cover) <- c("id", "VariableResult")


###Compute PCT_MCP###

PCT_MCP_stats <- function(data){
	present <- length(which(data == "Present"))
	total <- length(which(data == "Present"))+
	length(which(data == "Absent"))
	result <- (present/total)*100
	return(result)
}
PCT_MCP.result <- tapply(macrophyte_cover$VariableResult, macrophyte_cover$id, PCT_MCP_stats)


###Call macrophyte cover attached data###
macroalgae_cover_attached <- data.frame(cbind(algae$id[which(algae$AnalyteName == 'Macroalgae Cover, Attached')], as.character(algae$VariableResult[which(algae$AnalyteName == 'Macroalgae Cover, Attached')])))
colnames(macroalgae_cover_attached) <- c("id", "VariableResult")

###Compute PCT_MAA###

PCT_MAA_stats <- function(data){
	present <- length(which(data == "Present"))
	total <- length(which(data == "Present"))+
	length(which(data == "Absent"))
	result <- (present/total)*100
	return(result)
}
PCT_MAA.result <- tapply(macroalgae_cover_attached$VariableResult, macroalgae_cover_attached$id, PCT_MAA_stats)

###Call macrophyte cover unattached data###
macroalgae_cover_unattached <- data.frame(cbind(algae$id[which(algae$AnalyteName == 'Macroalgae Cover, Unattached')], as.character(algae$VariableResult[which(algae$AnalyteName == 'Macroalgae Cover, Unattached')])))
colnames(macroalgae_cover_unattached) <- c("id", "VariableResult")

###Compute PCT_MAU###

PCT_MAU_stats <- function(data){
	present <- length(which(data == "Present"))
	total <- length(which(data == "Present"))+
	length(which(data == "Absent"))
	result <- (present/total)*100
	return(result)
}
PCT_MAU.result <- tapply(macroalgae_cover_unattached$VariableResult, macroalgae_cover_unattached$id, PCT_MAA_stats)


###Compute PCT_MAP###

colnames(macroalgae_cover_unattached)<- c("id", "VariableResult2")
macroalgae_cover <- cbind(macroalgae_cover_unattached, macroalgae_cover_attached$VariableResult)
colnames(macroalgae_cover) <- c("id", "unattached", "attached")

macroalgae_cover$PCT_MAP <- 1:length(macrophyte_cover$id)
for(i in 1:length(macrophyte_cover$id)){
	if(((macroalgae_cover$unattached[i] == "Present")|(macroalgae_cover$attached[i] == "Present"))){macroalgae_cover$PCT_MAP[i] <- "Present"} else
	if (((macroalgae_cover$unattached[i] == "Absent")&(macroalgae_cover$attached[i] == "Absent"))) {macroalgae_cover$PCT_MAP[i] <- "Absent"
	}
}

PCT_MAP_stats <- function(data){
	present <- length(which(data=="Present"))
	total <- length(which(data=="Absent"))+present
	result <- 100*(present/total)
	return(result)
}
PCT_MAP.result <- tapply(macroalgae_cover$PCT_MAP, macroalgae_cover$id, PCT_MAP_stats)

###Compute PCT_NSA###

macroalgae_cover$PCT_NSA_characters <- as.character(microalgae$VariableResult)
for(i in 1:length(microalgae$VariableResult)){
	if(microalgae$VariableResult[i] == "3"){macroalgae_cover$PCT_NSA_characters[i] <- "Present"} else
	if(microalgae$VariableResult[i] == "4"){macroalgae_cover$PCT_NSA_characters[i] <- "Present"} else
	if(microalgae$VariableResult[i] == "5"){macroalgae_cover$PCT_NSA_characters[i] <- "Present"} else
	if(microalgae$VariableResult[i] == "0"){macroalgae_cover$PCT_NSA_characters[i] <- "Absent"} else
	if(microalgae$VariableResult[i] == "1"){macroalgae_cover$PCT_NSA_characters[i] <- "Absent"} else
	if(microalgae$VariableResult[i] == "2"){macroalgae_cover$PCT_NSA_characters[i] <- "Absent"} 
}
macroalgae_cover$PCT_NSA <- 1:length(macrophyte_cover$id)
for(i in 1:length(macrophyte_cover$id)){
	if(((macroalgae_cover$PCT_MAP[i] == "Present")|(macroalgae_cover$PCT_NSA_characters[i] == "Present"))){macroalgae_cover$PCT_NSA[i] <- "Present"} else
	if (((macroalgae_cover$PCT_MAP[i] == "Absent")&(macroalgae_cover$PCT_NSA_characters[i] == "Absent"))) {macroalgae_cover$PCT_NSA[i] <- "Absent"
	}
}
PCT_NSA_sum <- function(data){
	result <- length(which(data=="Present"))
	return(result)}

macroalgae_cover$PCT_NSA_total <- 1:length(macroalgae_cover$id)
for(i in 1:length(macroalgae_cover$id)){
	if(macroalgae_cover$unattached[i] == "Dry"){macroalgae_cover$PCT_NSA_total[i] <- "blank"} else{
	if(macroalgae_cover$unattached[i] == "Not Recorded"){macroalgae_cover$PCT_NSA_total[i] <- "blank"} else{
	if(macroalgae_cover$attached[i] == "Not Recorded"){macroalgae_cover$PCT_NSA_total[i] <- "blank"} else{
	if(macroalgae_cover$attached[i] == "Dry"){macroalgae_cover$PCT_NSA_total[i] <- "blank"} else{
	if(macroalgae_cover$PCT_NSA_characters[i] == "Dry"){macroalgae_cover$PCT_NSA_total[i] <- "blank"} else{		
	if(macroalgae_cover$PCT_NSA_characters[i] == "Not Recorded"){macroalgae_cover$PCT_NSA_total[i] <- "blank"}}}}}}}

PCT_NSA_total <- function(data){
	result <- length(which(data != "blank"))
	return(result)}

PCT_NSA_sums <- tapply(macroalgae_cover$PCT_NSA, macroalgae_cover[[1]], PCT_NSA_sum)
PCT_NSA_totals <- tapply(macroalgae_cover$PCT_NSA_total, macroalgae_cover[[1]], PCT_NSA_total)
PCT_NSA.result <- (PCT_NSA_sums/PCT_NSA_totals)*100



###Write the results to file###

algae_results1 <- cbind(PCT_MIATP.result, PCT_MIAT1.result, PCT_MIAT1P.result, PCT_MAA.result, PCT_MCP.result,
                        PCT_MAU.result, PCT_MAP.result, PCT_NSA.result)
algae_results_final <- cbind(XMIAT, XMIATP, algae_results1)
fc <- file("Algae_metrics.csv", open="w")
write.csv(algae_results_final, fc)
close(fc)
print("Algae done")