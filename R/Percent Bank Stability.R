###Read in requested observations
setwd("L:/Bioassessment Data Management Tools_RM/R Scripts")
rowinput <- read.csv("input.csv", header=T)
options(useFancyQuotes = F)
rowinput$StationCode <- sQuote(rowinput$StationCode)
SC <- paste(rowinput$StationCode, collapse="")
SC <- gsub("''", "', '", SC)

###Construct Query
select <- "SELECT Sample_Entry.StationCode, Sample_Entry.SampleDate, Location_Entry.LocationCode, AnalyteLookUp.AnalyteName, HabitatResult_Entry.VariableResult, HabitatResult_Entry.ResQualCode, HabitatResult_Entry.QACode"
from <- "FROM AnalyteLookUp INNER JOIN (((Sample_Entry INNER JOIN Location_Entry ON Sample_Entry.SampleRowID = Location_Entry.SampleRowID) INNER JOIN HabitatCollection_Entry ON Location_Entry.LocationRowID = HabitatCollection_Entry.LocationRowID) INNER JOIN (ConstituentLookUp INNER JOIN HabitatResult_Entry ON ConstituentLookUp.ConstituentRowID = HabitatResult_Entry.ConstituentRowID) ON HabitatCollection_Entry.HabitatCollectionRowID = HabitatResult_Entry.HabitatCollectionRowID) ON AnalyteLookUp.AnalyteCode = ConstituentLookUp.AnalyteCode"
if(length(rowinput$StationCode) < 50){
where <- paste("WHERE (((Sample_Entry.StationCode) IN (", SC, "))  AND ((AnalyteLookUp.AnalyteName) In ('Bank Stability')) AND ((Sample_Entry.EventCode)='ba'))")}else
  {where <- "WHERE (AnalyteLookUp.AnalyteName) In ('Bank Stability') AND ((Sample_Entry.EventCode)='ba')"}
orderby <- "ORDER BY Sample_Entry.StationCode, Sample_Entry.SampleDate, AnalyteLookUp.AnalyteName"

###Connect to DB
library(RODBC)
mydsn <- odbcConnect("SMCreporter", uid ="GisUser", pwd = "")

###Query

Bank_stability <- data.frame(sqlQuery(mydsn, paste(select, from, where, orderby)))
odbcClose(mydsn)

###Format data table###
Bank_stability$VariableResult[Bank_stability$ResQualCode=="NR"] <- NA
colnames(Bank_stability) <- c("StationCode", "SampleDate", "LocationCode",
                         "AnalyteName", "VariableResult", "ResultQualifierCode", "QACode")
Bank_stability$id <- do.call(paste, c(Bank_stability[c("StationCode", "SampleDate")]))

###PBM_S###

stable <- function(data){
    length(which(data == "stable"))
}
total <- function(data){length(c(which(data == "stable"), 
                                 which(data == "vulnerable"), which(data == "eroded")))}
PBM_S_sum <- tapply(Bank_stability$VariableResult, Bank_stability$id, stable)
total_obs <- tapply(Bank_stability$VariableResult, Bank_stability$id, total)
PBM_S.result <- (PBM_S_sum/total_obs)*100

###PBM_V###
vulnerable <- function(data){
  length(which(data == "vulnerable"))
}
PBM_V_sum <- tapply(Bank_stability$VariableResult, Bank_stability$id, vulnerable)
PBM_V.result <- (PBM_V_sum/total_obs)*100

###PBM_E###
eroded <- function(data){
  length(which(data == "eroded"))
}
PBM_E_sum <- tapply(Bank_stability$VariableResult, Bank_stability$id, eroded)
PBM_E.result <- (PBM_E_sum/total_obs)*100

###Write to file###
results <- cbind(PBM_S.result, PBM_V.result, PBM_E.result)
fc <- file("Percent_Bank_Stability_metrics.csv", open="w")
write.csv(results, fc)
close(fc)
print("Percent stability done")