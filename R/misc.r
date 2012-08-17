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
where <- paste("WHERE (((Sample_Entry.StationCode) IN (", SC, "))  AND ((AnalyteLookUp.AnalyteName) In ('Riffle/Run Channel Alteration', 'Riffle/Run Epifaunal Substrate', 'Riffle/Run Sediment Deposition', 'Dominant Land Use', 'Evidence of Fire', 'Evidence of Recent Rainfall')) AND ((Sample_Entry.EventCode)='ba'))")}else
  {where <- "WHERE (AnalyteLookUp.AnalyteName) In ('Riffle/Run Channel Alteration', 'Riffle/Run Epifaunal Substrate', 'Riffle/Run Sediment Deposition', 'Dominant Land Use', 'Evidence of Fire', 'Evidence of Recent Rainfall') AND ((Sample_Entry.EventCode)='ba')"}
orderby <- "ORDER BY Sample_Entry.StationCode, Sample_Entry.SampleDate, AnalyteLookUp.AnalyteName"

###Connect to DB
library(RODBC)
mydsn <- odbcConnect("SMCreporter", uid ="GisUser", pwd = "")

###Query

misc <- data.frame(sqlQuery(mydsn, paste(select, from, where, orderby)))
odbcClose(mydsn)

###Report###
misc$VariableResult[misc$ResQualCode=="NR"] <- NA
misc$VariableResult <- as.character(misc$VariableResult)
misc$id <- do.call(paste, c(misc[c("StationCode", "SampleDate")]))
NFC_DLU <- tapply(misc$VariableResult[misc$AnalyteName=="Dominant Land Use"],
                  misc$id[misc$AnalyteName=="Dominant Land Use"], invisible)
NFC_EFR <- tapply(misc$VariableResult[misc$AnalyteName=="Evidence of Fire"],
                  misc$id[misc$AnalyteName=="Evidence of Fire"], invisible)
NFC_ERN <- tapply(misc$VariableResult[misc$AnalyteName=="Evidence of Recent Rainfall"],
                  misc$id[misc$AnalyteName=="Evidence of Recent Rainfall"], invisible)
RBP_CHN <- tapply(misc$VariableResult[misc$AnalyteName=="Riffle/Run Channel Alteration"],
                  misc$id[misc$AnalyteName=="Riffle/Run Channel Alteration"], invisible)
RBP_EPI <- tapply(misc$VariableResult[misc$AnalyteName=="Riffle/Run Epifaunal Substrate"],
                  misc$id[misc$AnalyteName=="Riffle/Run Epifaunal Substrate"], invisible)
RBP_SED <- tapply(misc$VariableResult[misc$AnalyteName=="Riffle/Run Sediment Deposition"],
                  misc$id[misc$AnalyteName=="Riffle/Run Sediment Deposition"], invisible)

misc_metrics <- as.data.frame(matrix(NA, nrow=length(unique(misc$id)), ncol=6))
rownames(misc_metrics)<- unique(misc$id)
misc_metrics[which(rownames(misc_metrics)%in%names(NFC_DLU)), 1]<-NFC_DLU
misc_metrics[which(rownames(misc_metrics)%in%names(NFC_EFR)), 2]<-NFC_EFR
misc_metrics[which(rownames(misc_metrics)%in%names(NFC_ERN)), 3]<-NFC_ERN
misc_metrics[which(rownames(misc_metrics)%in%names(RBP_CHN)), 4]<-RBP_CHN
misc_metrics[which(rownames(misc_metrics)%in%names(RBP_EPI)), 5]<-RBP_EPI
misc_metrics[which(rownames(misc_metrics)%in%names(RBP_SED)), 6]<-RBP_SED
colnames(misc_metrics)<- c("NFC_DLU.result", "NFC_EFR.result", "NFC_ERN.result",
                           "RBP_CHN.result", "RBP_EPI.result", "RBP_SED.result")

###Write to file###
fc <- file("misc_metrics.csv", open="w")
write.csv(misc_metrics, fc)
close(fc)
print("misc complete")