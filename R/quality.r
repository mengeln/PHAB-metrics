###Read in requested observations
setwd("L:/Bioassessment Data Management Tools_RM/R Scripts")
rowinput <- read.csv("input.csv", header=T)
options(useFancyQuotes = F)
rowinput$StationCode <- sQuote(rowinput$StationCode)
SC <- paste(rowinput$StationCode, collapse="")
SC <- gsub("''", "', '", SC)

###Construct Query
select <- "SELECT Sample_Entry.StationCode, Sample_Entry.SampleDate, FieldCollection_Entry.CollectionTime, AnalyteLookUp.AnalyteName, FieldResult_Entry.Result, FieldResult_Entry.ResQualCode, FieldResult_Entry.QACode, FieldResult_Entry.CalibrationDate, FieldResult_Entry.CollectionDeviceCode, CollectionDeviceLookUp.CollectionDeviceName"
from <- "FROM CollectionDeviceLookUp INNER JOIN (AnalyteLookUp INNER JOIN (((Sample_Entry INNER JOIN Location_Entry ON Sample_Entry.SampleRowID = Location_Entry.SampleRowID) INNER JOIN FieldCollection_Entry ON Location_Entry.LocationRowID = FieldCollection_Entry.LocationRowID) INNER JOIN (ConstituentLookUp INNER JOIN FieldResult_Entry ON ConstituentLookUp.ConstituentRowID = FieldResult_Entry.ConstituentRowID) ON FieldCollection_Entry.FieldCollectionRowID = FieldResult_Entry.FieldCollectionRowID) ON AnalyteLookUp.AnalyteCode = ConstituentLookUp.AnalyteCode) ON CollectionDeviceLookUp.CollectionDeviceCode = FieldResult_Entry.CollectionDeviceCode"
if(length(rowinput$StationCode) < 50){
where <- paste("WHERE (((Sample_Entry.StationCode) IN (", SC, "))  AND ((AnalyteLookUp.AnalyteCode) In (100,78,3,108,38,24,110,92)) AND ((Sample_Entry.EventCode)='ba'))")}else
  {where <- "WHERE (AnalyteLookUp.AnalyteCode) In (100,78,3,108,38,24,110,92) AND ((Sample_Entry.EventCode)='ba')"}
orderby <- "ORDER BY Sample_Entry.StationCode, Sample_Entry.SampleDate, AnalyteLookUp.AnalyteName"

###Connect to DB
library(RODBC)
mydsn <- odbcConnect("SMCreporter", uid ="GisUser", pwd = "")

###Query

quality <- data.frame(sqlQuery(mydsn, paste(select, from, where, orderby)))
odbcClose(mydsn)

###Metrics##
quality$Result[quality$ResQualCode=="NR"] <- NA
quality$id <- do.call(paste, list(quality$StationCode, quality$SampleDate))
quality$Result[quality$Result==-88] <- NA

XWAK <- tapply(quality$Result[quality$AnalyteName=="Alkalinity as CaCO3"], 
               quality$id[quality$AnalyteName=="Alkalinity as CaCO3"], mean)
XWDO <- tapply(quality$Result[quality$AnalyteName=="Oxygen, Dissolved"], 
               quality$id[quality$AnalyteName=="Oxygen, Dissolved"], mean)
XWPH <- tapply(quality$Result[quality$AnalyteName=="pH"], 
               quality$id[quality$AnalyteName=="pH"], mean)
XWSL <-  tapply(quality$Result[quality$AnalyteName=="Salinity"], 
                quality$id[quality$AnalyteName=="Salinity"], mean)
XWSC <- tapply(quality$Result[quality$AnalyteName=="SpecificConductivity"], 
               quality$id[quality$AnalyteName=="SpecificConductivity"], mean)
XWTC <- tapply(quality$Result[quality$AnalyteName=="Temperature"], 
               quality$id[quality$AnalyteName=="Temperature"], mean)
XWTF <- tapply(quality$Result[quality$AnalyteName=="Temperature"], 
               quality$id[quality$AnalyteName=="Temperature"], function(d)(mean(d)*(9/5))+32)
XWTB <- tapply(quality$Result[quality$AnalyteName=="Turbidity"], 
               quality$id[quality$AnalyteName=="Turbidity"], mean)

quality_metrics <- as.data.frame(matrix(NA, nrow=length(unique(quality$id)), ncol=8))
rownames(quality_metrics)<- unique(quality$id)
quality_metrics[which(rownames(quality_metrics)%in%names(XWAK)), 1]<-XWAK
quality_metrics[which(rownames(quality_metrics)%in%names(XWDO)), 2]<-XWDO
quality_metrics[which(rownames(quality_metrics)%in%names(XWPH)), 3]<-XWPH
quality_metrics[which(rownames(quality_metrics)%in%names(XWSL)), 4]<-XWSL
quality_metrics[which(rownames(quality_metrics)%in%names(XWSC)), 5]<-XWSC
quality_metrics[which(rownames(quality_metrics)%in%names(XWTC)), 6]<-XWTC
quality_metrics[which(rownames(quality_metrics)%in%names(XWTF)), 7]<-XWTF
quality_metrics[which(rownames(quality_metrics)%in%names(XWTB)), 8]<-XWTB
colnames(quality_metrics)<- c("XWAK.result", "XWDO.result", "XWPH.result", "XWSL.result", "XWSC.result",
                              "XWTC.result", "XWTF.result", "XWTB.result")

###Write to file###
fc <- file("water_quality_metrics.csv", open="w")
write.csv(quality_metrics, fc)
close(fc)
print("Water quality complete")


      