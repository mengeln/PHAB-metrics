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
  where <- paste("WHERE (((Sample_Entry.StationCode) IN (", SC, "))  AND ((AnalyteLookUp.AnalyteName) In ('Canopy Cover')) AND ((Sample_Entry.EventCode)='ba'))")}else
    {where <- "WHERE (AnalyteLookUp.AnalyteName) In ('Canopy Cover') AND ((Sample_Entry.EventCode)='ba')"}
orderby <- "ORDER BY Sample_Entry.StationCode, Sample_Entry.SampleDate, AnalyteLookUp.AnalyteName"

###Connect to DB
library(RODBC)
mydsn <- odbcConnect("SMCreporter", uid ="GisUser", pwd = "")

###Query

Densiometer <- data.frame(sqlQuery(mydsn, paste(select, from, where, orderby)))
odbcClose(mydsn)

###Format data table###
Densiometer$Result[Densiometer$ResQualCode=="NR"] <- NA
colnames(Densiometer) <- c("StationCode", "SampleDate", "LocationCode",
                         "AnalyteName", "Result", "ResultQualifierCode", "QACode")
Densiometer$id <- do.call(paste, c(Densiometer[c("StationCode", "SampleDate")]))
x <- as.character(Densiometer$LocationCode)
split <- data.frame(do.call('rbind',strsplit(x, ",")))
colnames(split) <- c("trans", "view")
Densiometer <- cbind(Densiometer, split)

###Calculate XCDENBK###

a <- c(which(Densiometer$view == " LeftViewLeft"), which(Densiometer$view == " RightViewRight"))
XCDENBK_data <- data.frame(cbind(Densiometer$id[a], (Densiometer$Result[a])))
colnames(XCDENBK_data) <- c("id", "Result")
transform <- function(data) as.numeric(as.character(data))*(100/17)
XCDENBK_data$trans <- 1:length(XCDENBK_data$Result)
XCDENBK_data$trans <- (transform(XCDENBK_data$Result))
sumna <- function(data)sum(data, na.rm = T)
XCDENBK_sum <- tapply(XCDENBK_data$trans, XCDENBK_data$id, sumna)
lengthna <- function(data)sum(!is.na(data))
XCDENBK.count <- tapply(XCDENBK_data$trans, XCDENBK_data$id, lengthna)
XCDENBK.result <- XCDENBK_sum/XCDENBK.count
sdna <- function(data)sd(data, na.rm = T)
XCDENBK.sd <- tapply(XCDENBK_data$trans, XCDENBK_data$id, sdna)

###Calculate XCDENMID###
b <- which(1:length(Densiometer$view) != a)
XCDENMID_data <- data.frame(cbind(Densiometer$id[b], (Densiometer$Result[b])))
colnames(XCDENMID_data) <- c("id", "Result")
XCDENMID_data$trans <- 1:length(XCDENMID_data$Result)
XCDENMID_data$trans <- (transform(XCDENMID_data$Result))
XCDENMID_sum <- tapply(XCDENMID_data$trans, XCDENMID_data$id, sumna)
XCDENMID.count <- tapply(XCDENMID_data$trans, XCDENMID_data$id, lengthna)
XCDENMID.result <- XCDENMID_sum/XCDENMID.count
XCDENMID.sd <- tapply(XCDENMID_data$trans, XCDENMID_data$id, sdna)


###Write to file###
results <- cbind(XCDENMID.result, XCDENMID.count, XCDENMID.sd, XCDENBK.result, XCDENBK.count, XCDENBK.sd)
fc <- file("densiometer_metrics.csv", open="w")
write.csv(results, fc)
close(fc)
print("Densiometer done")