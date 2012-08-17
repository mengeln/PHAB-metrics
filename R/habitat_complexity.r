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
where <- paste("WHERE (((Sample_Entry.StationCode) IN (", SC, "))  AND ((AnalyteLookUp.AnalyteName) In ('Fish Cover Macrophytes', 'Fish Cover Artificial Structures', 'Fish Cover Boulders', 'Fish Cover Filamentous Algae', 'Fish Cover Woody Debris >0.3 m', 'Fish Cover Live Trees/Roots', 'Fish Cover Overhang.Veg', 'Fish Cover Woody Debris <0.3 m', 'Fish Cover Undercut Banks')) AND ((Sample_Entry.EventCode)='ba'))")}else
  {where <- "WHERE (AnalyteLookUp.AnalyteName) In ('Fish Cover Macrophytes', 'Fish Cover Artificial Structures', 'Fish Cover Boulders', 'Fish Cover Filamentous Algae', 'Fish Cover Woody Debris >0.3 m', 'Fish Cover Live Trees/Roots', 'Fish Cover Overhang.Veg', 'Fish Cover Woody Debris <0.3 m', 'Fish Cover Undercut Banks') AND ((Sample_Entry.EventCode)='ba')"}
orderby <- "ORDER BY Sample_Entry.StationCode, Sample_Entry.SampleDate, AnalyteLookUp.AnalyteName"

###Connect to DB
library(RODBC)
mydsn <- odbcConnect("SMCreporter", uid ="GisUser", pwd = "")

###Query

habitat <- data.frame(sqlQuery(mydsn, paste(select, from, where, orderby)))
odbcClose(mydsn)

###Format data table###
habitat$VariableResult[habitat$ResQualCode=="NR"] <- NA
colnames(habitat) <- c("StationCode", "SampleDate", "LocationCode",
                         "AnalyteName", "VariableResult", "ResultQualifierCode", "QACode")
habitat$id <- do.call(paste, c(habitat[c("StationCode", "SampleDate")]))

habitat$convert <- rep(NA, length(habitat$StationCode))

habitat$convert<-gsub("1", "5",habitat$VariableResult)
habitat$convert<-gsub("2", "25",habitat$convert)
habitat$convert<-gsub("3", "57.5",habitat$convert)
habitat$convert<-gsub("4", "87.5",habitat$convert)

###Compute Stats
analytes <- c("Fish Cover Macrophytes", "Fish Cover Artificial Structures", "Fish Cover Boulders",
              "Fish Cover Filamentous Algae", "Fish Cover Woody Debris >0.3 m", 
              "Fish Cover Live Trees/Roots", "Fish Cover Overhang.Veg", "Fish Cover Woody Debris <0.3 m",
              "Fish Cover Undercut Banks")
statname <- c("XFC_AQM",  "XFC_HUM",  "XFC_RCK",	"XFC_ALG",	"XFC_LWD",
              "XFC_LTR",	"XFC_OHV",	"XFC_BRS",	"XFC_UCB",	
              "XFC_BIG.result",	"XFC_NAT_EMAP.result",	"XFC_NAT_SWAMP.result",	
              "CFC_AQM.result",	"CFC_HUM.result",	"CFC_RCK.result",	"CFC_ALG.result",	"CFC_LWD.result",
              "CFC_LTR.result",	"CFC_OHV.result",	"CFC_BRS.result",	"CFC_UCB.result",	
              "CFC_ALL_EMAP.result",	"CFC_ALL_SWAMP.result")

result <- matrix(NA, nrow=length(unique(habitat$id)), 
                 ncol=((length(statname[1:9])*3)+length(statname[10:23])))
result <- as.data.frame(result)
rownames(result) <- unique(habitat$id)
snames <- paste(rep(statname[1:9], each=3), c(".result", ".count", ".sd"), sep="")
colnames(result) <- c(snames, statname[10:23])
                                
sumna <- function(data){sum(data, na.rm=T)}
lengthna <- function(data){sum(!is.na(data))}

habitat$convert <- as.numeric(habitat$convert)

for(i in 1:9){
  analyte <- subset(habitat, habitat$AnalyteName == analytes[i])
  analytesum <- tapply(analyte$convert, analyte$id, sumna)
  analytetotal <- tapply(analyte$convert, analyte$id, lengthna)
  analytemean <- analytesum/analytetotal
  analytesd <- tapply(analyte$convert, analyte$id, sd)
  result[[((i-1)*3)+1]] <- analytemean
  result[[((i-1)*3)+2]] <- analytetotal
  result[[((i-1)*3)+3]] <- analytesd
}

result$XFC_BIG.result <-  result$XFC_LWD.result + result$XFC_RCK.result + 
  result$XFC_UCB.result + result$XFC_HUM.result

result$XFC_NAT_EMAP.result <- result$XFC_LWD.result + result$XFC_BRS.result +
  result$XFC_OHV.result + result$XFC_RCK.result + result$XFC_UCB.result

result$XFC_NAT_SWAMP.result <- result$XFC_LWD.result + result$XFC_BRS.result +
  result$XFC_OHV.result + result$XFC_RCK.result + result$XFC_UCB.result +
  result$XFC_LTR.result + result$XFC_AQM.result

habitat$present <- as.numeric(as.character(habitat$VariableResult)) >= 1

for(i in 1:9){
  analyte <- subset(habitat, habitat$AnalyteName == analytes[i])
  analytesum <- tapply(analyte$present, analyte$id, sumna)
  result[[i+30]] <- analytesum
}

t <- result[,c("XFC_AQM.result", "XFC_HUM.result", "XFC_RCK.result", "XFC_ALG.result", "XFC_LWD.result", "XFC_OHV.result", "XFC_BRS.result", "XFC_UCB.result")]
result$CFC_ALL_EMAP.result <- rowSums((t>0), na.rm=T)

q <- result[,c("XFC_LTR.result", "XFC_AQM.result", "XFC_HUM.result", "XFC_RCK.result", "XFC_ALG.result", "XFC_LWD.result", "XFC_OHV.result", "XFC_BRS.result", "XFC_UCB.result")]
result$CFC_ALL_SWAMP.result <- rowSums((q>0), na.rm=T)

###Write to file

fc <- file("habitat_complexity_metrics.csv", open="w")
write.csv(result, fc)
close(fc)
print("Habitat complexity done")