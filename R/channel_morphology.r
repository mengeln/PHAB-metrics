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
where <- paste("WHERE (((Sample_Entry.StationCode) IN (", SC, ")) AND ((AnalyteLookUp.AnalyteName) In ('Cascade/Falls', 'Dry', 'Glide', 'Pool', 'Rapid', 'Riffle', 'Run')) AND ((Sample_Entry.EventCode)='ba'))")}else
  {where <- "WHERE (AnalyteLookUp.AnalyteName) In ('Cascade/Falls', 'Dry', 'Glide', 'Pool', 'Rapid', 'Riffle', 'Run') AND ((Sample_Entry.EventCode)='ba')"}
orderby <- "ORDER BY Sample_Entry.StationCode, Sample_Entry.SampleDate, AnalyteLookUp.AnalyteName"

###Connect to DB
library(RODBC)
mydsn <- odbcConnect("SMCreporter", uid ="GisUser", pwd = "")

###Query

channel_morph <- data.frame(sqlQuery(mydsn, paste(select, from, where, orderby)))
odbcClose(mydsn)

###Format data table###
channel_morph$Result[channel_morph$ResQualCode=="NR"] <- NA
colnames(channel_morph) <- c("StationCode", "SampleDate", "LocationCode",
                             "AnalyteName", "Result", "ResultQualifierCode", "QACode")
channel_morph$id <- do.call(paste, c(channel_morph[c("StationCode", "SampleDate")]))

###Compute PCT_CF###

PCT_CF_data <- data.frame(cbind(channel_morph$id[which(channel_morph$AnalyteName == "Cascade/Falls")], channel_morph$Result[which(channel_morph$AnalyteName == "Cascade/Falls")]))
colnames(PCT_CF_data) <- c("id", "result")

sumna <- function(data)sum(as.numeric(as.character(data)), na.rm = T) 
sdna <- function(data)sd(data, na.rm = T)
lengthna <- function(data)sum(!is.na(data))

PCT_CF_sum <- tapply(PCT_CF_data$result, PCT_CF_data$id, sumna)
PCT_CF.count <- tapply(PCT_CF_data$result, PCT_CF_data$id, lengthna)
PCT_CF.result <- PCT_CF_sum/PCT_CF.count
PCT_CF.sd <- tapply(PCT_CF_data$result, PCT_CF_data$id, sdna)

###PCT_DR###

PCT_DR_data <- data.frame(cbind(channel_morph$id[which(channel_morph$AnalyteName == "Dry")], channel_morph$Result[which(channel_morph$AnalyteName == "Dry")]))
colnames(PCT_DR_data) <- c("id", "result")

PCT_DR_sum <- tapply(PCT_DR_data$result, PCT_DR_data$id, sumna)
PCT_DR.count <- tapply(PCT_DR_data$result, PCT_DR_data$id, lengthna)
PCT_DR.result <- PCT_DR_sum/PCT_DR.count
PCT_DR.sd <- tapply(PCT_DR_data$result, PCT_DR_data$id, sdna)

###PCT_GL###

PCT_GL_data <- data.frame(cbind(channel_morph$id[which(channel_morph$AnalyteName == "Glide")], channel_morph$Result[which(channel_morph$AnalyteName == "Glide")]))
colnames(PCT_GL_data) <- c("id", "result")

PCT_GL_sum <- tapply(PCT_GL_data$result, PCT_GL_data$id, sumna)
PCT_GL.count <- tapply(PCT_GL_data$result, PCT_GL_data$id, lengthna)
PCT_GL.result <- PCT_GL_sum/PCT_GL.count
PCT_GL.sd <- tapply(PCT_GL_data$result, PCT_GL_data$id, sdna)

###PCT_POOL###

PCT_POOL_data <- data.frame(cbind(channel_morph$id[which(channel_morph$AnalyteName == "Pool")]
                                  , channel_morph$Result[which(channel_morph$AnalyteName == "Pool")]))
colnames(PCT_POOL_data) <- c("id", "result")

PCT_POOL_sum <- tapply(PCT_POOL_data$result, PCT_POOL_data$id, sumna)
PCT_POOL.count <- tapply(PCT_POOL_data$result, PCT_POOL_data$id, lengthna)
PCT_POOL.result <- PCT_POOL_sum/PCT_POOL.count
PCT_POOL.sd <- tapply(PCT_POOL_data$result, PCT_POOL_data$id, sdna)

###PCT_RA###
PCT_RA_data <- data.frame(cbind(channel_morph$id[which(channel_morph$AnalyteName == "Rapid")], channel_morph$Result[which(channel_morph$AnalyteName == "Rapid")]))
colnames(PCT_RA_data) <- c("id", "result")

PCT_RA_sum <- tapply(PCT_RA_data$result, PCT_RA_data$id, sumna)
PCT_RA.count <- tapply(PCT_RA_data$result, PCT_RA_data$id, lengthna)
PCT_RA.result <- PCT_RA_sum/PCT_RA.count
PCT_RA.sd <- tapply(PCT_RA_data$result, PCT_RA_data$id, sdna)

###PCT_RI###

PCT_RI_data <- data.frame(cbind(channel_morph$id[which(channel_morph$AnalyteName == "Riffle")], channel_morph$Result[which(channel_morph$AnalyteName == "Riffle")]))
colnames(PCT_RI_data) <- c("id", "result")

PCT_RI_sum <- tapply(PCT_RI_data$result, PCT_RI_data$id, sumna)
PCT_RI.count <- tapply(PCT_RI_data$result, PCT_RI_data$id, lengthna)
PCT_RI.result <- PCT_RI_sum/PCT_RI.count
PCT_RI.sd <- tapply(PCT_RI_data$result, PCT_RI_data$id, sdna)

###PCT_RN###

PCT_RN_data <- data.frame(cbind(channel_morph$id[which(channel_morph$AnalyteName == "Run")], channel_morph$Result[which(channel_morph$AnalyteName == "Run")]))
colnames(PCT_RN_data) <- c("id", "result")

PCT_RN_sum <- tapply(PCT_RN_data$result, PCT_RN_data$id, sumna)
PCT_RN.count <- tapply(PCT_RN_data$result, PCT_RN_data$id, lengthna)
PCT_RN.result <- PCT_RN_sum/PCT_RN.count
PCT_RN.sd <- tapply(PCT_RN_data$result, PCT_RN_data$id, sdna)

###PCT_FAST###

PCT_FAST.result <- PCT_CF.result + PCT_RA.result + PCT_RI.result + PCT_RN.result

###PCT_SLOW###

PCT_SLOW.result <- PCT_GL.result + PCT_POOL.result

###PCT_CF_WT###

PCT_CF_WT1 <- (PCT_CF_sum + PCT_RA_sum + PCT_RI_sum + PCT_RN_sum + PCT_GL_sum + PCT_POOL_sum)/100
PCT_CF_WT2 <- PCT_CF_sum*PCT_CF_WT1
PCT_CF_WT.result <- PCT_CF_WT2/(PCT_CF.count*10)
PCT_CF_WT.count<- PCT_CF.count

###PCT_GL_WT#

PCT_GL_WT1 <- (PCT_CF_sum + PCT_RA_sum + PCT_RI_sum + PCT_RN_sum + PCT_GL_sum + PCT_POOL_sum)/100
PCT_GL_WT2 <- PCT_GL_sum*PCT_GL_WT1
PCT_GL_WT.result <- PCT_GL_WT2/(PCT_GL.count*10)
PCT_GL_WT.count<- PCT_GL.count

###PCT_POOL_WT###

PCT_POOL_WT1 <- (PCT_CF_sum + PCT_RA_sum + PCT_RI_sum + PCT_RN_sum + PCT_GL_sum + PCT_POOL_sum)/100
PCT_POOL_WT2 <- PCT_POOL_sum*PCT_POOL_WT1
PCT_POOL_WT.result <- PCT_POOL_WT2/(PCT_POOL.count*10)
PCT_POOL_WT.count<- PCT_POOL.count

###PCT_RA_WT###

PCT_RA_WT1 <- (PCT_CF_sum + PCT_RA_sum + PCT_RI_sum + PCT_RN_sum + PCT_GL_sum + PCT_POOL_sum)/100
PCT_RA_WT2 <- PCT_RA_sum*PCT_RA_WT1
PCT_RA_WT.result <- PCT_RA_WT2/(PCT_RA.count*10)
PCT_RA_WT.count<- PCT_RA.count

###PCT_RI_WT###

PCT_RI_WT1 <- (PCT_CF_sum + PCT_RA_sum + PCT_RI_sum + PCT_RN_sum + PCT_GL_sum + PCT_POOL_sum)/100
PCT_RI_WT2 <- PCT_RI_sum*PCT_RI_WT1
PCT_RI_WT.result <- PCT_RI_WT2/(PCT_RI.count*10)
PCT_RI_WT.count<- PCT_RI.count

###PCT_RN_WT###

PCT_RN_WT1 <- (PCT_CF_sum + PCT_RA_sum + PCT_RI_sum + PCT_RN_sum + PCT_GL_sum + PCT_POOL_sum)/100
PCT_RN_WT2 <- PCT_RN_sum*PCT_RN_WT1
PCT_RN_WT.result <- PCT_RN_WT2/(PCT_RN.count*10)
PCT_RN_WT.count<- PCT_RN.count

###PCT_FAST_WT###

PCT_FAST_WT.result <- PCT_CF_WT.result + PCT_RA_WT.result + PCT_RI_WT.result + PCT_RN_WT.result

###PCT_SLOW_WT###

PCT_SLOW_WT.result <- PCT_GL_WT.result + PCT_POOL_WT.result


###Write to file###
results <- cbind(PCT_CF.result, PCT_CF.count, PCT_CF.sd, PCT_DR.result, PCT_DR.count, PCT_DR.sd, PCT_GL.result,
                 PCT_GL.count, PCT_GL.sd, PCT_POOL.result, PCT_POOL.count, PCT_POOL.sd, PCT_RA.result, PCT_RA.count,
                 PCT_RA.sd, PCT_RI.result, PCT_RI.count, PCT_RI.sd, PCT_RN.result, PCT_RN.count, PCT_RN.sd,
                 PCT_FAST.result, PCT_SLOW.result, PCT_CF_WT.result, PCT_CF_WT.count, PCT_GL_WT.result, PCT_GL_WT.count,
                 PCT_POOL_WT.result, PCT_POOL_WT.count, PCT_RA_WT.result, PCT_RA_WT.count, PCT_RI_WT.result, 
                 PCT_RI_WT.count, PCT_RN_WT.result, PCT_RN_WT.count, PCT_FAST_WT.result, PCT_SLOW_WT.result)
fc <- file("channel_morphology_metrics.csv", open="w")
write.csv(results, fc)
close(fc)
print("Channel morphology done")





