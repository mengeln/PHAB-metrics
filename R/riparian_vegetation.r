###Read in requested observations
setwd("L:/Bioassessment Data Management Tools_RM/R Scripts")
rowinput <- read.csv("input.csv", header=T)
options(useFancyQuotes = F)
rowinput$StationCode <- sQuote(rowinput$StationCode)
SC <- paste(rowinput$StationCode, collapse="")
SC <- gsub("''", "', '", SC)
options(warn=-1) 
###Construct Query
select <- "SELECT Sample_Entry.StationCode, Sample_Entry.SampleDate, Location_Entry.LocationCode, AnalyteLookUp.AnalyteName, HabitatResult_Entry.VariableResult, HabitatResult_Entry.ResQualCode, HabitatResult_Entry.QACode"
from <- "FROM AnalyteLookUp INNER JOIN (((Sample_Entry INNER JOIN Location_Entry ON Sample_Entry.SampleRowID = Location_Entry.SampleRowID) INNER JOIN HabitatCollection_Entry ON Location_Entry.LocationRowID = HabitatCollection_Entry.LocationRowID) INNER JOIN (ConstituentLookUp INNER JOIN HabitatResult_Entry ON ConstituentLookUp.ConstituentRowID = HabitatResult_Entry.ConstituentRowID) ON HabitatCollection_Entry.HabitatCollectionRowID = HabitatResult_Entry.HabitatCollectionRowID) ON AnalyteLookUp.AnalyteCode = ConstituentLookUp.AnalyteCode"
if(length(rowinput$StationCode) < 50){
where <- paste("WHERE (((Sample_Entry.StationCode) IN (", SC, "))  AND ((AnalyteLookUp.AnalyteName) In ('Riparian GroundCover Barren', 'Riparian GroundCover NonWoody Plants', 'Riparian GroundCover Woody Shrubs', 'Riparian Lower Canopy All Vegetation', 'Riparian Upper Canopy All Trees', 'Riparian Lower Canopy All Vegetation', 'Riparian Upper Canopy All Trees', 'Riparian GroundCover Woody Shrubs', 'Riparian GroundCover NonWoody Plants')) AND ((Sample_Entry.EventCode)='ba'))")}else
  {where <- "WHERE (AnalyteLookUp.AnalyteName) In ('Riparian GroundCover Barren', 'Riparian GroundCover NonWoody Plants', 'Riparian GroundCover Woody Shrubs', 'Riparian Lower Canopy All Vegetation', 'Riparian Upper Canopy All Trees', 'Riparian Lower Canopy All Vegetation', 'Riparian Upper Canopy All Trees', 'Riparian GroundCover Woody Shrubs', 'Riparian GroundCover NonWoody Plants') AND ((Sample_Entry.EventCode)='ba')"}
orderby <- "ORDER BY Sample_Entry.StationCode, Sample_Entry.SampleDate, AnalyteLookUp.AnalyteName"

###Connect to DB
library(RODBC)
mydsn <- odbcConnect("SMCreporter", uid ="GisUser", pwd = "")

###Query

ripveg <- data.frame(sqlQuery(mydsn, paste(select, from, where, orderby)))
odbcClose(mydsn)

###Format data table###
ripveg$VariableResult[ripveg$ResQualCode=="NR"] <- NA
colnames(ripveg) <- c("StationCode", "SampleDate", "LocationCode",
                              "AnalyteName", "VariableResult", "ResultQualifierCode", "QACode")
ripveg$id <- do.call(paste, c(ripveg[c("StationCode", "SampleDate")]))

###Slice Riparian GroundCover Barren Data ###

barrenindex <- which(ripveg$AnalyteName == "Riparian GroundCover Barren")
barren <- data.frame(cbind(ripveg$id[barrenindex], as.character(ripveg$AnalyteName[barrenindex]),
as.character(ripveg$VariableResult[barrenindex])))
colnames(barren) <- c("id", "AnalyteName", "result")
barren$result <- as.numeric(as.character(barren$result))

###Compute XGB###

for(i in (1:length(barren$result))[which(!is.na(barren$result))]){
	if(barren$result[i] == 1){barren$result[i] <- 5} else
	if(barren$result[i] == 2){barren$result[i] <- 25} else
	if(barren$result[i] == 3){barren$result[i] <- 57.5} else
	if(barren$result[i] == 4){barren$result[i] <- 87.5}
}
sumna <- function(data){
	sum(data, na.rm = T)
}
XGB_sum <- tapply(barren$result, barren$id, sumna)
lengthna <- function(data){
	sum(!is.na(data))
}
XGB.count <- tapply(barren$result, barren$id, lengthna)
XGB.result <- XGB_sum/XGB.count
sdna <- function(data){
	sd(data, na.rm = T)
}
XGB.sd <- tapply(barren$result, barren$id, sdna)
XGB <- data.frame(cbind(XGB.result, XGB.count, XGB.sd))

###Slice for Riparian GroundCover Nonwoody Plants###

nonwoodyindex <- which(ripveg$AnalyteName == "Riparian GroundCover NonWoody Plants")
nonwoody <- data.frame(cbind(ripveg$id[nonwoodyindex], as.character(ripveg$AnalyteName[nonwoodyindex]),
as.character(ripveg$VariableResult[nonwoodyindex])))
colnames(nonwoody) <- c("id", "AnalyteName", "result")
nonwoody$result <- as.numeric(as.character(nonwoody$result))
head(nonwoody)

###Compute XGH###
for(i in (1:length(nonwoody$result))[which(!is.na(nonwoody$result))]){
	if(nonwoody$result[i] == 1){nonwoody$result[i] <- 5} else
	if(nonwoody$result[i] == 2){nonwoody$result[i] <- 25} else
	if(nonwoody$result[i] == 3){nonwoody$result[i] <- 57.5} else
	if(nonwoody$result[i] == 4){nonwoody$result[i] <- 87.5}
}
sumna <- function(data){
	sum(data, na.rm = T)
}
XGH_sum <- tapply(nonwoody$result, nonwoody$id, sumna)
lengthna <- function(data){
	sum(!is.na(data))
}
XGH.count <- tapply(nonwoody$result, nonwoody$id, lengthna)
XGH.result <- XGH_sum/XGH.count
sdna <- function(data){
	sd(data, na.rm = T)
}
XGH.sd <- tapply(nonwoody$result, nonwoody$id, sdna)
XGH <- data.frame(cbind(XGH.result, XGH.count, XGH.sd))

###Slice for Riparian GroundCover Woody Shrubs###

woodyindex <- which(ripveg$AnalyteName == "Riparian GroundCover Woody Shrubs")
woody <- data.frame(cbind(ripveg$id[woodyindex], as.character(ripveg$AnalyteName[woodyindex]),
as.character(ripveg$VariableResult[woodyindex])))
colnames(woody) <- c("id", "AnalyteName", "result")
woody$result <- as.numeric(as.character(woody$result))
head(woody)

###Compute XGW###

for(i in (1:length(woody$result))[which(!is.na(woody$result))]){
	if(woody$result[i] == 1){woody$result[i] <- 5} else
	if(woody$result[i] == 2){woody$result[i] <- 25} else
	if(woody$result[i] == 3){woody$result[i] <- 57.5} else
	if(woody$result[i] == 4){woody$result[i] <- 87.5}
}
sumna <- function(data){
	sum(data, na.rm = T)
}
XGW_sum <- tapply(woody$result, woody$id, sumna)
lengthna <- function(data){
	sum(!is.na(data))
}
XGW.count <- tapply(woody$result, woody$id, lengthna)
XGW.result <- XGW_sum/XGW.count
sdna <- function(data){
	sd(data, na.rm = T)
}
XGW.sd <- tapply(woody$result, woody$id, sdna)
XGW <- data.frame(cbind(XGW.result, XGW.count, XGW.sd))

###Slice for Riparian Lower Canopy All Vegetation###

lowercanopyindex <- which(ripveg$AnalyteName == "Riparian Lower Canopy All Vegetation")
lowercanopy <- data.frame(cbind(ripveg$id[lowercanopyindex], as.character(ripveg$AnalyteName[lowercanopyindex]),
as.character(ripveg$VariableResult[lowercanopyindex])))
colnames(lowercanopy) <- c("id", "AnalyteName", "result")
lowercanopy$result <- as.numeric(as.character(lowercanopy$result))
head(lowercanopy)

###Compute XM###

for(i in (1:length(lowercanopy$result))[which(!is.na(lowercanopy$result))]){
	if(lowercanopy$result[i] == 1){lowercanopy$result[i] <- 5} else
	if(lowercanopy$result[i] == 2){lowercanopy$result[i] <- 25} else
	if(lowercanopy$result[i] == 3){lowercanopy$result[i] <- 57.5} else
	if(lowercanopy$result[i] == 4){lowercanopy$result[i] <- 87.5}
}
sumna <- function(data){
	sum(data, na.rm = T)
}
XM_sum <- tapply(lowercanopy$result, lowercanopy$id, sumna)
lengthna <- function(data){
	sum(!is.na(data))
}
XM.count <- tapply(lowercanopy$result, lowercanopy$id, lengthna)
XM.result <- XM_sum/XM.count
sdna <- function(data){
	sd(data, na.rm = T)
}
XM.sd <- tapply(lowercanopy$result, lowercanopy$id, sdna)
XM <- data.frame(cbind(XM.result, XM.count, XM.sd))

###Slice for Riparian Upper Canopy All Trees###

uppercanopyindex <- which(ripveg$AnalyteName == "Riparian Upper Canopy All Trees")
uppercanopy <- data.frame(cbind(ripveg$id[uppercanopyindex], as.character(ripveg$AnalyteName[uppercanopyindex]),
as.character(ripveg$VariableResult[uppercanopyindex])))
colnames(uppercanopy) <- c("id", "AnalyteName", "result")
uppercanopy$result <- as.numeric(as.character(uppercanopy$result))
head(uppercanopy)

###Compute XC###

for(i in (1:length(uppercanopy$result))[which(!is.na(uppercanopy$result))]){
	if(uppercanopy$result[i] == 1){uppercanopy$result[i] <- 5} else
	if(uppercanopy$result[i] == 2){uppercanopy$result[i] <- 25} else
	if(uppercanopy$result[i] == 3){uppercanopy$result[i] <- 57.5} else
	if(uppercanopy$result[i] == 4){uppercanopy$result[i] <- 87.5}
}
sumna <- function(data){
	sum(data, na.rm = T)
}
XC_sum <- tapply(uppercanopy$result, uppercanopy$id, sumna)
lengthna <- function(data){
	sum(!is.na(data))
}
XC.count <- tapply(uppercanopy$result, uppercanopy$id, lengthna)
XC.result <- XC_sum/XC.count
sdna <- function(data){
	sd(data, na.rm = T)
}
XC.sd <- tapply(uppercanopy$result, uppercanopy$id, sdna)
XC <- data.frame(cbind(XC.result, XC.count, XC.sd))


###Compute XG###
XG.result <- XGW$XGW.result + XGH$XGH.result

###Compute XCM###
XCM.result <- XC$XC.result + XM$XM.result

###Compute XCMG###

XCMG.result <- XG.result + XCM.result

###Compute XPMID###

lowercanopyindex <- which(ripveg$AnalyteName == "Riparian Lower Canopy All Vegetation")
lowercanopy <- data.frame(cbind(ripveg$id[lowercanopyindex], as.character(ripveg$AnalyteName[lowercanopyindex]),
as.character(ripveg$VariableResult[lowercanopyindex])))
colnames(lowercanopy) <- c("id", "AnalyteName", "result")
lowercanopy$result <- as.numeric(as.character(lowercanopy$result))
head(lowercanopy)

XPMID_total <- tapply(lowercanopy$result, lowercanopy$id, lengthna)

lowercanopy$result[which(lowercanopy$result == NA)] = 0
XPMID_subcountf <- function(data){
	length(which(data != 0))
}
XPMID_subcount <- tapply(lowercanopy$result, lowercanopy$id, XPMID_subcountf)
XPMID.result <- XPMID_total/XPMID_subcount

###Compute XPCAN###

uppercanopyindex <- which(ripveg$AnalyteName == "Riparian Upper Canopy All Trees")
uppercanopy <- data.frame(cbind(ripveg$id[uppercanopyindex], as.character(ripveg$AnalyteName[uppercanopyindex]),
as.character(ripveg$VariableResult[uppercanopyindex])))
colnames(uppercanopy) <- c("id", "AnalyteName", "result")
uppercanopy$result <- as.numeric(as.character(uppercanopy$result))
head(uppercanopy)

XPCAN_total <- tapply(uppercanopy$result, uppercanopy$id, lengthna)

uppercanopy$result[which(uppercanopy$result == NA)] = 0
XPCAN_subcountf <- function(data){
	length(which(data != 0))
}
XPCAN_subcount <- tapply(uppercanopy$result, uppercanopy$id, XPCAN_subcountf)
XPCAN.result <- XPCAN_subcount/XPCAN_total

###Compute XPGVEG###
woodyindex <- which(ripveg$AnalyteName == "Riparian GroundCover Woody Shrubs")
woody <- data.frame(cbind(ripveg$id[woodyindex], as.character(ripveg$AnalyteName[woodyindex]),
as.character(ripveg$VariableResult[woodyindex])))
colnames(woody) <- c("id", "AnalyteName", "result")
woody$result <- as.numeric(as.character(woody$result))
head(woody)
nonwoodyindex <- which(ripveg$AnalyteName == "Riparian GroundCover NonWoody Plants")
nonwoody <- data.frame(cbind(ripveg$id[nonwoodyindex], as.character(ripveg$AnalyteName[nonwoodyindex]),
as.character(ripveg$VariableResult[nonwoodyindex])))
colnames(nonwoody) <- c("id", "AnalyteName", "result")
nonwoody$result <- as.numeric(as.character(nonwoody$result))
head(nonwoody)
woody$XPGVEG<-apply((cbind(woody$result, nonwoody$result)), 1, sum, na.rm=T)
XPGVEG_subcounting<-(data.frame(!(is.na(woody$result)&(is.na(nonwoody$result))), as.character(woody$id)))
XPGVEG_total <-tapply(XPGVEG_subcounting[[1]], (XPGVEG_subcounting[[2]]), sum)

woody$XPGVEG[which(woody$XPGVEG == NA)] = 0
XPGVEG_subcountf <- function(data){
	length(which(data != 0))
}
XPGVEG_subcount <- tapply(woody$XPGVEG, woody$id, XPGVEG_subcountf)
XPGVEG.result <- XPGVEG_subcount/XPGVEG_total

###XPCM###
aframe <- as.data.frame(cast(ripveg, id + LocationCode ~ AnalyteName, value = "VariableResult",fun.aggregate=NULL))

for(i in 3:7){
  aframe[[i]] <- as.numeric(as.character(aframe[[i]]))
}
aframe[is.na(aframe)] <- (-1)
aframe[aframe == "Not Recorded"] <- (-1)
aframe$XPCM <- rep(NA, length(aframe$id))
for(i in which(!is.na(aframe[[3]]))){
  aframe$XPCM[i] <- if((aframe$"Riparian Upper Canopy All Trees"[i]>0)&(
    aframe$"Riparian Lower Canopy All Vegetation"[i]>0)){T} else
      if((aframe$"Riparian Upper Canopy All Trees"[i]==0)|(
        aframe$"Riparian Lower Canopy All Vegetation"[i]==0)){F}else{NA}
}
aframe$"Riparian Upper Canopy All Trees"[which(aframe$"Riparian Upper Canopy All Trees" == -1)] <-NA
XPCM.result <- tapply(aframe$XPCM, aframe$id, sumna)/tapply(aframe$XPCM, aframe$id, lengthna)

###XPCMG###
aframe$"Riparian Upper Canopy All Trees"[is.na(aframe$"Riparian Upper Canopy All Trees")]<- (-1)
aframe$XPCMG <- rep(NA, length(aframe$id))
for(i in which(!is.na(aframe[[3]]))){
  aframe$XPCMG[i] <- if(((aframe$"Riparian Upper Canopy All Trees"[i]>0)&(
    aframe$"Riparian Lower Canopy All Vegetation"[i]>0))&(
      (aframe$"Riparian GroundCover Woody Shrubs"[i] >0) |
        (aframe$"Riparian GroundCover NonWoody Plants"[i] >0))){T} else
          if(((aframe$"Riparian Upper Canopy All Trees"[i]==0)|(
            aframe$"Riparian Lower Canopy All Vegetation"[i]==0))|(
              (aframe$"Riparian GroundCover Woody Shrubs"[i] ==0) |
                (aframe$"Riparian GroundCover NonWoody Plants"[i] ==0))){F}else{NA}
}
XPCMG.result <- tapply(aframe$XPCMG, aframe$id, sumna)/tapply(aframe$XPCMG, aframe$id, lengthna)

###XPMGVEG###

aframe$XPMGVEG <- rep(NA, length(aframe$id))
for(i in which(!is.na(aframe[[3]]))){
  aframe$XPMGVEG[i] <- if((aframe$"Riparian GroundCover Woody Shrubs"[i] >1) |
    (aframe$"Riparian GroundCover NonWoody Plants"[i] >1)){T} else
      if((aframe$"Riparian GroundCover Woody Shrubs"[i] ==1) |
        (aframe$"Riparian GroundCover NonWoody Plants"[i] ==1)){F}else
          if((aframe$"Riparian GroundCover Woody Shrubs"[i] ==0) |
            (aframe$"Riparian GroundCover NonWoody Plants"[i] ==0)){F} else{NA}
}
XPMGVEG.result <- tapply(aframe$XPMGVEG, aframe$id, sumna)/tapply(aframe$XPMGVEG, aframe$id, lengthna)


###Write to file###
results <- cbind(XGB, XGH, XGW, XM, XC, XG.result, XCM.result, XCMG.result, 
                 XPMID.result, XPCAN.result, XPGVEG.result, XPCM.result, XPCMG.result, XPMGVEG.result)
fc <- file("riparian_vegetation_metrics.csv", open="w")
write.csv(results, fc)
close(fc)
print("Riparian Vegetation done")