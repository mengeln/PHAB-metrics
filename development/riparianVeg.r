library(reshape2)

riparianVegetation <- function(datum){
  data <- subset(datum, AnalyteName %in% c("Riparian Upper Canopy All Trees", "Riparian Lower Canopy All Vegetation",
                                           "Riparian GroundCover NonWoody Plants", "Riparian GroundCover Barren",
                                           "Riparian GroundCover Woody Shrubs") 
                 & ResQualCode == "=")
  
  
  data$Result2 <- with(data, ifelse(VariableResult == 0, 0, ifelse(
    VariableResult == 1, 5, ifelse(
      VariableResult == 2, 25, ifelse(
        VariableResult == 3, 57.5, ifelse(
          VariableResult == 4, 87.5, NA))))))
  
  
  
  data <- dcast(data, SampleID + StationCode + SampleDate + LocationCode ~ AnalyteName, value.var="Result2")
  data$Location2 <- sapply(strsplit(as.character(data$LocationCode), ","), function(x)x[1])
  
  data$XPGVEG <- mapply(function(x,y)(x > 0)|(y > 0), data$"Riparian GroundCover NonWoody Plants", data$"Riparian GroundCover Woody Shrubs")
  
  data$XPCM <- mapply(function(x,y)(x > 0)&(y > 0), data$"Riparian Upper Canopy All Trees", data$"Riparian Lower Canopy All Vegetation")
  
  data$XPCMG <- mapply(function(x,y)(x > 0)&(y > 0), data$XPGVEG, data$XPCM)
  
  data$XPMGVEG <- mapply(function(x,y)(x > 5)|(y > 5), data$"Riparian GroundCover NonWoody Plants", data$"Riparian GroundCover Woody Shrubs")
  data
  metrics <- c("XGB" = 'mean(d$"Riparian GroundCover Barren")',
               "XGH" = 'mean(d$"Riparian GroundCover NonWoody Plants")',
               "XGW" = 'mean(d$"Riparian GroundCover Woody Shrubs")',
               "XM" = 'mean(d$"Riparian Lower Canopy All Vegetation")',
               "XC" = 'mean(d$"Riparian Upper Canopy All Trees")',
               "XG" = 'sum(mean(d$"Riparian GroundCover NonWoody Plants"), mean(d$"Riparian GroundCover Woody Shrubs"))',
               "XCM" = 'sum(mean(d$"Riparian Upper Canopy All Trees"), mean(d$"Riparian Lower Canopy All Vegetation"))',
               "XCMG" = 'sum(mean(d$"Riparian Upper Canopy All Trees"), mean(d$"Riparian Lower Canopy All Vegetation"), sum(mean(d$"Riparian GroundCover NonWoody Plants"), mean(d$"Riparian GroundCover Woody Shrubs")))',
               "XPMID" = 'mean(d$"Riparian Lower Canopy All Vegetation" != 0)',
               "XPCAN" = 'mean(d$"Riparian Upper Canopy All Trees" != 0)',
               "XPGVEG" = 'mean(d$XPGVEG)',
               "XPCM" = 'mean(d$XPCM)',
               "XPCMG" = 'mean(d$XPCMG)',
               "XPMGVEG" = 'mean(d$XPMGVEG)'
  )
  
  result <- metricCalc(NULL)(data, metrics)
  
  canopy <- subset(datum, AnalyteName == "Canopy Cover")
  canopy$Result2 <- canopy$Result * (100/17)
  canopy$Location2 <- sapply(strsplit(as.character(canopy$LocationCode), ","), function(x)x[1])
  canopy$Location3 <- sapply(strsplit(as.character(canopy$LocationCode), ","), function(x)x[2])
  canopy$MidLoc <- grepl("ctr", canopy$Location3) | grepl("Ctr", canopy$Location3)

  canopy <- dcast(canopy, SampleID + Location2 ~ MidLoc, value.var="Result2", fun.aggregate=mean, na.rm=TRUE)
  canopy_result <- metricCalc(NULL)(canopy, c("XCDENMID" = "mean(d$'TRUE', na.rm=TRUE)",
                                            "XCDENBK" = "mean(d$'FALSE', na.rm=TRUE)")
                                            )
  rbind(result, canopy_result)
}


