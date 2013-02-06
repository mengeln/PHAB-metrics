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
  metrics <- c("XGB" = function(x)mean(x$"Riparian GroundCover Barren"),
               "XGH" = function(x)mean(x$"Riparian GroundCover NonWoody Plants"),
               "XGW" = function(x)mean(x$"Riparian GroundCover Woody Shrubs"),
               "XM" = function(x)mean(x$"Riparian Lower Canopy All Vegetation"),
               "XC" = function(x)mean(x$"Riparian Upper Canopy All Trees"),
               "XG" = function(x)sum(mean(x$"Riparian GroundCover NonWoody Plants"), mean(x$"Riparian GroundCover Woody Shrubs")),
               "XCM" = function(x)sum(mean(x$"Riparian Upper Canopy All Trees"), mean(x$"Riparian Lower Canopy All Vegetation")),
               "XCMG" = function(x)sum(mean(x$"Riparian Upper Canopy All Trees"), mean(x$"Riparian Lower Canopy All Vegetation"), sum(mean(x$"Riparian GroundCover NonWoody Plants"), mean(x$"Riparian GroundCover Woody Shrubs"))),
               "XPMID" = function(x)mean(x$"Riparian Lower Canopy All Vegetation" != 0),
               "XPCAN" = function(x)mean(x$"Riparian Upper Canopy All Trees" != 0),
               "XPGVEG" = function(x)mean(x$XPGVEG),
               "XPCM" = function(x)mean(x$XPCM),
               "XPCMG" = function(x)mean(x$XPCMG),
               "XPMGVEG" = function(x)mean(x$XPMGVEG)
  )
  
  result <- metricCalc(NULL)(data, metrics)
  
  canopy <- subset(datum, AnalyteName == "Canopy Cover")
  canopy$Result2 <- canopy$Result * (100/17)
  canopy$Location2 <- sapply(strsplit(as.character(canopy$LocationCode), ","), function(x)x[1])
  canopy$Location3 <- sapply(strsplit(as.character(canopy$LocationCode), ","), function(x)x[2])
  canopy$MidLoc <- grepl("ctr", canopy$Location3) | grepl("Ctr", canopy$Location3)

  canopy <- dcast(canopy, SampleID + Location2 ~ MidLoc, value.var="Result2", fun.aggregate=mean, na.rm=TRUE)
  canopy_result <- metricCalc(NULL)(canopy, c("XCDENMID" = function(x)mean(x$'TRUE', na.rm=TRUE),
                                            "XCDENBK" = function(x)mean(x$'FALSE', na.rm=TRUE))
                                            )
  rbind(result, canopy_result)
}


