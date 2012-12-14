library(reshape2)

humanDisturbance <- function(x){
  data <- subset(x, AnalyteName %in% c("Riparian Bridges/Abutments",
                                          "Riparian Buildings",
                                          "Riparian Landfill/Trash",
                                          "Riparian Logging",
                                          "Riparian Mining",
                                          "Riparian Orchards/Vineyards",
                                          "Riparian Park/Lawn",
                                          "Riparian Pasture/Range",
                                          "Riparian Pavement",
                                          "Riparian Pipes",
                                          "Riparian Road",
                                          "Riparian Row Crops",
                                          "Riparian Vegetation Management",
                                          "Riparian Wall/Dike"
                                          ) &
                   ResQualCode == "=")
  data$Location1 <- sapply(strsplit(as.character(data$LocationCode), ", "), function(x)x[2])
  data$Location1[is.na(data$Location1)] <- "Channel"
  loc1 <- sapply(strsplit(as.character(data$LocationCode), ","), function(x)x[1])
  data$Location2 <- substr(loc1, nchar(loc1), nchar(loc1))
  hdist <- dcast(data, SampleID + Location2 + AnalyteName ~ Location1, value.var="VariableResult")
  hdist$Channel[is.na(hdist$Channel)] <- "N"
  
  convert <- function(x, y){
    ifelse(y == "Y", 1.5, ifelse(
      x == "B", 1.5, ifelse(
        x == "C", 1, ifelse(
          x == "P", 0.667, 0))))
  }
  hdist$ResultLeft <- mapply(convert, hdist$Left, hdist$Channel)
  hdist$ResultRight <- mapply(convert, hdist$Right, hdist$Channel)

  hdistm <- function(dat, x){
    sum(dat[dat$AnalyteName %in% x, 'ResultRight'], dat[dat$AnalyteName %in% x, 'ResultLeft'])
  }
  

  metrics <- c("W1H_BRDG" = "hdistm(d, 'Riparian Bridges/Abutments')",
               "W1H_BLDG" = "hdistm(d, 'Riparian Buildings')",
               "W1H_LDFL" = "hdistm(d, 'Riparian Landfill/Trash')",
               "W1H_LOG" = "hdistm(d, 'Riparian Logging')",
               "W1H_MINE" = "hdistm(d, 'Riparian Mining')",
               "W1H_ORVY" = "hdistm(d, 'Riparian Orchards/Vineyards')",
               "W1H_PARK" = "hdistm(d, 'Riparian Park/Lawn')",
               "W1H_PSTR" = "hdistm(d, 'Riparian Pasture/Range')",
               "W1H_PVMT" = "hdistm(d, 'Riparian Pavement')",
               "W1H_PIPE" = "hdistm(d, 'Riparian Pipes')",
               "W1H_ROAD" = "hdistm(d, 'Riparian Road')",
               "W1H_CROP" =  "hdistm(d, 'Riparian Row Crops')",
               "W1H_VEGM" = "hdistm(d, 'Riparian Vegetation Management')",
               "W1H_WALL" = "hdistm(d, 'Riparian Wall/Dike')",
               "W1_HALL_SWAMP" = "hdistm(d, c('Riparian Bridges/Abutments', 'Riparian Buildings', 'Riparian Landfill/Trash',
               'Riparian Logging', 'Riparian Mining', 'Riparian Orchards/Vineyards', 'Riparian Park/Lawn', 'Riparian Pasture/Range',
               'Riparian Pavement', 'Riparian Pipes', 'Riparian Road', 'Riparian Row Crops', 'Riparian Vegetation Management', 
               'Riparian Wall/Dike'))"
               )
  hdistMetrics <- metricCalc(NULL)
  result <- hdistMetrics(hdist, metrics)
  count <- tapply(hdist$Location2, hdist$SampleID, function(x)length(unique(x)))
  result$count <- rep(count, each=length(metrics))
  result
}






