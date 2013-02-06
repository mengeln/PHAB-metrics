library(reshape2)
#source("../r functions/find_replace.r")

humanDisturbance2 <- function(x){
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
  
  data$VariableResult <- as.character(data$VariableResult)
  #data <- data[!duplicated(data[, c("SampleID", "Location2", "AnalyteName")]), ]
  View(data)
  hdist <- dcast(droplevels(data), SampleID + Location2 + AnalyteName ~ Location1, value.var="VariableResult",
                 function(x)print(x))

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
  
  
  metrics <- c(W1H_BRDG = function(x)hdistm(x, 'Riparian Bridges/Abutments'),
               W1H_BLDG = function(x)hdistm(x, 'Riparian Buildings'),
               W1H_LDFL = function(x)hdistm(x, 'Riparian Landfill/Trash'),
               W1H_LOG = function(x)hdistm(x, 'Riparian Logging'),
               W1H_MINE = function(x)hdistm(x, 'Riparian Mining'),
               W1H_ORVY = function(x)hdistm(x, 'Riparian Orchards/Vineyards'),
               W1H_PARK = function(x)hdistm(x, 'Riparian Park/Lawn'),
               W1H_PSTR = function(x)hdistm(x, 'Riparian Pasture/Range'),
               W1H_PVMT = function(x)hdistm(x, 'Riparian Pavement'),
               W1H_PIPE = function(x)hdistm(x, 'Riparian Pipes'),
               W1H_ROAD = function(x)hdistm(x, 'Riparian Road'),
               W1H_CROP =  function(x)hdistm(x, 'Riparian Row Crops'),
               W1H_VEGM = function(x)hdistm(x, 'Riparian Vegetation Management'),
               W1H_WALL = function(x)hdistm(x, 'Riparian Wall/Dike'),
               W1_HALL_SWAMP = function(x)hdistm(x, c('Riparian Bridges/Abutments', 'Riparian Buildings', 'Riparian Landfill/Trash',
                                                      'Riparian Logging', 'Riparian Mining', 'Riparian Orchards/Vineyards', 'Riparian Park/Lawn', 'Riparian Pasture/Range',
                                                      'Riparian Pavement', 'Riparian Pipes', 'Riparian Road', 'Riparian Row Crops', 'Riparian Vegetation Management', 
                                                      'Riparian Wall/Dike'))
  )
  lapply(1:21, function(i){
    hdistMetrics <- metricCalc2(NULL, NULL, i)
    result <- hdistMetrics(hdist, metrics)
    count <- tapply(hdist$Location2, hdist$SampleID, function(x)length(unique(x)))
    result$count <- rep(count, each=length(metrics))
    result
  })
}