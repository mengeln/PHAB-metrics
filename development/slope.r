

slope <- function(x){
  
  data <- subset(x, AnalyteName %in% c("Proportion", "Elevation Difference", "Length, Segment", "Slope", "Bearing"))
  
  slope <- dcast(data, SampleID + LocationCode ~ AnalyteName, value.var="Result", mean, na.rm=TRUE)
  slope$Location2 <- slope$LocationCode
  if(is.null(slope$Slope))slope$Slope <- rep(NA, nrow(slope))
  slope$Slope[is.na(slope$Slope)] <- (slope$"Elevation Difference"/100 * slope$"Length, Segment")[is.na(slope$Slope)]
  slope$product <- slope$Slope * slope$Proportion/100
  slope$xbearing <- slope$Bearing * slope$Proportion/100
  
  metricCalc(NULL)(slope, c(XSLOPE = function(x)mean(x$product),
                            SLOPE_0 = function(x)mean(x$Slope == 0),
                            SLOPE_0_5 = function(x)mean(x$Slope <= 0.5),
                            SLOPE_01 = function(x)mean(x$Slope <= 1),
                            SLOPE_02 = function(x)mean(x$Slope <= 2),
                            XBEARING = function(x)mean(x$xbearing)))
}