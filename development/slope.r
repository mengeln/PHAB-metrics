

slope <- function(x){
  
  data <- subset(full.sub, AnalyteName %in% c("Proportion", "Elevation Difference", "Length, Segment", "Slope", "Bearing"))
  
  slope <- dcast(data, SampleID + LocationCode ~ AnalyteName, value.var="Result", mean, na.rm=TRUE)
  slope$Location2 <- slope$LocationCode
  slope$Slope[is.na(slope$Slope)] <- (slope$"Elevation Difference"/100 * slope$"Length, Segment")[is.na(slope$Slope)]
  slope$product <- slope$Slope * slope$Proportion/100
  slope$xbearing <- slope$Bearing * slope$Proportion/100
  
  metricCalc(NULL)(slope, c("XSLOPE" = "mean(d$product)",
                            "SLOPE_0" = "mean(d$Slope == 0)",
                            "SLOPE_0_5" = "mean(d$Slope <= 0.5)",
                            "SLOPE_01" = "mean(d$Slope <= 1)",
                            "SLOPE_02" = "mean(d$Slope <= 2)",
                            "XBEARING" = "mean(d$xbearing)"))
}