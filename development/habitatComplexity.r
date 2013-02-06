library(reshape2)
library(plyr)

habitatComplexity <- function(x){
  
  data <- subset(x, AnalyteName %in% AnalyteName[grep("Fish", x$AnalyteName)] & ResQualCode == '=')
  
  data$Result2 <- with(data, ifelse(VariableResult == 0, 0, ifelse(
    VariableResult == 1, 5, ifelse(
      VariableResult == 2, 25, ifelse(
        VariableResult == 3, 57.5, ifelse(
          VariableResult == 4, 87.5, NA))))))
  data$Location2 <- data$LocationCode
  
  habitat <- dcast(data, SampleID + Location2 ~ AnalyteName, value.var="Result2")
  
  metrics <- c(XFC_AQM = function(x)mean(x$'Fish Cover Macrophytes'),
               XFC_HUM = function(x)mean(x$'Fish Cover Artificial Structures'),
               XFC_RCK = function(x)mean(x$'Fish Cover Boulders'),
               XFC_ALG = function(x)mean(x$'Fish Cover Filamentous Algae'),
               XFC_LWD = function(x)mean(x$'Fish Cover Woody Debris >0.3 m'),
               XFC_LTR = function(x)mean(x$'Fish Cover Live Trees/Roots'),
               XFC_OHV = function(x)mean(x$'Fish Cover Overhang.Veg'),
               XFC_BRS = function(x)mean(x$'Fish Cover Woody Debris <0.3 m'),
               XFC_UCB = function(x)mean(x$'Fish Cover Undercut Banks'),
               XFC_BIG = function(x)sum(mean(x$'Fish Cover Woody Debris >0.3 m'),
               mean(x$'Fish Cover Boulders'),
               mean(x$'Fish Cover Undercut Banks'),
               mean(x$'Fish Cover Artificial Structures')),
               XFC_NAT_EMAP = function(x)sum(mean(x$'Fish Cover Woody Debris >0.3 m'),
               mean(x$'Fish Cover Boulders'),
               mean(x$'Fish Cover Undercut Banks'),
               mean(x$'Fish Cover Woody Debris <0.3 m'),
               mean(x$'Fish Cover Overhang.Veg')),
               XFC_NAT_SWAMP = function(x)sum(mean(x$'Fish Cover Woody Debris >0.3 m'),
               mean(x$'Fish Cover Boulders'),
               mean(x$'Fish Cover Undercut Banks'),
               mean(x$'Fish Cover Woody Debris <0.3 m'),
               mean(x$'Fish Cover Overhang.Veg'),
               mean(x$'Fish Cover Live Trees/Roots'),
               mean(x$'Fish Cover Macrophytes')),
               CFC_AQM = function(x)sum(x$'Fish Cover Macrophytes' > 0),
               CFC_HUM = function(x)sum(x$'Fish Cover Artificial Structures' > 0),
               CFC_RCK = function(x)sum(x$'Fish Cover Boulders' > 0),
               CFC_ALG = function(x)sum(x$'Fish Cover Filamentous Algae' > 0),
               CFC_LWD = function(x)sum(x$'Fish Cover Woody Debris >0.3 m' > 0),
               CFC_LTR = function(x)sum(x$'Fish Cover Live Trees/Roots' > 0),
               CFC_OHV = function(x)sum(x$'Fish Cover Overhang.Veg' > 0),
               CFC_BRS = function(x)sum(x$'Fish Cover Woody Debris <0.3 m' > 0),
               CFC_UCB = function(x)sum(x$'Fish Cover Undercut Banks' > 0),
               CFC_BIG = function(x)sum(sum(x$'Fish Cover Woody Debris >0.3 m' > 0),
               sum(x$'Fish Cover Boulders' > 0),
               sum(x$'Fish Cover Undercut Banks' > 0),
               sum(x$'Fish Cover Artificial Structures' > 0)),
               CFC_NAT_EMAP = function(x)sum(c(sum(x$'Fish Cover Woody Debris >0.3 m' > 0),
               sum(x$'Fish Cover Boulders' > 0),
               sum(x$'Fish Cover Undercut Banks' > 0),
               sum(x$'Fish Cover Woody Debris <0.3 m' > 0),
               sum(x$'Fish Cover Overhang.Veg' > 0)) > 0),
               CFC_NAT_SWAMP = function(x)sum(c(sum(x$'Fish Cover Woody Debris >0.3 m' > 0),
               sum(x$'Fish Cover Boulders' > 0),
               sum(x$'Fish Cover Undercut Banks' > 0),
               sum(x$'Fish Cover Woody Debris <0.3 m' > 0),
               sum(x$'Fish Cover Overhang.Veg' > 0),
               sum(x$'Fish Cover Live Trees/Roots' > 0),
               sum(x$'Fish Cover Macrophytes' > 0)) > 0))
  
  metricCalc(NULL)(habitat, metrics)
}