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
  
  metrics <- c("XFC_AQM" = "mean(d$'Fish Cover Macrophytes')",
               "XFC_HUM" = "mean(d$'Fish Cover Artificial Structures')",
               "XFC_RCK" = "mean(d$'Fish Cover Boulders')",
               "XFC_ALG" = "mean(d$'Fish Cover Filamentous Algae')",
               "XFC_LWD" = "mean(d$'Fish Cover Woody Debris >0.3 m')",
               "XFC_LTR" = "mean(d$'Fish Cover Live Trees/Roots')",
               "XFC_OHV" = "mean(d$'Fish Cover Overhang.Veg')",
               "XFC_BRS" = "mean(d$'Fish Cover Woody Debris <0.3 m')",
               "XFC_UCB" = "mean(d$'Fish Cover Undercut Banks')",
               "XFC_BIG" = "sum(mean(d$'Fish Cover Woody Debris >0.3 m'),
               mean(d$'Fish Cover Boulders'),
               mean(d$'Fish Cover Undercut Banks'),
               mean(d$'Fish Cover Artificial Structures'))",
               "XFC_NAT_EMAP" = "sum(mean(d$'Fish Cover Woody Debris >0.3 m'),
               mean(d$'Fish Cover Boulders'),
               mean(d$'Fish Cover Undercut Banks'),
               mean(d$'Fish Cover Woody Debris <0.3 m'),
               mean(d$'Fish Cover Overhang.Veg'))",
               "XFC_NAT_SWAMP" = "sum(mean(d$'Fish Cover Woody Debris >0.3 m'),
               mean(d$'Fish Cover Boulders'),
               mean(d$'Fish Cover Undercut Banks'),
               mean(d$'Fish Cover Woody Debris <0.3 m'),
               mean(d$'Fish Cover Overhang.Veg'),
               mean(d$'Fish Cover Live Trees/Roots'),
               mean(d$'Fish Cover Macrophytes'))",
               "CFC_AQM" = "sum(d$'Fish Cover Macrophytes' > 0)",
               "CFC_HUM" = "sum(d$'Fish Cover Artificial Structures' > 0)",
               "CFC_RCK" = "sum(d$'Fish Cover Boulders' > 0)",
               "CFC_ALG" = "sum(d$'Fish Cover Filamentous Algae' > 0)",
               "CFC_LWD" = "sum(d$'Fish Cover Woody Debris >0.3 m' > 0)",
               "CFC_LTR" = "sum(d$'Fish Cover Live Trees/Roots' > 0)",
               "CFC_OHV" = "sum(d$'Fish Cover Overhang.Veg' > 0)",
               "CFC_BRS" = "sum(d$'Fish Cover Woody Debris <0.3 m' > 0)",
               "CFC_UCB" = "sum(d$'Fish Cover Undercut Banks' > 0)",
               "CFC_BIG" = "sum(sum(d$'Fish Cover Woody Debris >0.3 m' > 0),
               sum(d$'Fish Cover Boulders' > 0),
               sum(d$'Fish Cover Undercut Banks' > 0),
               sum(d$'Fish Cover Artificial Structures' > 0))",
               "CFC_NAT_EMAP" = "sum(c(sum(d$'Fish Cover Woody Debris >0.3 m' > 0),
               sum(d$'Fish Cover Boulders' > 0),
               sum(d$'Fish Cover Undercut Banks' > 0),
               sum(d$'Fish Cover Woody Debris <0.3 m' > 0),
               sum(d$'Fish Cover Overhang.Veg' > 0)) > 0)",
               "CFC_NAT_SWAMP" = "sum(c(sum(d$'Fish Cover Woody Debris >0.3 m' > 0),
               sum(d$'Fish Cover Boulders' > 0),
               sum(d$'Fish Cover Undercut Banks' > 0),
               sum(d$'Fish Cover Woody Debris <0.3 m' > 0),
               sum(d$'Fish Cover Overhang.Veg' > 0),
               sum(d$'Fish Cover Live Trees/Roots' > 0),
               sum(d$'Fish Cover Macrophytes' > 0)) > 0)")
  
  metricCalc(NULL)(habitat, metrics)
}