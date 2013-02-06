library(reshape2)

bankMorphology <- function(x){
  
  data <- subset(x, AnalyteName %in% c("Bank Stability", "Bankfull Height", "Bankfull Width") &
                   ResQualCode == "=")
  data$Location2 <- sapply(strsplit(as.character(data$LocationCode), ","), function(x)x[1])
  stability <- subset(data, AnalyteName == "Bank Stability", c(SampleID, Location2, VariableResult))  
  
  metrics <- c(PBM_S = function(x)mean(x$VariableResult == 'stable'),
               PBM_V = function(x)mean(x$VariableResult == 'vulnerable'),
               PBM_E = function(x)mean(x$VariableResult == 'eroded'))
  
  stability_result <- metricCalc(NULL)(stability, metrics)
  
  bankfull <- dcast(subset(data, AnalyteName %in% c("Bankfull Height", "Bankfull Width")),
                    SampleID + Location2 ~ AnalyteName, value.var="Result")
  
  bankfull_result <- metricCalc(NULL)(bankfull, c(XBKF_H = function(x)mean(x$'Bankfull Height', na.rm=TRUE),
                                                  XBKF_W = function(x)mean(x$'Bankfull Width', na.rm=TRUE)))
  
  rbind(stability_result, bankfull_result)
}