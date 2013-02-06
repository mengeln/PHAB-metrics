library(reshape2)
library(plyr)

algae <- function(data){
  data <- subset(data, AnalyteName %in% c("Microalgae Thickness",
                                          "Macrophyte Cover",
                                          "Macroalgae Cover, Unattached",
                                          "Macroalgae Cover, Attached") &
                 ResQualCode == "=")
  algae <- dcast(data, SampleID + StationCode + SampleDate + LocationCode ~ AnalyteName, value.var="VariableResult")
  algae$Location2 <- sapply(strsplit(as.character(algae$LocationCode), ","), function(x)x[1])
  
  algae$macro_present <- mapply(function(x,y)(x=="Present")|(y=="Present"),
                             algae[, "Macroalgae Cover, Attached"],
                             algae[, "Macroalgae Cover, Unattached"])
  algae$macro_count <- mapply(function(x,y)!(x %in% c("Dry", "Not Recorded")) & !(y %in% c("Dry", "Not Recorded")),
                        algae[, "Macroalgae Cover, Attached"],
                        algae[, "Macroalgae Cover, Unattached"])


  algae$micro <- ifelse(algae$"Microalgae Thickness" == 0, 0, ifelse(
    algae$"Microalgae Thickness" == 1, 0.25, ifelse(
      algae$"Microalgae Thickness" == 2, 0.5, ifelse(
        algae$"Microalgae Thickness" == 3, 3, ifelse(
          algae$"Microalgae Thickness" == 4, 12.5, ifelse(
            algae$"Microalgae Thickness" == 5, 20, NA))))))
  
  algae$nsa_present <- mapply(function(x, y)x|(y >= 3),
                              algae$macro_present,
                              algae$micro)
  
  algae$nsa_count <- mapply(function(x,y) x & !(y %in% c("Dry", "Not Recorded")),
                            algae$macro_count,
                            algae$micro)

  metrics <- c(PCT_MAP = function(x)sum(x$macro_present)/sum(x$macro_count), 
               XMIAT = function(x)mean(x$micro, na.rm=T),
               XMIATP = function(x)sum(x$micro, na.rm=T)/sum(x$micro > 0, na.rm=T),
               PCT_MIATP = function(x)sum(x$micro > 0, na.rm=T)/sum(x$micro >= 0, na.rm=T),
               PCT_MIAT1 = function(x)sum(x$micro >= 3, na.rm=T)/sum(x$micro >= 0, na.rm=T),
               PCT_MIAT1P = function(x)sum(x$micro >= 3, na.rm=T)/sum(x$micro > 0, na.rm=T),
               PCT_MAA = function(x)sum(x$'Macroalgae Cover, Attached' == 'Present')/sum(x$'Macroalgae Cover, Attached' %in% c('Present', 'Absent')),
               PCT_MCP = function(x)sum(x$'Macrophyte Cover' == 'Present')/sum(x$'Macrophyte Cover' %in% c('Present', 'Absent')),
               PCT_MAU = function(x)sum(x$'Macroalgae Cover, Unattached' == 'Present')/sum(x$'Macroalgae Cover, Unattached' %in% c('Present', 'Absent')),
               PCT_NSA = function(x)sum(x$nsa_present)/sum(x$nsa_count)
  )

  algaeMetrics <- metricCalc(NULL)
  algaeMetrics(algae, metrics)
}