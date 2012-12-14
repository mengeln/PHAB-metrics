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

  metrics <- c("PCT_MAP" = "sum(d$macro_present)/sum(d$macro_count)", 
               "XMIAT "= "mean(d$micro, na.rm=T)",
               "XMIATP" = "sum(d$micro, na.rm=T)/sum(d$micro > 0, na.rm=T)",
               "PCT_MIATP" = "sum(d$micro > 0, na.rm=T)/sum(d$micro >= 0, na.rm=T)",
               "PCT_MIAT1" = "sum(d$micro >= 3, na.rm=T)/sum(d$micro >= 0, na.rm=T)",
               "PCT_MIAT1P" = "sum(d$micro >= 3, na.rm=T)/sum(d$micro > 0, na.rm=T)",
               "PCT_MAA" = "sum(d$'Macroalgae Cover, Attached' == 'Present')/sum(d$'Macroalgae Cover, Attached' %in% c('Present', 'Absent'))",
               "PCT_MCP" = "sum(d$'Macrophyte Cover' == 'Present')/sum(d$'Macrophyte Cover' %in% c('Present', 'Absent'))",
               "PCT_MAU" = "sum(d$'Macroalgae Cover, Unattached' == 'Present')/sum(d$'Macroalgae Cover, Unattached' %in% c('Present', 'Absent'))",
               "PCT_NSA" = "sum(d$nsa_present)/sum(d$nsa_count)"
  )

  algaeMetrics <- metricCalc(NULL)
  algaeMetrics(algae, metrics)
}