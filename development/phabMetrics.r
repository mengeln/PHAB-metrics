library(plyr)
library(reshape2)
library(parallel)

dir <- "P:/PartTimers/MarkEngeln/PHAB-metrics/development/"
lapply(list.files(dir)[!grepl("phabMetrics", list.files(dir))], function(x)source(paste0(dir, x)))

# test <- read.csv("Data/BaseData_PHAB.csv")
# test$SampleID <- with(test, paste(StationCode, SampleDate))

parPhabMetrics <- function(x){
  
  metrics <- c("algae", "bankMorphology", "channelMorphology", "habitatComplexity", "humanDisturbance",
               "riparianVegetation","substrate") #flow slope
  
  cl <- makeCluster(getOption("cl.cores", 2))
  clusterExport(cl, c(deparse(substitute(x)), "dcast", "ddply", "metricCalc", "helperfn", "helperfn.NULL", "helperfn.character",
                      "trans", "trans.numeric", "trans.matrix", metrics))
  result <- parLapply(cl, metrics, function(fn)cbind(eval(parse(text=fn))(x), "class" = fn))
  stopCluster(cl)  
  
  
  arrange(Reduce(rbind, result), SampleID, metric)
}

phabMetrics <- function(x){
  metrics <- c("algae", "bankMorphology", "channelMorphology", "habitatComplexity", "humanDisturbance",
               "riparianVegetation","substrate", "slope", "flow")
  
  result <- lapply(metrics, function(fn)cbind(tryCatch(
    {eval(parse(text=fn))(x)},
    error=function(x){}, "class" = fn)))
  
  arrange(Reduce(rbind, result), SampleID, metric)
}