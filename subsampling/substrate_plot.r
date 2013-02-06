# library(RODBC)
library(reshape2)
library(ggplot2)
library(plyr)
# source("development/metricCalc.r")
# source("subsampling/substrate_subsample.r")
# 
# connection <- odbcConnectAccess2007("P:/PartTimers/MarkEngeln/SWAMP_RM_112012.mdb")
# 
# substrate_data <- sqlQuery(connection, "SELECT * FROM Query3_test 
#                        WHERE AnalyteName IN ('Substrate Size Class', 'Embeddedness', 'CPOM')")
# substrate_data$SampleID <- with(substrate_data, paste0(StationCode, SampleDate, ProjectCode))
# 
# odbcClose(connection)
# 
# substratum_set1 <- substrate2(substrate_data[substrate_data$SampleID %in% unique(substrate_data$SampleID)[1:1000], ])
# gc()
# substatrum_set3 <- substrate2(substrate_data[substrate_data$SampleID %in% unique(substrate_data$SampleID)[1001:1500], ])
# gc()
# substatrum_set2 <- substrate2(substrate_data[substrate_data$SampleID %in% unique(substrate_data$SampleID)[1501:1959], ])
# 
# substrate_result <- mapply(rbind, substratum_set1, substatrum_set3, substatrum_set2, SIMPLIFY = FALSE)

#load("data/substrate_iterations.rdata")

substrate_map <- function(data, x){
  test2 <- Reduce(cbind, data)
  names(test2) <- c(sapply(1:21, function(x)paste0(c("SampleID", "metric", "mean", "sd", "count"), "_", x)))
  test3 <- na.omit(data.frame(SampleID = test2$SampleID_1, metric = test2$metric_1, 
                              colwise(function(x)x/test2$mean_21)(test2[, grep("mean", names(test2))])))
  test4 <- melt(test3)
  
  transplot <- function(met){
    map <- subset(test4, metric == met)
    
    map$trans <- as.numeric(sapply(strsplit(as.character(map$variable), "_"), function(l)l[2]))
    x <- ddply(map, .(trans), function(df){
      sum(df$value > (1 - x) & df$value < (1 + x))/nrow(df)
    })
    names(x) <- c("trans", met)
    x
  }
  
  res <- Reduce(function(x, y)merge(x, y, by="trans"), lapply(as.character(unique(test4$metric)), function(x)transplot(x)))
  res2 <- melt(res, id.var="trans")
  res2$trans <- as.factor(res2$trans) 
  
  ggplot(res2, aes(trans, value, group=variable, colour=variable)) + geom_line() + scale_x_discrete("Number of Transects") +
    scale_y_continuous("Percent of Samples within +/- 10% of their 'True Value'") + geom_hline(yintercept=0.95)
}


#substrate_map(substrate_result, 0.1)