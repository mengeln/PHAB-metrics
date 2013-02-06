library(RODBC)
library(reshape2)
library(ggplot2)
library(plyr)
source("development/metricCalc.r")
source("subsampling/algae_subsample.r")

connection <- odbcConnectAccess2007("P:/PartTimers/MarkEngeln/SWAMP_RM_112012.mdb")

algae_data <- sqlQuery(connection, "SELECT * FROM Query3_test 
                       WHERE AnalyteName IN ('Microalgae Thickness', 'Macrophyte Cover',
                       'Macroalgae Cover, Unattached', 'Macroalgae Cover, Attached')")
algae_data$SampleID <- with(algae_data, paste0(StationCode, SampleDate, ProjectCode))

odbcClose(connection)

test <- algae(algae_data)

test2 <- Reduce(cbind, test)
names(test2) <- c(sapply(1:21, function(x)paste0(c("SampleID", "metric", "mean", "sd", "count"), "_", x)))
test3 <- na.omit(data.frame(SampleID = test2$SampleID_1, metric = test2$metric_1, 
                    colwise(function(x)x/test2$mean_21)(test2[, grep("mean", names(test2))])))
test4 <- melt(test3)

transplot <- function(met){
  map <- subset(test4, metric == met)

  map$trans <- as.numeric(sapply(strsplit(as.character(map$variable), "_"), function(l)l[2]))
  x <- ddply(map, .(trans), function(df){
    sum(df$value > 0.5 & df$value < 1.5)/nrow(df)
  })
  names(x) <- c("trans", met)
  x
}
  
res <- Reduce(function(x, y)merge(x, y, by="trans"), lapply(as.character(unique(test4$metric)), function(x)transplot(x)))
res2 <- melt(res, id.var="trans")
res2$trans <- as.factor(res2$trans) 

ggplot(res2, aes(trans, value, group=variable, colour=variable)) + geom_line() + scale_x_discrete("Number of Transects") +
  scale_y_continuous("Percent of Samples within +/- 10% of their 'True Value'") + geom_hline(yintercept=0.95) +
  scale_color_brewer(palette="Paired")

