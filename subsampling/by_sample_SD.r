test2 <- Reduce(cbind, test)

names(test2) <- c(sapply(1:21, function(x)paste0(c("SampleID", "metric", "mean", "sd", "count"), "_", x)))
test2 <- merge(test2, ddply(test2, .(metric_1), function(df)mean(df$sd_21, na.rm=TRUE)))
test2$upper <- test2$mean_21 + test2$V1
test2$upper[is.na(test2$upper)] <- 0
test2$lower <- test2$mean_21 - test2$V1
test2$lower[is.na(test2$lower)] <- 0
test3 <- na.omit(data.frame(SampleID = test2$SampleID_1, metric = test2$metric_1, 
                            #colwise(function(x)x > test2$upper & x < test2$lower )(test2[, grep("mean", names(test2))])
                            sapply(grep("mean", names(test2)), 
                                   function(col)test2[, col] >= test2$lower & test2[, col] <= test2$upper)))
test4 <- melt(test3, id.vars=c("SampleID", "metric"))
test4 <- ddply(test4, .(metric, variable), function(df)mean(df$value))

ggplot(test4, aes(variable, V1, group=metric, colour=metric)) + geom_line() + scale_x_discrete("Number of Transects") +
  scale_y_continuous("Percent of Samples within 1 SD of their 'True Value'") + geom_hline(yintercept=0.95) +
  scale_color_brewer(palette="Paired")
