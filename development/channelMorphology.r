library(plyr)

channelMorphology <- function(datum){
  data <- subset(datum, AnalyteName %in% c("Cascade/Falls",
                                          "Dry",
                                          "Glide",
                                          "Pool",
                                          "Rapid", 
                                          "Riffle",
                                          "Run" 
                                         ) &
                   ResQualCode == "=")
  data$Location2 <- as.character(data$LocationCode)

  metrics <- c(PCT_CF = function(x)sum(x$Result[x$AnalyteName == 'Cascade/Falls']),
               PCT_DR = function(x)sum(x$Result[x$AnalyteName == 'Dry']),
               PCT_GL = function(x)x$Result[x$AnalyteName == 'Glide'],
               PCT_POOL = function(x)sum(x$Result[x$AnalyteName == 'Pool']),
               PCT_RA = function(x)sum(x$Result[x$AnalyteName == 'Rapid']),
               PCT_RI = function(x)sum(x$Result[x$AnalyteName == 'Riffle']),
               PCT_RN = function(x)sum(x$Result[x$AnalyteName == 'Riffle']),
               PCT_FAST = function(x)sum(x$Result[x$AnalyteName %in% c('Cascade/Falls', 'Rapid', 'Riffle', 'Riffle')]),
               PCT_SLOW = function(x)sum(x$Result[x$AnalyteName %in% c('Pool', 'Glide')]),
               PCT_CF_WT = function(x)sum(x$Result[x$AnalyteName == 'Cascade/Falls']) * x$wt[1],
               PCT_DR_WT = function(x)sum(x$Result[x$AnalyteName == 'Dry']) * x$wt[1],
               PCT_GL_WT = function(x)sum(x$Result[x$AnalyteName == 'Glide']) * x$wt[1],
               PCT_POOL_WT = function(x)sum(x$Result[x$AnalyteName == 'Pool']) * x$wt[1],
               PCT_RA_WT = function(x)sum(x$Result[x$AnalyteName == 'Rapid']) * x$wt[1],
               PCT_RI_WT = function(x)sum(x$Result[x$AnalyteName == 'Riffle']) * x$wt[1],
               PCT_RN_WT = function(x)sum(x$Result[x$AnalyteName == 'Riffle']) * x$wt[1],
               PCT_FAST_WT = function(x)sum(x$Result[x$AnalyteName %in% c('Cascade/Falls', 'Rapid', 'Riffle', 'Riffle')]) * x$wt[1],
               PCT_SLOW_WT = function(x)sum(x$Result[x$AnalyteName %in% c('Pool', 'Glide')])* x$wt[1]

  )
  channelMetrics <- metricCalc("d$wt <- sum(d$Result[d$AnalyteName %in% c('Cascade/Falls', 'Rapid', 'Riffle', 'Riffle', 'Glide', 'Pool')])/100")
  result <- channelMetrics(data, metrics)
  result$count <- rep(tapply(data$Location2, data$SampleID, length)/7, each=length(metrics))
  
  depth <- subset(datum, AnalyteName == "StationWaterDepth")
  depth$Location2 <- sapply(strsplit(as.character(depth$LocationCode), ","), function(x)x[1])
  depth_result <- metricCalc(NULL)(depth, c(XWDEPTH = function(x)sum(x$Result),
                                            XWDM = function(x)max(x$Result)))
  
  width <- subset(datum, AnalyteName == "Wetted Width" & LocationCode != "X")
  width$Location2 <- sapply(strsplit(as.character(width$LocationCode), ","), function(x)x[1])
  width_result <- metricCalc(NULL)(width, c(XWIDTH = function(x)sum(x$Result)))
  

  XWDR <- data.frame(cbind(unique(width$SampleID), rep("XWDR", length(width_result$mean)),
                           width_result$mean / depth_result$mean[depth_result$metric == "XWDEPTH"],
                           rep(NA, length(width_result$mean)), rep(NA, length(width_result$mean))))
  names(XWDR) <- c("SampleID", "metric", "mean", "sd", "count")
  XWDA <- data.frame(cbind(unique(width$SampleID), rep("XWDA", length(width_result$mean)),
                           width_result$mean * (depth_result$mean[depth_result$metric == "XWDEPTH"]/100),
                           rep(NA, length(width_result$mean)), rep(NA, length(width_result$mean))))
  names(XWDA) <- c("SampleID", "metric", "mean", "sd", "count")

  velocity <- subset(datum, AnalyteName == "Velocity" & LocationCode == "X")
  velocity_result <- ddply(velocity, "SampleID", function(df){
    data.frame("SampleID" = rep(unique(df$SampleID), 3),
               "metric" = c("XWV", "MXWV", "PWVZ"),
               "mean" = c(mean(df$Result), max(df$Result), sum(df$Result == 0)/nrow(df)),
               "sd" = c(sd(df$Result), NA, NA),
               "count" = rep(nrow(df), 3))
  })
  
  rbind(result, depth_result, width_result, XWDR, XWDA, velocity_result)
}
  
  
  
  
  