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

  metrics <- c("PCT_CF" = "sum(d$Result[d$AnalyteName == 'Cascade/Falls'])",
               "PCT_DR" = "sum(d$Result[d$AnalyteName == 'Dry'])",
               "PCT_GL" = "d$Result[d$AnalyteName == 'Glide']",
               "PCT_POOL" = "sum(d$Result[d$AnalyteName == 'Pool'])",
               "PCT_RA" = "sum(d$Result[d$AnalyteName == 'Rapid'])",
               "PCT_RI" = "sum(d$Result[d$AnalyteName == 'Riffle'])",
               "PCT_RN" = "sum(d$Result[d$AnalyteName == 'Riffle'])",
               "PCT_FAST" = "sum(d$Result[d$AnalyteName %in% c('Cascade/Falls', 'Rapid', 'Riffle', 'Riffle')])",
               "PCT_SLOW" = "sum(d$Result[d$AnalyteName %in% c('Pool', 'Glide')])",
               "PCT_CF_WT" = "sum(d$Result[d$AnalyteName == 'Cascade/Falls']) * wt",
               "PCT_DR_WT" = "sum(d$Result[d$AnalyteName == 'Dry']) * wt",
               "PCT_GL_WT" = "sum(d$Result[d$AnalyteName == 'Glide']) * wt",
               "PCT_POOL_WT" = "sum(d$Result[d$AnalyteName == 'Pool']) * wt",
               "PCT_RA_WT" = "sum(d$Result[d$AnalyteName == 'Rapid']) * wt",
               "PCT_RI_WT" = "sum(d$Result[d$AnalyteName == 'Riffle']) * wt",
               "PCT_RN_WT" = "sum(d$Result[d$AnalyteName == 'Riffle']) * wt",
               "PCT_FAST_WT" = "sum(d$Result[d$AnalyteName %in% c('Cascade/Falls', 'Rapid', 'Riffle', 'Riffle')]) * wt",
               "PCT_SLOW_WT" = "sum(d$Result[d$AnalyteName %in% c('Pool', 'Glide')])* wt"

  )
  channelMetrics <- metricCalc("wt <- sum(d$Result[d$AnalyteName %in% c('Cascade/Falls', 'Rapid', 'Riffle', 'Riffle', 'Glide', 'Pool')])/100")
  result <- channelMetrics(data, metrics)
  result$count <- rep(tapply(data$Location2, data$SampleID, length)/7, each=length(metrics))
  
  depth <- subset(datum, AnalyteName == "StationWaterDepth")
  depth$Location2 <- sapply(strsplit(as.character(depth$LocationCode), ","), function(x)x[1])
  depth_result <- metricCalc(NULL)(depth, c("XWDEPTH" = "sum(d$Result)",
                                            "XWDM" = "max(d$Result)"))
  
  width <- subset(datum, AnalyteName == "Wetted Width" & LocationCode != "X")
  width$Location2 <- sapply(strsplit(as.character(width$LocationCode), ","), function(x)x[1])
  width_result <- metricCalc(NULL)(width, c("XWIDTH" = "sum(d$Result)"))
  

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
  
  
  
  
  