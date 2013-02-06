library(reshape2)
library(Hmisc)
library(randomForest)
library(foreach)
library(doParallel)
library(plyr)
library(ggplot2)
library(caret)

phabdata <- read.csv("data/MMI data/phab.data.csv")
stationsdata <- read.csv("data/MMI data/stations.csv")
xwalk <- read.csv("data/MMI data/phab.samples.csv")
count_threshold <- as.data.frame(daply(phabdata, "metric", function(df)
  quantile(df$count, probs=0.5, na.rm=TRUE)))
count_threshold$metric <- row.names(count_threshold)
names(count_threshold)[1] <- "Threshold"
phabdata <- merge(phabdata, count_threshold)
phabdata <- subset(phabdata, count >= Threshold)

phabdata.cast <- dcast(phabdata, StationCode + PhabCode + SampleID +
  SampleDate + SelectedSamples + SiteStatus + DevelopmentSet + 
  SiteSet ~ metric, value.var="mean", fun.aggregate=mean, na.rm=TRUE)

modeldata <- subset(phabdata.cast, DevelopmentSet == "Calibration" &
                    SiteStatus == "Reference" &
                    SelectedSamples == "Selected")
modeldata <- merge(modeldata, stationsdata)

predictors <- c("New_Lat",   "New_Long",  "ELEV_RANGE","BDH_AVE",   
                "PPT_00_09", "LPREM_mean","KFCT_AVE",  "TEMP_00_09","P_MEAN",    "N_MEAN",   
                "PRMH_AVE",  "AREA_SQKM", "SITE_ELEV", "MgO_Mean",  "S_Mean",   
                "SumAve_P",  "CaO_Mean",  "PCT_CENOZ", "PCT_QUART",
                "PCT_SEDIM")

####Initial Models
cl <- makeCluster(8)
registerDoParallel(cl)
models <- foreach(name = as.character(unique(phabdata$metric)),
                  .packages = "randomForest") %dopar%
                  {
                    data <- na.omit(modeldata[, c(predictors, name)])
                    randomForest(x = data[, 1:length(predictors)], 
                                 y = data[, 1 + length(predictors)],
                                 ntrees = 1000)
                    }
stopCluster(cl)
names(models) <- as.character(unique(phabdata$metric))
metric_summary <- data.frame("Metric" = as.character(unique(phabdata$metric)),
                             VarExp = sapply(models, function(l)tail(l$rsq, 1)))

metric_summary$Mean <- daply(phabdata, "metric", function(df){
  mean(df$mean, na.rm=TRUE)})[metric_summary$Metric]
metric_summary$SD <- daply(phabdata, "metric", function(df){
  mean(df$sd, na.rm=TRUE)})[metric_summary$Metric]
metric_summary$N <- daply(phabdata, "metric", function(df){
  mean(df$count, na.rm=TRUE)})[metric_summary$Metric]

tstat <- function(data, type, result){
  sapply(as.character(unique(phabdata$metric)), function(name){
    ref <- subset(data, SelectedSamples == "Selected" &
      DevelopmentSet == type & SiteStatus == "Reference", name)
    stress <- subset(data, SelectedSamples == "Selected" &
      DevelopmentSet == type & SiteStatus == "Stressed", name)
    x <- try(t.test(ref, stress))
    ifelse(class(x) != "htest", NA, abs(x[[result]]))
  })
}

metric_summary$tcal_raw <- tstat(phabdata.cast, "Calibration", "statistic")
metric_summary$tval_raw <- tstat(phabdata.cast, "Validation", "statistic")


fulldata <- merge(phabdata.cast, stationsdata)
fulldata <- fulldata[apply(fulldata[, predictors], 1, function(x)all(!is.na(x))), ]


residuals <- sapply(as.character(unique(phabdata$metric)), function(name){
  fulldata[, name] - predict(models[[name]], fulldata[, predictors])
})
residuals <- cbind(residuals, fulldata[, c("SelectedSamples", "SiteStatus",
                                           "DevelopmentSet")])

metric_summary$t_cal_df_model <- tstat(phabdata.cast, "Calibration", "parameter")
metric_summary$t_val_df_model <- tstat(phabdata.cast, "Validation", "parameter")
metric_summary$tcal_resid <- tstat(residuals, "Calibration", "statistic")
metric_summary$tval_resid <- tstat(residuals, "Validation", "statistic")

absresiduals <- cbind(numcolwise(abs)(residuals), fulldata[, c("SelectedSamples", "SiteStatus",
                                                               "DevelopmentSet")])

metric_summary$tcal_absresid <- tstat(absresiduals, "Calibration", "statistic")
metric_summary$tval_absresid <- tstat(absresiduals, "Validation", "statistic")

oovere <- sapply(as.character(unique(phabdata$metric)), function(name){
  fulldata[, name] / predict(models[[name]], fulldata[, predictors])
})
oovere <- cbind(oovere, fulldata[, c("SelectedSamples", "SiteStatus",
                                           "DevelopmentSet")])
metric_summary$tcal_OoverE <- tstat(oovere, "Calibration", "statistic")
metric_summary$tval_OoverE <- tstat(oovere, "Validation", "statistic")

metric_summary <- metric_summary[order(metric_summary$tcal_OoverE, decreasing=TRUE), ]
View(metric_summary[metric_summary$VarExp < 0.1, ])

candidates <- c("PCT_CB_resid", "CFC_ALG_absresid", "XCDENMID_absresid",
                "XCM_absresid", "PCT_FAST", "SB_PP_D90", "PBM_E_absresid")
names(residuals)[1:128] <- paste0(names(residuals)[1:128], "_resid")
names(absresiduals)[1:128] <- paste0(names(absresiduals)[1:128], "_absresid")
bind_data <- cbind(fulldata, residuals, absresiduals)
names(bind_data)[names(bind_data) == "SB_PP_D90.90%"] <- "SB_PP_D90"
bind_data$SB_PP_D90 <- log10(bind_data$SB_PP_D90)

bind_data.melt <- melt(bind_data[, c("SiteSet", candidates)], id.vars="SiteSet")
ggplot(bind_data.melt, aes(SiteSet, value)) + geom_boxplot() + 
  facet_wrap(~variable, scales="free")

mmfun <- function(quant, group){
  sapply(candidates, function(name){
    quantile(bind_data[bind_data$SiteSet %in% group, name], probs=quant,
             na.rm=TRUE) 
  })
}

minmax <- data.frame(best_d = mmfun(0.95, c("RefCal", "RefVal")),
                     worst_d = mmfun(0.05, c("StressCal", "StressVal")),
                     worst_i = mmfun(0.95, c("StressCal", "StressVal")),
                     best_i = mmfun(0.05, c("RefCal", "RefVal")))
row.names(minmax) <- substr(row.names(minmax), 1, 
                            nchar(row.names(minmax))-4)
increasers <- c("CFC_ALG_absresid", "XCDENMID_absresid",
                "XCM_absresid", "PBM_E_absresid")
decreasers <- c("PCT_CB_resid", "PCT_FAST", "SB_PP_D90")

increaser_score <- sapply(increasers, function(name){
  (bind_data[, name] - minmax[name, "worst_i"])/
    (minmax[name, "best_i"] - minmax[name, "worst_i"])
})
decreaser_score <- sapply(decreasers, function(name){
  (bind_data[, name] - minmax[name, "worst_d"])/
    (minmax[name, "best_d"] - minmax[name, "worst_d"])
})
vdata <- data.frame(increaser_score, decreaser_score, SiteSet=bind_data$SiteSet)
vdata$Total <- apply(vdata[, 1:7], 1, mean, na.rm=TRUE)
normalize <- mean(vdata$Total[vdata$SiteSet %in% c("RefCal", "RefVal")], na.rm=TRUE)
vdata$Score <- vdata$Total/normalize
vdata$Score[is.infinite(vdata$Score)] <- NA
tapply(vdata$Score, vdata$SiteSet, mean, na.rm=TRUE)
tapply(vdata$Score, vdata$SiteSet, sd, na.rm=TRUE)


####refined models####
raw_candidates <- c("PCT_CB", "CFC_ALG", "XCDENMID",
                    "XCM", "PCT_FAST", "SB_PP_D90",
                    "PBM_E")
cl <- makeCluster(8)
registerDoParallel(cl)
rfe_models <- lapply(raw_candidates, function(metric){
  data <- na.omit(bind_data[bind_data$SiteSet == "RefCal" & bind_data$SelectedSamples == "Selected",
                            c(predictors, metric)])
  rfe(x=data[, predictors], y=data[, metric], sizes = 1:17,
      rfeControl = rfeControl(functions = rfFuncs))
})
stopCluster(cl)
names(rfe_models) <- raw_candidates 
###Reassess#####

refined_residuals <- sapply(raw_candidates, function(metric){
  bind_data[, metric] - predict(rfe_models[[metric]], bind_data[, predictors])
})
colnames(refined_residuals) <- paste0(colnames(refined_residuals),
                                      "_resid")
refined_results <- within(data.frame(refined_residuals), {
  PCT_ALG_absresid <- abs(CFC_ALG_resid)
  CFC_ALG_absresid <- abs(CFC_ALG_resid)
  XCDENMID_absresid <- abs(XCDENMID_resid)
  XCM_absresid <- abs(XCM_resid)
  PBM_E_absresid <- abs(PBM_E_resid)
})
new_candidates <- c("PCT_CB_resid", "CFC_ALG_absresid", "XCDENMID_absresid",
                    "XCM_absresid", "PCT_FAST_resid", "SB_PP_D90_resid",
                    "PBM_E_absresid")
refined_results$SiteSet <- bind_data$SiteSet
refined_results.melt <- melt(refined_results[, c(new_candidates, "SiteSet")]
                             , id.vars="SiteSet")
ggplot(refined_results.melt, aes(SiteSet, value)) + geom_boxplot() +
  facet_wrap(~variable, scales="free")

mmfun <- function(quant, group){
  sapply(new_candidates, function(name){
    quantile(refined_results[bind_data$SiteSet %in% group, name], probs=quant,
             na.rm=TRUE) 
  })
}

minmax <- data.frame(best_d = mmfun(0.95, c("RefCal", "RefVal")),
                     worst_d = mmfun(0.05, c("StressCal", "StressVal")),
                     worst_i = mmfun(0.95, c("StressCal", "StressVal")),
                     best_i = mmfun(0.05, c("RefCal", "RefVal")))
row.names(minmax) <- substr(row.names(minmax), 1, 
                            nchar(row.names(minmax))-4)
increasers <- c("CFC_ALG_absresid", "XCDENMID_absresid",
                "XCM_absresid", "PBM_E_absresid")
decreasers <- c("PCT_CB_resid", "PCT_FAST_resid", "SB_PP_D90_resid")

increaser_score <- sapply(increasers, function(name){
  (refined_results[, name] - minmax[name, "worst_i"])/
    (minmax[name, "best_i"] - minmax[name, "worst_i"])
})
decreaser_score <- sapply(decreasers, function(name){
  (refined_results[, name] - minmax[name, "worst_d"])/
    (minmax[name, "best_d"] - minmax[name, "worst_d"])
})
results <- data.frame(increaser_score, decreaser_score, "SiteSet" = refined_results$SiteSet)
results$Total <- apply(results[, new_candidates], 1, mean)
results$Total[is.infinite(results$Total)] <- NA
results$Score <- results$Total / mean(results$Total[results$SiteSet %in% c("RefCal", "RefVal")], na.rm=TRUE)
results$SampleID <- bind_data$StationCode
tapply(results$Score, results$SiteSet, mean, na.rm=TRUE)
tapply(results$Score, results$SiteSet, sd, na.rm=TRUE)

round(table(results$SiteSet, bind_data$PSA9c)/rowSums(table(results$SiteSet, bind_data$PSA9c)), digits=2)
table(results$SiteSet[!is.na(results$Score)], bind_data$PSA9c[!is.na(results$Score)])
ggplot(results, aes(SiteSet, Score)) + geom_boxplot()

results$PSA9c <- bind_data$PSA9c
ggplot(results, aes(SiteSet, Score)) + geom_boxplot() + facet_wrap(~PSA9c)
