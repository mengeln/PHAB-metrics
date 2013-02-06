library(plyr)
library(reshape2)
library(randomForest)
library(ggplot2)
library(caret)
library(doParallel)
load("../FamilyIndex/data/station.rdata")

phabdata <- read.csv("Data/phab.csv")
test <- merge(phabdata, stations, by.x="DelineationCode", by.y="StationCode")


###t stats for raw metrics###
tstat <- function (data, test) {
  stat <- lapply(as.list(data), function(x){
    ref <- as.numeric(x[test$SiteSet == "RefCal"])
    stress <- as.numeric(x[test$SiteSet == "StressCal"])
    try(t.test(ref, stress)$statistic)
  })
  stat <- Filter(is.numeric, stat)
  tstat <- Reduce(rbind, stat)
  tstat <- data.frame("metric" = names(stat), t_stat = abs(tstat[, 1]), row.names=NULL)
  tstat <- tstat[order(tstat$t_stat, decreasing=TRUE), ]
  tstat
}

View(tstat(test[, 3:165], test))

###residual vs. raw##########

modeldata <- test[test$SiteSet == "RefCal",
                  c("DelineationCode", names(stations)[11:27], "SiteSet",
                      names(phabdata)[3:ncol(phabdata)])]

phabRFmods <- lapply(
  lapply(as.list(modeldata[, names(phabdata)[3:ncol(phabdata)]]),
         function(x)cbind(x, modeldata[, names(stations)[11:27]])),
  function(metric){
    metric <- na.omit(metric)
    try(randomForest(x = metric[, 2:ncol(metric)], y = metric$x, ntrees=1000, mtry=2))
  })
phabRFmods <- Filter(function(x)class(x) == "randomForest", phabRFmods)
result <- sapply(names(phabRFmods), function(name){
  rsq <- phabRFmods[[name]]$rsq[length(phabRFmods[[name]]$rsq)]
  if(is.na(rsq)) test[, name] else                       
  if(rsq > .1)
    abs(predict(phabRFmods[[name]], test[, names(stations)[11:27]]) - test[, name]) else
      test[, name]
})

###Metric Selection######################

corMatrix <- cor(as.matrix(result), use = "pairwise.complete.obs")^2

resid_tstat <- tstat(as.data.frame(result), test)


metricSelect <- function(x, select){
  
  if(nrow(x) == 0)return(select)
  
  
  x <- x[x$t_stat > 5,]
  x <- x[order(x$t_stat, decreasing = TRUE),]
  
  if(length(select) != 0){
    if(any(na.omit(corMatrix[select, as.character(x$metric[1])]) >= 0.5))return(metricSelect(tail(x, -1), select))
  }
  
  select[length(select) + 1] <- as.character(x$metric[1])

  return(metricSelect(x, select))
}

metrics <- metricSelect(resid_tstat, character(0))

###investigate candidates##################
candidates <- c("CFC_RCK", "PCT_FAST", "SB_PP_D90", "XCDENMID", "XGW",
                "CFC_AQM", "PBM_S")

candidate_data <- result[, candidates]
names(candidate_data) <- sapply(candidates, function(x){
  rsq <- phabRFmods[[x]]$rsq[length(phabRFmods[[x]]$rsq)]
  if(rsq > .1) paste0(x, "_residual") else x
})
candidate_data <- cbind(candidate_data, test[, c(names(stations)[11:27], "SiteSet")])

candidate_data.melt <- melt(candidate_data, id.vars=c(names(stations)[11:27], "SiteSet"))

ggplot(candidate_data.melt, aes(SiteSet, value)) + geom_boxplot() +
  facet_wrap(~variable, scales="free")


####Refine Models##############
cl <- makeCluster(8)
registerDoParallel(cl)

refined_models <- lapply(candidates, function(metric){
  data <- test[which(test$SiteSet == "RefCal"), c(names(stations)[c(11:27, 63, 65:66)], metric)]
  rfe(x=data[, 1:17], y=data[, 18],sizes = 1:17,
      rfeControl = rfeControl(functions = rfFuncs))
})
stopCluster(cl)
names(refined_models) <- candidates


refined_results <- as.data.frame(sapply(1:7, function(i){
  test[, candidates[1]] - predict(refined_models[[i]], test[, names(stations)[11:27]])
}))
names(refined_results) <- candidates
refined_results$SiteSet <- test$SiteSet
refined_results.melt <- melt(refined_results, id.vars="SiteSet")
ggplot(refined_results.melt, aes(SiteSet, value)) + geom_boxplot() + facet_wrap(~variable, scales="free")


