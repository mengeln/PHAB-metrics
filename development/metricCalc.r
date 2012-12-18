library(plyr)

metricCalc <- function(helpers, outerhelpers=NULL){
  function(data, metrics){
    data.l <- lapply(split(data, data$SampleID), function(l){
      if(!is.null(outerhelpers))eval(parse(text=outerhelpers))
      sample <- lapply(split(l, l$Location2), function(d){
        sapply(metrics, function(code){
          if(!is.null(helpers))eval(parse(text=helpers))
          eval(parse(text=code))
        })
      })
      sample <- Reduce(rbind, sample)
      cbind(apply(sample, 2, mean, na.rm=T), apply(sample, 2, sd, na.rm=T), nrow(l))
    })
    result <- Reduce(rbind, data.l)
    rnames <- row.names(result)
    row.names(result) <- NULL
    result <- as.data.frame(result)
    result$SampleID <- rep(names(data.l), times=length(metrics))
    result$metric <- rnames
    names(result)[1:3] <- c("mean", "sd", "count")
    result <- result[,c("SampleID", "metric", "mean", "sd", "count")]
    arrange(result, SampleID)
  }
}