stationsplit <- split(full$result[full$analytename== analyte & !is.na(full$result) & (full$result > 0 )],
                      full$stationcode[full$analytename== analyte & !is.na(full$result) & (full$result > 0 )])

est.mean <- sapply(stationsplit, function(s){
  if(length(s) == 11){
    sapply(2:11, function(i){
      samp <- combn(s, i)
      apply(samp, 2, mean)
    })
  }})
est.mean <- est.mean[-which(sapply(est.mean, is.null))]


est.mean.df <- ldply(est.mean, function(l, i){
  u <- do.call(c, l)
  len <- rep(2:11, times=sapply(l, length))
  df <- data.frame(len, u)
  names(df) <- c("Transect", "Value")
  df
})

ggplot(est.mean.df , aes(Transect, Value, group=.id)) + geom_jitter(alpha=0.1) +
  labs(x="Number of Transects", y="Estimated mean", title="Bankful Height mean Estimates")

est.sd <- sapply(stationsplit, function(s){
  if(length(s) == 11){
    sapply(2:11, function(i){
      samp <- combn(s, i)
      apply(samp, 2, sd)/sd(s)
    })
  }})
est.sd <- est.sd[-which(sapply(est.sd, is.null))]


est.sd.df <- ldply(est.sd, function(l, i){
  u <- do.call(c, l)
  len <- rep(2:11, times=sapply(l, length))
  df <- data.frame(len, u)
  names(df) <- c("Transect", "Value")
  df
})
est.sd.df <- merge(est.sd.df, height.mean[, c("stationcode", "V1")], by.x=".id", by.y="stationcode")
est.sd.df$Transect <- as.factor(est.sd.df$Transect)
ggplot(est.sd.df , aes(Transect, Value, group=.id, colour=V1)) + #geom_jitter(alpha=0.1) +
  geom_line(alpha=0.2) + 
  labs(x="Number of Transects", y="Estimated SD", title="Bankful Height SD Estimates") +
  scale_color_continuous(low="Blue", high="Red")

est.cv <- sapply(stationsplit, function(s){
  if(length(s) == 11){
    sapply(2:11, function(i){
      samp <- combn(s, i)
      apply(samp, 2, sd)/apply(samp, 2, mean)
    })
  }})
est.cv <- est.cv[-which(sapply(est.cv, is.null))]


est.cv.df <- ldply(est.cv, function(l, i){
  u <- do.call(c, l)
  len <- rep(2:11, times=sapply(l, length))
  df <- data.frame(len, u)
  names(df) <- c("Transect", "Value")
  df
})
est.cv.df <- merge(est.cv.df, height.mean[, c("stationcode", "V1")], by.x=".id", by.y="stationcode")
est.cv.df$Transect <- as.factor(est.cv.df$Transect)
ggplot(est.cv.df , aes(Transect, Value, group=.id, colour=V1)) + geom_jitter(alpha=0.1) +
  labs(x="Number of Transects", y="Estimated cv", title="Bankful Height cv Estimates") +
  scale_color_continuous(low="Blue", high="Red")