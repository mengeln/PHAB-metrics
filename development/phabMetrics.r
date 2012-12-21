

phabMetrics <- function(x){
  
  metrics <- c("algae", "bankMorphology", "channelMorphology", "habitatComplexity", "humanDisturbance",
               "riparianVegetation", "slope", "substrate")
  
  result <- lapply(metrics, function(fn)cbind(eval(parse(text=fn))(x), "class" = fn))
  
  Reduce(rbind, result)
}