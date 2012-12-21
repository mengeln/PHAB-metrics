
flow <- function(x){
  data <- subset(x, AnalyteName %in% c("Distance from Bank", "StationWaterDepth", "Velocity") )
  data$Location2 <-  sapply(strsplit(as.character(data$LocationCode), ","), function(x)x[1])
  data$Location3 <-  sapply(strsplit(as.character(data$LocationCode), ","), function(x)x[2])
  
  data_va <- subset(data, Location2 == "X")
  flow <- dcast(data_va, SampleID + Replicate ~ AnalyteName, value.var="Result")
  
  flow$Distance2 <- Reduce(c, 
                           lapply(split(flow, flow$SampleID), function(flow){ 
                             sapply(1:nrow(flow), function(i){
                               if(i == 1)flow$Distance[2] - flow$Distance[1] else
                                 flow$Distance[i] - flow$Distance[i-1]
                             })
                           }))/2
  
  flow$discharge <- mapply(function(dis, depth, vel) dis * depth * vel * 0.00107639104,
                           flow$Distance2, flow$StationWaterDepth, flow$Velocity)
  flow$Location2 <- 1:nrow(flow)
  
  
  metricCalc("result <- sum(l$discharge)")(flow, c("FL_Q_F" = "sum(result)",
                                                   "FL_Q_M" = "sum(result)*0.0283168466"))
}