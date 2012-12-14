library(reshape)
library(ggplot2)

load("data/fullSMCFieldHabitat.rdata")

analyte <- "Bankfull Height"
pdf(file=paste(analyte, "_boostrap_mean2.pdf", sep=""))
#for(names in sample(unique(full$stationcode[full$analytename== analyte]), 3)){
for(names in unique(full$stationcode[full$analytename== analyte])){
  bank <- full[full$analytename==analyte & full$stationcode == names & !is.na(full$result) & (full$result > 0 ), ]
  if(nrow(bank)==11){
    mc <- sapply(2:11, function(i){
      sapply(1:100, function(j){
        s <- sample(bank$result, i, replace=T)
        sd(s)
      })
    })
    meltmc <- melt(mc)
    meltmc$X2 <- meltmc$X2 + 1
    meltmc <- meltmc[meltmc$value!=Inf, ]
    print(ggplot(meltmc, aes(x=as.factor(X2), y=value)) + geom_violin(fill="grey80") +
      geom_boxplot(alpha=0.6) +
      stat_sum_single(mean, geom="point") +
      #geom_jitter(alpha=0.1) + 
      labs(x="Number of Transects", y="Mean", title=names)) 
  }
}
dev.off()
