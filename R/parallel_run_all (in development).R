#rm(list=ls())
biglist <- c("L://Bioassessment Data Management Tools_RM/R Scripts/Bank_morphology.r",
"L://Bioassessment Data Management Tools_RM/R Scripts/channel_morphology.r",
"L://Bioassessment Data Management Tools_RM/R Scripts/densiometer.r",
"L://Bioassessment Data Management Tools_RM/R Scripts/Percent Bank Stability.r",
"L://Bioassessment Data Management Tools_RM/R Scripts/riparian_vegetation.r",
"L://Bioassessment Data Management Tools_RM/R Scripts/human_disturbance.r",
"L://Bioassessment Data Management Tools_RM/R Scripts/substrate_size_and_composition.r",
"L://Bioassessment Data Management Tools_RM/R Scripts/channel_sinuosity_and_slope.r",
"L://Bioassessment Data Management Tools_RM/R Scripts/habitat_complexity.r",
"L://Bioassessment Data Management Tools_RM/R Scripts/algae.r")

parallelt_start <- proc.time()
require(doSNOW)
registerDoSNOW(makeCluster(2, type = "SOCK"))


foreach(i = 1:length(biglist), .packages="reshape") %dopar% {source(biglist[i])}


parallelt_end <- proc.time()
print(parallelt_end-parallelt_start)

getDoParWorkers()
getDoParName()
getDoParVersion()