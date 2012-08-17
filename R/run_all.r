###PHAB metrics###

###Metric Calculations###
start <- proc.time()
source("L://Bioassessment Data Management Tools_RM/R Scripts/Bank_morphology.r")
source("L://Bioassessment Data Management Tools_RM/R Scripts/channel_morphology.r")
source("L://Bioassessment Data Management Tools_RM/R Scripts/densiometer.r")
source("L://Bioassessment Data Management Tools_RM/R Scripts/Percent Bank Stability.r")
source("L://Bioassessment Data Management Tools_RM/R Scripts/riparian_vegetation.r")
source("L://Bioassessment Data Management Tools_RM/R Scripts/human_disturbance.r")
source("L://Bioassessment Data Management Tools_RM/R Scripts/substrate_size_and_composition.r")
source("L://Bioassessment Data Management Tools_RM/R Scripts/channel_sinuosity_and_slope.r")
source("L://Bioassessment Data Management Tools_RM/R Scripts/habitat_complexity.r")
source("L://Bioassessment Data Management Tools_RM/R Scripts/algae.r")
source("L://Bioassessment Data Management Tools_RM/R Scripts/flow.r")
source("L://Bioassessment Data Management Tools_RM/R Scripts/quality.r")
source("L://Bioassessment Data Management Tools_RM/R Scripts/misc.r")

###Data formatting###
library(reshape)
filelist <- c("Bank_morphology_metrics.csv", "channel_morphology_metrics.csv",
              "densiometer_metrics.csv", "Percent_Bank_Stability_metrics.csv", 
              "riparian_vegetation_metrics.csv", "human_disturbance_metrics.csv",
              "substrate_size_and_composition_metrics.csv", 
              "channel_sinuosity_and_slope_metrics.csv", "habitat_complexity_metrics.csv",
              "algae_metrics.csv", "water_quality_metrics.csv", "misc_metrics.csv", 
              "flow_metrics.csv")

finalresults <- data.frame()
for(i in 1:length(filelist)){
  aa <- read.csv(filelist[i])[,which(!(colnames(read.csv(filelist[i])) %in% c("Filename", "Filename.1", "Filename.2", "Filename.3",
                                                                              "Filename.4", "Filename.5", "Filename.6", "Filename.7", "Filename.8"
                                                                              , "Filename.9", "X.1", "X.2", "X.3", "X.4"
                                                                              
                                                                              , "X.5", "X.6", "X.7", "X.8", "X.9", "X.10", "Filename.10",
                                                                              "X.11", "X.12", "Filename.11", "Filename.12")))]
  colnames(aa)[1] <- "ID"
  output <- aa[,which(!(1:length(colnames(aa)) %in% c((grep("_count", colnames(aa))),
                                                      (grep("_sd", colnames(aa))))))]
  output <- melt(output, id="ID")
  output$type<-gsub("([[:alnum:]_]+)\\.([[:alnum:]]+)", "\\2", output$variable)
  output$variable<-gsub("([[:alnum:]_]+)\\.([[:alnum:]]+)", "\\1", output$variable)
  output$ID<-gsub("Site ([[:digit:]])", "Site_\\1", output$ID)
  output$ID<-gsub("SGUT ([[:digit:]])", "SGUT_\\1", output$ID)
  output$StationCode <- gsub("([_[:alnum:]-]+)[[:blank:]]([[:alnum:]-]+)", "\\1", output$ID)
  output$SampleDate <- gsub("([_[:alnum:]-]+)[[:blank:]]([[:alnum:]-]+)", "\\2", output$ID)
  
  finalresults<-rbind(output[, c("StationCode", "SampleDate", "variable", "type", "value")], finalresults)
}

fc <- file("all_metrics_revised.csv", open="w")
write.csv(finalresults, fc)
close(fc)
#unlink(filelist)
end <-proc.time()
print(end-start)

setwd("C:\\Documents and Settings\\gisuser\\Desktop")
gmailsender("mengeln@gmail.com", "mengeln@gmail.com", finalresults, user="mengeln", pw="kram92620")
