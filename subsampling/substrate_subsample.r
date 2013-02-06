library(plyr)

substrate2 <- function(data){
  data <- subset(data, AnalyteName %in% c("Substrate Size Class",
                                          "Embeddedness",
                                          "CPOM") &
                   ResQualCode == "=")
  data$Location2 <- sapply(strsplit(as.character(data$LocationCode), ","), function(x)x[1])
  
  substrate <- subset(data, AnalyteName == "Substrate Size Class")
  substrate$VariableResult <- as.character(substrate$VariableResult)  
  substrate$VariableResult2 <- as.character(cut(substrate$Result, breaks=c(0, 0.06, 2, 16, 64, 250, 1000, 4000), 
                                                labels=c("FN", "SA", "GF", "GC", "CB", "SB", "XB")))
  substrate$VariableResult2 <- mapply(function(x,y)ifelse(!is.na(x), x, y), substrate$VariableResult, substrate$VariableResult2)
  
  substrate$Result2 <- with(substrate, ifelse(VariableResult == "RS", 5660, ifelse(
    VariableResult=="RR", 5660, ifelse(
      VariableResult=="XB", 2500, ifelse(
        VariableResult=="SB", 625, ifelse(
          VariableResult=="CB", 157, ifelse(
            VariableResult=="GC", 9, ifelse(
              VariableResult=="GF", 9, ifelse(
                VariableResult=="SA", 1.03, ifelse(
                  VariableResult=="FN", 0.03, ifelse(
                    VariableResult=="HP", 5660, ifelse(
                      VariableResult=="RC", 5660, NA))))))))))))
  
  substrate$Result2 <- mapply(function(x,y)ifelse(!is.na(x), x, y), substrate$Result2, substrate$Result)
  
  metrics <- c(PCT_RS = function(x)sum(x$VariableResult2 == 'RS', na.rm=T)/x$total[1],
               PCT_RR = function(x)sum(x$VariableResult2 == 'RR', na.rm=T)/x$total[1],
               PCT_RC = function(x)sum(x$VariableResult2 == 'RC', na.rm=T)/x$total[1],
               PCT_XB = function(x)sum(x$VariableResult2 == 'RR', na.rm=T)/x$total[1],
               PCT_SB = function(x)sum(x$VariableResult2 == 'SB', na.rm=T)/x$total[1],
               PCT_CB = function(x)sum(x$VariableResult2 == 'CB', na.rm=T)/x$total[1],
               PCT_GC = function(x)sum(x$VariableResult2 == 'GC', na.rm=T)/x$total[1],
               PCT_GF = function(x)sum(x$VariableResult2 == 'GF', na.rm=T)/x$total[1],
               PCT_SA = function(x)sum(x$VariableResult2 == 'SA', na.rm=T)/x$total[1],
               PCT_FN = function(x)sum(x$VariableResult2 == 'FN', na.rm=T)/x$total[1],
               PCT_HP = function(x)sum(x$VariableResult2 == 'HP', na.rm=T)/x$total[1],
               PCT_WD = function(x)sum(x$VariableResult2 == 'WD', na.rm=T)/x$total[1],
               PCT_OT = function(x)sum(x$VariableResult2 == 'OT', na.rm=T)/x$total[1],
               PCT_BDRK = function(x)sum(x$VariableResult2 %in% c('RR', 'RS'))/x$total[1],
               PCT_BIGR = function(x)sum(x$VariableResult2 %in% c('RR', 'RS', 'XB', 'SB', 'CB', 'GC'))/x$total[1],
               PCT_SFGF = function(x)sum(x$VariableResult2 %in% c('SA', 'FN', 'GF'))/x$total[1],
               PCT_SAFN = function(x)sum(x$VariableResult2 %in% c('SA', 'FN'))/x$total[1],
               XSDGM = function(x)10^(sum(log10(x$Result2))/x$total[1]),
               XSPDGM = function(x)10^(sum(log10(x$Result2[x$Result2 <= 2500]))/x$total[1]),
               SB_PT_D50 = function(x)parent.frame(6)$quantAll["50%"],
               SB_PT_D10 = function(x)parent.frame(6)$quantAll["10%"],
               SB_PT_D25 = function(x)parent.frame(6)$quantAll["25%"],
               SB_PT_D75 = function(x)parent.frame(6)$quantAll["75%"],
               SB_PT_D90 = function(x)parent.frame(6)$quantAll["90%"],
               SB_PP_D50 = function(x)parent.frame(6)$quantPart["50%"],
               SB_PP_D10 = function(x)parent.frame(6)$quantPart["10%"],
               SB_PP_D25 = function(x)parent.frame(6)$quantPart["25%"],
               SB_PP_D75 = function(x)parent.frame(6)$quantPart["75%"],
               SB_PP_D90 = function(x)parent.frame(6)$quantPart["90%"]
  )
  
  lapply(1:21, function(x){
  substrateMetrics <- metricCalc2("d$total <- sum(!is.na(d$VariableResult2))",
                                 "quantAll <- quantile(l$Result2, c(0.5, 0.1, 0.25, 0.75, 0.9), na.rm=T)
                                  quantPart <- quantile(l$Result2[l$Result2 <= 2500], c(0.5, 0.1, 0.25, 0.75, 0.9), na.rm=T)", x)
  result1 <- substrateMetrics(substrate, metrics)
  
  cpom <- subset(data, AnalyteName == "CPOM")
  cpomMetric <- metricCalc2(NULL, NULL, x)
  result2 <- cpomMetric(cpom, c("CPOM" = function(x)sum(x$VariableResult=='Present')/sum(x$VariableResult %in% c('Present', 'Absent'))))
  
  embed <- subset(data, AnalyteName == "Embeddedness")
  embedMetric <- metricCalc2(NULL, NULL, x)
  result3 <- embedMetric(embed, c("XEMBED" = function(x)mean(x$Result, na.rm=T)))
  
  rbind(result1, result2, result3)
  })
}