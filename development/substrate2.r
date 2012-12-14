library(plyr)

substrate <- function(data){
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
  
  metrics <- c("PCT_RS" = "sum(d$VariableResult2 == 'RS', na.rm=T)/total",
               "PCT_RR" = "sum(d$VariableResult2 == 'RR', na.rm=T)/total",
               "PCT_RC" = "sum(d$VariableResult2 == 'RC', na.rm=T)/total",
               "PCT_XB" = "sum(d$VariableResult2 == 'RR', na.rm=T)/total",
               "PCT_SB" = "sum(d$VariableResult2 == 'SB', na.rm=T)/total",
               "PCT_CB" = "sum(d$VariableResult2 == 'CB', na.rm=T)/total",
               "PCT_GC" = "sum(d$VariableResult2 == 'GC', na.rm=T)/total",
               "PCT_GF" = "sum(d$VariableResult2 == 'GF', na.rm=T)/total",
               "PCT_SA" = "sum(d$VariableResult2 == 'SA', na.rm=T)/total",
               "PCT_FN" = "sum(d$VariableResult2 == 'FN', na.rm=T)/total",
               "PCT_HP" = "sum(d$VariableResult2 == 'HP', na.rm=T)/total",
               "PCT_WD" = "sum(d$VariableResult2 == 'WD', na.rm=T)/total",
               "PCT_OT" = "sum(d$VariableResult2 == 'OT', na.rm=T)/total",
               "PCT_BDRK" = "sum(d$VariableResult2 %in% c('RR', 'RS'))/total",
               "PCT_BIGR" = "sum(d$VariableResult2 %in% c('RR', 'RS', 'XB', 'SB', 'CB', 'GC'))/total",
               "PCT_SFGF" = "sum(d$VariableResult2 %in% c('SA', 'FN', 'GF'))/total",
               "PCT_SAFN" = "sum(d$VariableResult2 %in% c('SA', 'FN'))/total",
               "XSDGM" = "10^(sum(log10(d$Result2))/total)",
               "XSPDGM" = "10^(sum(log10(d$Result2[d$Result2 <= 2500]))/total)",
               "SB_PT_D50" = "quantAll['50%']",
               "SB_PT_D10" = "quantAll['10%']",
               "SB_PT_D25" = "quantAll['25%']",
               "SB_PT_D75" = "quantAll['75%']",
               "SB_PT_D90" = "quantAll['90%']",
               "SB_PP_D50" = "quantPart['50%']",
               "SB_PP_D10" = "quantPart['10%']",
               "SB_PP_D25" = "quantPart['25%']",
               "SB_PP_D75" = "quantPart['75%']",
               "SB_PP_D90" = "quantPart['90%']")

  substrateMetrics <- metricCalc("total <- sum(!is.na(d$VariableResult2))",
                                 "quantAll <- quantile(l$Result2, c(0.5, 0.1, 0.25, 0.75, 0.9), na.rm=T)
                                  quantPart <- quantile(l$Result2[l$Result2 <= 2500], c(0.5, 0.1, 0.25, 0.75, 0.9), na.rm=T)")
  result1 <- substrateMetrics(substrate, metrics)
  
  cpom <- subset(data, AnalyteName == "CPOM")
  cpomMetric <- metricCalc(NULL)
  result2 <- cpomMetric(cpom, c("CPOM" = "sum(d$VariableResult=='Present')/sum(d$VariableResult %in% c('Present', 'Absent'))"))
  
  embed <- subset(data, AnalyteName == "Embeddedness")
  embedMetric <- metricCalc(NULL)
  result3 <- embedMetric(embed, c("XEMBED" = "mean(d$Result, na.rm=T)"))
  
  rbind(result1, result2, result3)
}