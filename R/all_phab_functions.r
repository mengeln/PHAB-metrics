algae <- function (algae) {
  algae <- algae[which(algae$AnalyteName %in% c('Microalgae Thickness', 'Macrophyte Cover', 'Macroalgae Cover, Attached', 'Macroalgae Cover, Unattached')),]
  ###Slice for microalgae###
  microalgae <- data.frame(cbind(algae$id[which(algae$AnalyteName == 'Microalgae Thickness')], as.character(algae$VariableResult[which(algae$AnalyteName == 'Microalgae Thickness')])))
  colnames(microalgae) <- c("id", "VariableResult")
  
  
  ###Compute PCT_MIATP###
  
  FUN_PCT_MIATP <- function(data){
    x <- {1:length(data)}
    present_calculation <- function(number){
      for(i in 1:length(data)){
        if(data[i] == number ){
          x[i] <- 1
        }else{
          x[i] <- 0
        }
      }
      present <- sum(x)
      return(present) 
    }
    present_subtotals <- tapply(1:5, as.factor(1:5), present_calculation)
    present <- sum(present_subtotals)
    
    total_calculation <- function(number){
      for(i in 1:length(data)){
        if(data[i] == number ){
          x[i] <- 1
        }else{
          x[i] <- 0
        }
      }
      total <- sum(x)
      return(total) 
    }
    total_subtotals <- tapply(0:5, as.factor(0:5), total_calculation)
    total <- sum(total_subtotals)
    result <- (present/total)*100
    return(result)
  }
  PCT_MIATP.result <- tapply(microalgae$VariableResult, microalgae$id, FUN_PCT_MIATP)
  
  
  ###Compute PCT_MIAT1###
  
  FUN_PCT_MIAT1 <- function(data){
    x <- {1:length(data)}
    present_calculation <- function(number){
      for(i in 1:length(data)){
        if(data[i] == number ){
          x[i] <- 1
        }else{
          x[i] <- 0
        }
      }
      present <- sum(x)
      return(present) 
    }
    present_subtotals <- tapply(3:5, as.factor(3:5), present_calculation)
    present <- sum(present_subtotals)
    
    total_calculation <- function(number){
      for(i in 1:length(data)){
        if(data[i] == number ){
          x[i] <- 1
        }else{
          x[i] <- 0
        }
      }
      total <- sum(x)
      return(total) 
    }
    total_subtotals <- tapply(0:5, as.factor(0:5), total_calculation)
    total <- sum(total_subtotals)
    result <- (present/total)*100
    return(result)
  }
  PCT_MIAT1.result <- tapply(microalgae$VariableResult, microalgae$id, FUN_PCT_MIAT1)
  
  
  ###Compute PCT_MIAT1P###
  
  FUN_PCT_MIAT1P <- function(data){
    x <- {1:length(data)}
    present_calculation <- function(number){
      for(i in 1:length(data)){
        if(data[i] == number ){
          x[i] <- 1
        }else{
          x[i] <- 0
        }
      }
      present <- sum(x)
      return(present) 
    }
    present_subtotals <- tapply(3:5, as.factor(3:5), present_calculation)
    present <- sum(present_subtotals)
    
    total_calculation <- function(number){
      for(i in 1:length(data)){
        if(data[i] == number ){
          x[i] <- 1
        }else{
          x[i] <- 0
        }
      }
      total <- sum(x)
      return(total) 
    }
    total_subtotals <- tapply(1:5, as.factor(1:5), total_calculation)
    total <- sum(total_subtotals)
    result <- (present/total)*100
    return(result)
  }
  PCT_MIAT1P.result <- tapply(microalgae$VariableResult, microalgae$id, FUN_PCT_MIAT1P)
  PCT_MIAT1P.result[is.na(PCT_MIAT1P.result)] <- 0
  
  
  ###Convert data values for XMIAT and XMIATP###
  
  XMIAT_data <- microalgae$VariableResult
  XMIAT_data <- as.character(XMIAT_data)
  for(i in 1:length(XMIAT_data)){
    if(XMIAT_data[i] == 1){XMIAT_data[i] <- .25} else
      if(XMIAT_data[i] == 2){XMIAT_data[i] <- .5} else
        if(XMIAT_data[i] == 4){XMIAT_data[i] <- 12.5} else
          if(XMIAT_data[i] == 5){XMIAT_data[i] <- 20}
  }
  XMIAT_data <- as.numeric(XMIAT_data)
  XMIAT_frame <- microalgae
  XMIAT_frame$result <- XMIAT_data
  
  ###Compute XMIAT###
  
  XMIAT_countss <- function(data){
    XMIAT_count <- sum(!is.na(data))
    return(XMIAT_count)	
  }
  XMIAT_meanss <- function(data){
    XMIAT_count <- sum(!is.na(data))
    XMIAT_mean <- mean(data, na.rm=TRUE)
    return(XMIAT_mean)	
  }
  XMIAT_SDSs <- function(data){
    XMIAT_SD <- sd(data, na.rm=TRUE)
    return(XMIAT_SD)
  }
  XMIAT_countst <- tapply(XMIAT_frame$result, XMIAT_frame$id, XMIAT_countss)
  XMIAT_meanst <- tapply(XMIAT_frame$result, XMIAT_frame$id, XMIAT_meanss)
  XMIAT_sdst <- tapply(XMIAT_frame$result, XMIAT_frame$id, XMIAT_SDSs)
  
  XMIAT <- cbind(XMIAT_meanst, XMIAT_countst, XMIAT_sdst)
  colnames(XMIAT) <- c("XMIAT.result", "XMIAT.count", "XMIAT.sd")
  
  ###Compute XMIATP###
  
  XMIATP_countss <- function(data){
    XMIATP_count<- sum(!is.na(data))-length(which(data == 0))
    return(XMIATP_count)	
  }
  XMIATP_meanss <- function(data){
    XMIATP_count <- sum(!is.na(data))-length(which(data == 0))
    XMIATP_mean<- sum(data, na.rm=TRUE)/XMIATP_count
    if(XMIATP_mean == "NaN"){XMIATP_mean <- 0}
    return(XMIATP_mean)	
  }
  XMIATP_SDSs <- function(data){
    XMIATP_SD <- sd(data[which(data != 0)], na.rm=TRUE)
    return(XMIATP_SD)
  }
  XMIATP_countst <- tapply(XMIAT_frame$result, XMIAT_frame$id, XMIATP_countss)
  XMIATP_meanst <- tapply(XMIAT_frame$result, XMIAT_frame$id, XMIATP_meanss)
  XMIATP_sdst <- tapply(XMIAT_frame$result, XMIAT_frame$id, XMIATP_SDSs)
  
  XMIATP <- cbind(XMIATP_meanst, XMIATP_countst, XMIATP_sdst)
  colnames(XMIATP) <- c("XMIATP.result", "XMIATP.count", "XMIATP.sd")
  XMIATP
  
  ###Slice for macrophyte cover data###
  
  macrophyte_cover <- data.frame(cbind(algae$id[which(algae$AnalyteName == 'Macrophyte Cover')], as.character(algae$VariableResult[which(algae$AnalyteName == 'Macrophyte Cover')])))
  colnames(macrophyte_cover) <- c("id", "VariableResult")
  
  
  ###Compute PCT_MCP###
  
  PCT_MCP_stats <- function(data){
    present <- length(which(data == "Present"))
    total <- length(which(data == "Present"))+
      length(which(data == "Absent"))
    result <- (present/total)*100
    return(result)
  }
  PCT_MCP.result <- tapply(macrophyte_cover$VariableResult, macrophyte_cover$id, PCT_MCP_stats)
  
  
  ###Call macrophyte cover attached data###
  macroalgae_cover_attached <- data.frame(cbind(algae$id[which(algae$AnalyteName == 'Macroalgae Cover, Attached')], as.character(algae$VariableResult[which(algae$AnalyteName == 'Macroalgae Cover, Attached')])))
  colnames(macroalgae_cover_attached) <- c("id", "VariableResult")
  
  ###Compute PCT_MAA###
  
  PCT_MAA_stats <- function(data){
    present <- length(which(data == "Present"))
    total <- length(which(data == "Present"))+
      length(which(data == "Absent"))
    result <- (present/total)*100
    return(result)
  }
  PCT_MAA.result <- tapply(macroalgae_cover_attached$VariableResult, macroalgae_cover_attached$id, PCT_MAA_stats)
  
  ###Call macrophyte cover unattached data###
  macroalgae_cover_unattached <- data.frame(cbind(algae$id[which(algae$AnalyteName == 'Macroalgae Cover, Unattached')], as.character(algae$VariableResult[which(algae$AnalyteName == 'Macroalgae Cover, Unattached')])))
  colnames(macroalgae_cover_unattached) <- c("id", "VariableResult")
  
  ###Compute PCT_MAU###
  
  PCT_MAU_stats <- function(data){
    present <- length(which(data == "Present"))
    total <- length(which(data == "Present"))+
      length(which(data == "Absent"))
    result <- (present/total)*100
    return(result)
  }
  PCT_MAU.result <- tapply(macroalgae_cover_unattached$VariableResult, macroalgae_cover_unattached$id, PCT_MAA_stats)
  
  
  ###Compute PCT_MAP###
  
  colnames(macroalgae_cover_unattached)<- c("id", "VariableResult2")
  macroalgae_cover <- cbind(macroalgae_cover_unattached, macroalgae_cover_attached$VariableResult)
  colnames(macroalgae_cover) <- c("id", "unattached", "attached")
  
  macroalgae_cover$PCT_MAP <- 1:length(macrophyte_cover$id)
  for(i in 1:length(macrophyte_cover$id)){
    if(((macroalgae_cover$unattached[i] == "Present")|(macroalgae_cover$attached[i] == "Present"))){macroalgae_cover$PCT_MAP[i] <- "Present"} else
      if (((macroalgae_cover$unattached[i] == "Absent")&(macroalgae_cover$attached[i] == "Absent"))) {macroalgae_cover$PCT_MAP[i] <- "Absent"
      }
  }
  
  PCT_MAP_stats <- function(data){
    present <- length(which(data=="Present"))
    total <- length(which(data=="Absent"))+present
    result <- 100*(present/total)
    return(result)
  }
  PCT_MAP.result <- tapply(macroalgae_cover$PCT_MAP, macroalgae_cover$id, PCT_MAP_stats)
  
  ###Compute PCT_NSA###
  
  macroalgae_cover$PCT_NSA_characters <- as.character(microalgae$VariableResult)
  for(i in 1:length(microalgae$VariableResult)){
    if(microalgae$VariableResult[i] == "3"){macroalgae_cover$PCT_NSA_characters[i] <- "Present"} else
      if(microalgae$VariableResult[i] == "4"){macroalgae_cover$PCT_NSA_characters[i] <- "Present"} else
        if(microalgae$VariableResult[i] == "5"){macroalgae_cover$PCT_NSA_characters[i] <- "Present"} else
          if(microalgae$VariableResult[i] == "0"){macroalgae_cover$PCT_NSA_characters[i] <- "Absent"} else
            if(microalgae$VariableResult[i] == "1"){macroalgae_cover$PCT_NSA_characters[i] <- "Absent"} else
              if(microalgae$VariableResult[i] == "2"){macroalgae_cover$PCT_NSA_characters[i] <- "Absent"} 
  }
  macroalgae_cover$PCT_NSA <- 1:length(macrophyte_cover$id)
  for(i in 1:length(macrophyte_cover$id)){
    if(((macroalgae_cover$PCT_MAP[i] == "Present")|(macroalgae_cover$PCT_NSA_characters[i] == "Present"))){macroalgae_cover$PCT_NSA[i] <- "Present"} else
      if (((macroalgae_cover$PCT_MAP[i] == "Absent")&(macroalgae_cover$PCT_NSA_characters[i] == "Absent"))) {macroalgae_cover$PCT_NSA[i] <- "Absent"
      }
  }
  PCT_NSA_sum <- function(data){
    result <- length(which(data=="Present"))
    return(result)}
  
  macroalgae_cover$PCT_NSA_total <- 1:length(macroalgae_cover$id)
  for(i in 1:length(macroalgae_cover$id)){
    if(macroalgae_cover$unattached[i] == "Dry"){macroalgae_cover$PCT_NSA_total[i] <- "blank"} else{
      if(macroalgae_cover$unattached[i] == "Not Recorded"){macroalgae_cover$PCT_NSA_total[i] <- "blank"} else{
        if(macroalgae_cover$attached[i] == "Not Recorded"){macroalgae_cover$PCT_NSA_total[i] <- "blank"} else{
          if(macroalgae_cover$attached[i] == "Dry"){macroalgae_cover$PCT_NSA_total[i] <- "blank"} else{
            if(macroalgae_cover$PCT_NSA_characters[i] == "Dry"){macroalgae_cover$PCT_NSA_total[i] <- "blank"} else{		
              if(macroalgae_cover$PCT_NSA_characters[i] == "Not Recorded"){macroalgae_cover$PCT_NSA_total[i] <- "blank"}}}}}}}
  
  PCT_NSA_total <- function(data){
    result <- length(which(data != "blank"))
    return(result)}
  
  PCT_NSA_sums <- tapply(macroalgae_cover$PCT_NSA, macroalgae_cover[[1]], PCT_NSA_sum)
  PCT_NSA_totals <- tapply(macroalgae_cover$PCT_NSA_total, macroalgae_cover[[1]], PCT_NSA_total)
  PCT_NSA.result <- (PCT_NSA_sums/PCT_NSA_totals)*100
  
  
  
  ###Write the results to file###
  
  algae_results1 <- cbind(PCT_MIATP.result, PCT_MIAT1.result, PCT_MIAT1P.result, PCT_MAA.result, PCT_MCP.result,
                          PCT_MAU.result, PCT_MAP.result, PCT_NSA.result)
  algae_results_final <- cbind(XMIAT, XMIATP, algae_results1)
  print("algae done")
  algae_results_final
}

bankmorph <- function (bankmorph) {
  bankmorph <- bankmorph[which(bankmorph$AnalyteName %in% c('Bankfull Height', 'Bankfull Width', 'StationWaterDepth', 'Wetted Width')),]
  bankmorph <- bankmorph[which(!(bankmorph$LocationCode == "X")),]
  bankmorph$Result[bankmorph$Result == -88] <- NA
  ###Slice Bankfull Height Data ###
  
  heightindex <- which(bankmorph$AnalyteName == "Bankfull Height")
  bankfullheight <- data.frame(cbind(bankmorph$id[heightindex], as.character(bankmorph$AnalyteName[heightindex]),
                                     as.character(bankmorph$Result[heightindex])))
  colnames(bankfullheight) <- c("id", "AnalyteName", "result")
  bankfullheight$result <- as.numeric(as.character(bankfullheight$result))
  head(bankfullheight)
  
  ###Compute XBKF_H###
  sumna <- function(data){
    sum(as.numeric(as.character(data)), na.rm = T)
  }
  XBKF_sum <- tapply(bankfullheight$result, bankfullheight$id, sumna)
  lengthna <- function(data){
    sum(!is.na(data))
  }
  XBKF_H.count <- tapply(bankfullheight$result, bankfullheight$id, lengthna)
  XBKF_H.result <- XBKF_sum/XBKF_H.count
  sdna <- function(data){
    sd(data, na.rm = T)
  }
  XBKF_H.sd <- tapply(bankfullheight$result, bankfullheight$id, sdna)
  XBKF_H <- data.frame(cbind(XBKF_H.result, XBKF_H.count, XBKF_H.sd))
  XBKF_H
  
  ###Slice Bankfull Width###
  
  widthindex <- which(bankmorph$AnalyteName == "Bankfull Width")
  bankfullwidth <- data.frame(cbind(bankmorph$id[widthindex], as.character(bankmorph$AnalyteName[widthindex]),
                                    as.character(bankmorph$Result[widthindex])))
  colnames(bankfullwidth) <- c("id", "AnalyteName", "result")
  bankfullwidth$result <- as.numeric(as.character(bankfullwidth$result))
  head(bankfullwidth)
  
  ###Compute XKBF_W###
  
  XKBF_W_sum <- tapply(bankfullwidth$result, bankfullwidth$id, sumna)
  XKBF_W.count <- tapply(bankfullwidth$result, bankfullwidth$id, lengthna)
  XKBF_W.result <- XKBF_W_sum/XKBF_W.count
  XKBF_W.sd <- tapply(bankfullwidth$result, bankfullwidth$id, sdna)
  XKBF_W <- data.frame(cbind(XKBF_W.result, XKBF_W.count, XKBF_W.sd))
  XKBF_W
  
  ###XWDEPTH###
  
  XWDEPTHdata <- data.frame(cbind(bankmorph$id[which(bankmorph$AnalyteName == "StationWaterDepth")], 
                                  as.numeric(as.character(bankmorph$Result[which(bankmorph$AnalyteName == "StationWaterDepth")]))))
  colnames(XWDEPTHdata) <- c("id", "result")
  XWDEPTH_sum <- tapply(XWDEPTHdata$result, XWDEPTHdata$id, sumna)
  XWDEPTH.count <- tapply(XWDEPTHdata$result, XWDEPTHdata$id, lengthna)
  XWDEPTH.result <- XWDEPTH_sum/XWDEPTH.count
  XWDEPTH.sd <- tapply(XWDEPTHdata$result, XWDEPTHdata$id, sdna)
  
  ###XWIDTH###
  
  XWIDTHdata <- data.frame(cbind(bankmorph$id[which(bankmorph$AnalyteName == "Wetted Width")], 
                                 as.numeric(as.character(bankmorph$Result[which(bankmorph$AnalyteName == "Wetted Width")]))))
  colnames(XWIDTHdata) <- c("id", "result")
  XWIDTH_sum <- tapply(XWIDTHdata$result, XWIDTHdata$id, sumna)
  XWIDTH.count <- tapply(XWIDTHdata$result, XWIDTHdata$id, lengthna)
  XWIDTH.result <- XWIDTH_sum/XWIDTH.count
  XWIDTH.sd <- tapply(XWIDTHdata$result, XWIDTHdata$id, sdna)
  
  ###XWDR###
  
  XWDR.result <- (XWIDTH.result/XWDEPTH.result)*100
  XWDR.count<- XWIDTH.count
  ###XWDA###
  
  XWDA.result <- XWDEPTH.result/(XWIDTH.result*10)
  XWDA.count <- XWDEPTH.count
  ###XWDM###
  library(reshape)
  bankmorph$LocationCode2 <- gsub("Tran.(\\w)", "\\1", bankmorph$LocationCode)
  
  ll <- c( ", LCtr" , ", Ctr" , ", RCtr",
           ", Right", ", Left")
  for(i in 1:length(ll)){
    bankmorph$LocationCode2 <-gsub(ll[i], "", bankmorph$LocationCode2)}
  XWDM_max <- cast(bankmorph[bankmorph$AnalyteName == "StationWaterDepth", ], 
                   id ~ LocationCode2, value = "Result", fun.aggregate =max)
  XWDM_max[XWDM_max == -Inf] <- NA
  XWDM_max<- XWDM_max[,which(!(1:length(colnames(XWDM_max)) %in% grep("Float", colnames(XWDM_max))))]
  dim <- XWDM_max[[1]]
  XWDM_max<-XWDM_max[, which(!(1:(length(XWDM_max)-1) %in% grep("Section", (colnames(XWDM_max)))))]
  XWDM_max <- XWDM_max[, 2:length(XWDM_max)]
  XWDM.result <- rowSums(XWDM_max, na.rm = T)/apply(XWDM_max, 1, lengthna)
  names(XWDM.result) <- dim
  XWDM.count <-apply(XWDM_max, 1, lengthna)
  
  ###Write to file###
  results <- cbind(XBKF_H.result, XBKF_H.count, XBKF_H.sd, XKBF_W.result, XKBF_W.count, XKBF_W.sd, XWDEPTH.result, 
                   XWDEPTH.count, XWDEPTH.sd, XWIDTH.result, XWIDTH.count, XWIDTH.sd, XWDR.result, XWDR.count, 
                   XWDA.result, XWDA.count, XWDM.result , XWDM.count)
  print("bankmorph done")
  results
}

channelmorph <- function (channel_morph) {
  channel_morph <- channel_morph[which(channel_morph$AnalyteName %in% c('Cascade/Falls', 'Dry', 'Glide', 'Pool', 'Rapid', 'Riffle', 'Run')),]
  ###Compute PCT_CF###
  
  PCT_CF_data <- data.frame(cbind(channel_morph$id[which(channel_morph$AnalyteName == "Cascade/Falls")], channel_morph$Result[which(channel_morph$AnalyteName == "Cascade/Falls")]))
  colnames(PCT_CF_data) <- c("id", "result")
  
  sumna <- function(data)sum(as.numeric(as.character(data)), na.rm = T) 
  sdna <- function(data)sd(data, na.rm = T)
  lengthna <- function(data)sum(!is.na(data))
  
  PCT_CF_sum <- tapply(PCT_CF_data$result, PCT_CF_data$id, sumna)
  PCT_CF.count <- tapply(PCT_CF_data$result, PCT_CF_data$id, lengthna)
  PCT_CF.result <- PCT_CF_sum/PCT_CF.count
  PCT_CF.sd <- tapply(PCT_CF_data$result, PCT_CF_data$id, sdna)
  
  ###PCT_DR###
  
  PCT_DR_data <- data.frame(cbind(channel_morph$id[which(channel_morph$AnalyteName == "Dry")], channel_morph$Result[which(channel_morph$AnalyteName == "Dry")]))
  colnames(PCT_DR_data) <- c("id", "result")
  
  PCT_DR_sum <- tapply(PCT_DR_data$result, PCT_DR_data$id, sumna)
  PCT_DR.count <- tapply(PCT_DR_data$result, PCT_DR_data$id, lengthna)
  PCT_DR.result <- PCT_DR_sum/PCT_DR.count
  PCT_DR.sd <- tapply(PCT_DR_data$result, PCT_DR_data$id, sdna)
  
  ###PCT_GL###
  
  PCT_GL_data <- data.frame(cbind(channel_morph$id[which(channel_morph$AnalyteName == "Glide")], channel_morph$Result[which(channel_morph$AnalyteName == "Glide")]))
  colnames(PCT_GL_data) <- c("id", "result")
  
  PCT_GL_sum <- tapply(PCT_GL_data$result, PCT_GL_data$id, sumna)
  PCT_GL.count <- tapply(PCT_GL_data$result, PCT_GL_data$id, lengthna)
  PCT_GL.result <- PCT_GL_sum/PCT_GL.count
  PCT_GL.sd <- tapply(PCT_GL_data$result, PCT_GL_data$id, sdna)
  
  ###PCT_POOL###
  
  PCT_POOL_data <- data.frame(cbind(channel_morph$id[which(channel_morph$AnalyteName == "Pool")]
                                    , channel_morph$Result[which(channel_morph$AnalyteName == "Pool")]))
  colnames(PCT_POOL_data) <- c("id", "result")
  
  PCT_POOL_sum <- tapply(PCT_POOL_data$result, PCT_POOL_data$id, sumna)
  PCT_POOL.count <- tapply(PCT_POOL_data$result, PCT_POOL_data$id, lengthna)
  PCT_POOL.result <- PCT_POOL_sum/PCT_POOL.count
  PCT_POOL.sd <- tapply(PCT_POOL_data$result, PCT_POOL_data$id, sdna)
  
  ###PCT_RA###
  PCT_RA_data <- data.frame(cbind(channel_morph$id[which(channel_morph$AnalyteName == "Rapid")], channel_morph$Result[which(channel_morph$AnalyteName == "Rapid")]))
  colnames(PCT_RA_data) <- c("id", "result")
  
  PCT_RA_sum <- tapply(PCT_RA_data$result, PCT_RA_data$id, sumna)
  PCT_RA.count <- tapply(PCT_RA_data$result, PCT_RA_data$id, lengthna)
  PCT_RA.result <- PCT_RA_sum/PCT_RA.count
  PCT_RA.sd <- tapply(PCT_RA_data$result, PCT_RA_data$id, sdna)
  
  ###PCT_RI###
  
  PCT_RI_data <- data.frame(cbind(channel_morph$id[which(channel_morph$AnalyteName == "Riffle")], channel_morph$Result[which(channel_morph$AnalyteName == "Riffle")]))
  colnames(PCT_RI_data) <- c("id", "result")
  
  PCT_RI_sum <- tapply(PCT_RI_data$result, PCT_RI_data$id, sumna)
  PCT_RI.count <- tapply(PCT_RI_data$result, PCT_RI_data$id, lengthna)
  PCT_RI.result <- PCT_RI_sum/PCT_RI.count
  PCT_RI.sd <- tapply(PCT_RI_data$result, PCT_RI_data$id, sdna)
  
  ###PCT_RN###
  
  PCT_RN_data <- data.frame(cbind(channel_morph$id[which(channel_morph$AnalyteName == "Run")], channel_morph$Result[which(channel_morph$AnalyteName == "Run")]))
  colnames(PCT_RN_data) <- c("id", "result")
  
  PCT_RN_sum <- tapply(PCT_RN_data$result, PCT_RN_data$id, sumna)
  PCT_RN.count <- tapply(PCT_RN_data$result, PCT_RN_data$id, lengthna)
  PCT_RN.result <- PCT_RN_sum/PCT_RN.count
  PCT_RN.sd <- tapply(PCT_RN_data$result, PCT_RN_data$id, sdna)
  
  ###PCT_FAST###
  
  PCT_FAST.result <- PCT_CF.result + PCT_RA.result + PCT_RI.result + PCT_RN.result
  
  ###PCT_SLOW###
  
  PCT_SLOW.result <- PCT_GL.result + PCT_POOL.result
  
  ###PCT_CF_WT###
  
  PCT_CF_WT1 <- (PCT_CF_sum + PCT_RA_sum + PCT_RI_sum + PCT_RN_sum + PCT_GL_sum + PCT_POOL_sum)/100
  PCT_CF_WT2 <- PCT_CF_sum*PCT_CF_WT1
  PCT_CF_WT.result <- PCT_CF_WT2/(PCT_CF.count*10)
  PCT_CF_WT.count<- PCT_CF.count
  
  ###PCT_GL_WT#
  
  PCT_GL_WT1 <- (PCT_CF_sum + PCT_RA_sum + PCT_RI_sum + PCT_RN_sum + PCT_GL_sum + PCT_POOL_sum)/100
  PCT_GL_WT2 <- PCT_GL_sum*PCT_GL_WT1
  PCT_GL_WT.result <- PCT_GL_WT2/(PCT_GL.count*10)
  PCT_GL_WT.count<- PCT_GL.count
  
  ###PCT_POOL_WT###
  
  PCT_POOL_WT1 <- (PCT_CF_sum + PCT_RA_sum + PCT_RI_sum + PCT_RN_sum + PCT_GL_sum + PCT_POOL_sum)/100
  PCT_POOL_WT2 <- PCT_POOL_sum*PCT_POOL_WT1
  PCT_POOL_WT.result <- PCT_POOL_WT2/(PCT_POOL.count*10)
  PCT_POOL_WT.count<- PCT_POOL.count
  
  ###PCT_RA_WT###
  
  PCT_RA_WT1 <- (PCT_CF_sum + PCT_RA_sum + PCT_RI_sum + PCT_RN_sum + PCT_GL_sum + PCT_POOL_sum)/100
  PCT_RA_WT2 <- PCT_RA_sum*PCT_RA_WT1
  PCT_RA_WT.result <- PCT_RA_WT2/(PCT_RA.count*10)
  PCT_RA_WT.count<- PCT_RA.count
  
  ###PCT_RI_WT###
  
  PCT_RI_WT1 <- (PCT_CF_sum + PCT_RA_sum + PCT_RI_sum + PCT_RN_sum + PCT_GL_sum + PCT_POOL_sum)/100
  PCT_RI_WT2 <- PCT_RI_sum*PCT_RI_WT1
  PCT_RI_WT.result <- PCT_RI_WT2/(PCT_RI.count*10)
  PCT_RI_WT.count<- PCT_RI.count
  
  ###PCT_RN_WT###
  
  PCT_RN_WT1 <- (PCT_CF_sum + PCT_RA_sum + PCT_RI_sum + PCT_RN_sum + PCT_GL_sum + PCT_POOL_sum)/100
  PCT_RN_WT2 <- PCT_RN_sum*PCT_RN_WT1
  PCT_RN_WT.result <- PCT_RN_WT2/(PCT_RN.count*10)
  PCT_RN_WT.count<- PCT_RN.count
  
  ###PCT_FAST_WT###
  
  PCT_FAST_WT.result <- PCT_CF_WT.result + PCT_RA_WT.result + PCT_RI_WT.result + PCT_RN_WT.result
  
  ###PCT_SLOW_WT###
  
  PCT_SLOW_WT.result <- PCT_GL_WT.result + PCT_POOL_WT.result
  
  
  ###Write to file###
  results <- cbind(PCT_CF.result, PCT_CF.count, PCT_CF.sd, PCT_DR.result, PCT_DR.count, PCT_DR.sd, PCT_GL.result,
                   PCT_GL.count, PCT_GL.sd, PCT_POOL.result, PCT_POOL.count, PCT_POOL.sd, PCT_RA.result, PCT_RA.count,
                   PCT_RA.sd, PCT_RI.result, PCT_RI.count, PCT_RI.sd, PCT_RN.result, PCT_RN.count, PCT_RN.sd,
                   PCT_FAST.result, PCT_SLOW.result, PCT_CF_WT.result, PCT_CF_WT.count, PCT_GL_WT.result, PCT_GL_WT.count,
                   PCT_POOL_WT.result, PCT_POOL_WT.count, PCT_RA_WT.result, PCT_RA_WT.count, PCT_RI_WT.result, 
                   PCT_RI_WT.count, PCT_RN_WT.result, PCT_RN_WT.count, PCT_FAST_WT.result, PCT_SLOW_WT.result)
  print("channelmorph done")
  result
}

channelsinuosity <- function (data) {
  data <- data[which(data$AnalyteName %in% c('Slope', 'length, Segment', 'Elevation Difference', 'Bearing', 'Proportion', 'Length Reach')),]
  ###XSLOPE###
  data$Result[data$Result==-88] <- NA
  library(reshape)
  casted <- cast(data, id + FractionName +LocationCode ~ AnalyteName, value = "Result", fun.aggregate=NULL)
  casted <- as.data.frame(casted)
  colnames(casted)[colnames(casted)=="Length, Segment"] <- "Segment"
  casted$index <- as.factor(1:length(casted$id))
  if(is.null(casted$Slope))casted$Slope <- rep(NA, length(casted$id))
  if(is.null(casted$"Elevation Difference"))casted$Elevation <- rep(NA, length(casted$id))
  
  convert <- function(i, Slope, Elevation, Segment){
    if(!is.na(Slope[i])){Slope[i]} else
      if(!is.na(Elevation[i])){Elevation[i]/Segment[i]} else
        if(is.na(Slope[i])&is.na(Elevation[i])){NA}}
  
  print(length(casted$index))
  print(length(casted$Slope))
  print(length(casted$Elevation))
  print(length(casted$Segment))

  casted$convert <- unlist(lapply(X=casted$index, FUN=convert, Slope=casted$Slope, Elevation=casted$Elevation, Segment=casted$Segment ))
  casted$convert <- casted$convert * ((casted$Proportion)/100)
  
  sumna <- function(data)sum(as.numeric(as.character(data)), na.rm = T) 
  sdna <- function(data)sd(data, na.rm = T)
  lengthna <- function(data)sum(!is.na(data))
  
  XSLOPE_sum <- tapply(casted$convert, casted$id, sumna)
  XSLOPE.count <- tapply(casted$convert, casted$id, lengthna)
  XSLOPE.result <- XSLOPE_sum/XSLOPE.count
  XSLOPE.sd <- tapply(casted$convert, casted$id, sdna)
  
  
  ###Make segment/proportion column###
  
  casted$segpro <- (casted$Segment/casted$Proportion)*100
  casted$convert[is.na(casted$convert)] <- -88
  ###SLOPE_0###
  casted$sumna5 <- lapply(1:length(casted$id), function(i, d, p)if(casted$convert[i] == 0){d[i]}else{NA}, d=casted$segpro, p=casted$convert)
  SLOPE_0_sum <- tapply(casted$sumna5, casted$id, sumna)
  counts <- tapply(casted$segpro, casted$id, lengthna)
  
  SLOPE_0_reach <- as.array(data$Result[data$AnalyteName=="Length Reach"])
  dimnames(SLOPE_0_reach) <- list(data$id[data$AnalyteName=="Length Reach"])
  
  reach <- array(rep(NA), length(SLOPE_0_sum))
  dimnames(reach) <- dimnames(SLOPE_0_sum)
  reach[which(unlist(dimnames(reach)) %in% 
    unlist(dimnames(SLOPE_0_reach)))] <- SLOPE_0_reach
  
  
  missing <- names(which(is.na(reach)))
  
  alternate <- tapply(casted[casted$id %in% missing, "Segment"], 
                      casted[casted$id %in% missing, "id"], sumna)
  
  SLOPE_0.notes <- array(rep(NA), length(SLOPE_0_sum))
  dimnames(SLOPE_0.notes) <- dimnames(SLOPE_0_sum)
  SLOPE_0.notes[which(unlist(dimnames(SLOPE_0.notes)) %in% 
    unlist(dimnames(SLOPE_0_reach)))] <- "Reach length pulled from analyte 'Reach, length'"
  SLOPE_0.notes[which(unlist(dimnames(SLOPE_0.notes)) %in% 
    unlist(dimnames(alternate)))] <- "Reach length calculated from sum of segments"
  
  reach[which(unlist(dimnames(reach)) %in% 
    unlist(dimnames(alternate)))] <- alternate
  
  
  SLOPE_0.result <- (SLOPE_0_sum/unlist(reach))*100
  SLOPE_0.count <-counts
  
  ###SLOPE_0_5###
  casted$sumna5 <- lapply(1:length(casted$id), function(i, d, p)if(casted$convert[i] > 0 && 0.5 >= casted$convert[i]){d[i]}else{NA}, d=casted$segpro, p=casted$convert)
  SLOPE_0_5_sum <- tapply(casted$sumna5, casted$id, sumna)
  SLOPE_0_5.result <- (SLOPE_0_5_sum/reach)*100
  SLOPE_0_5.count <-counts
  
  ###SLOPE_1
  casted$sumna1 <- lapply(1:length(casted$id), function(i, d, p)if(casted$convert[i] > 0 && 1 >= casted$convert[i]){d[i]}else{NA}, d=casted$segpro, p=casted$convert)
  SLOPE_1_sum <- tapply(casted$sumna1, casted$id, sumna)
  SLOPE_1.result <- (SLOPE_1_sum/reach)*100
  SLOPE_1.count <-counts
  
  ###SLOPE_2
  casted$sumna2 <- lapply(1:length(casted$id), function(i, d, p)if(casted$convert[i] > 0 && 2 >= casted$convert[i]){d[i]}else{NA}, d=casted$segpro, p=casted$convert)
  SLOPE_2_sum <- tapply(casted$sumna2, casted$id, sumna)
  SLOPE_2.result <- (SLOPE_2_sum/reach)*100
  SLOPE_2.count <-counts
  
  ###XBEARING###
  
  casted$XBEARING <- casted$Bearing * (casted$Proportion/100)
  XBEARING_sum <- tapply(casted$XBEARING, casted$id, sumna)
  XBEARING.count <- tapply(casted$XBEARING, casted$id, lengthna)
  XBEARING.result <- XBEARING_sum/XBEARING.count
  XBEARING.sd <- tapply(casted$XBEARING, casted$id, sdna)
  
  ###SINU###
  
  cos1 <- function(i, Segment, Bearing){(Segment[i] * cos((Bearing[i]/360)*2*pi))}
  sin1 <- function(i, Segment, Bearing){(Segment[i] * sin((Bearing[i]/360)*2*pi))}
  
  #cast(data, id + FractionName ~ AnalyteName, value = "Result", fun.aggregate=mean)
  casted$cos <- unlist(lapply(1:length(casted$Segment), Segment=casted$Segment, FUN=cos1, Bearing = casted$Bearing))
  casted$sin <- unlist(lapply(1:length(casted$Segment), Segment=casted$Segment, FUN=sin1, Bearing = casted$Bearing))
  
  
  distance <- tapply(casted$Segment, casted$id, sumna)
  sin2 <- tapply(casted$sin, casted$id, sumna)
  cos2 <- tapply(casted$cos, casted$id, sumna)
  casted$ttt <- unlist(lapply(1:length(casted$id), function(i, si, co) 2*si[i], si=casted$sin, co=casted$cos))
  SINU.NOT_WORKING <- distance/(tapply(casted$ttt, casted$id, sumna))
  
  ###Write to file###
  
  result <- cbind(XSLOPE.result, XSLOPE.count, XSLOPE.sd, SLOPE_0.result, SLOPE_0.count, SLOPE_0.notes, SLOPE_0_5.result,
                  SLOPE_0_5.count, SLOPE_1.result, SLOPE_1.count, SLOPE_2.result, SLOPE_2.count, XBEARING.result, XBEARING.count, XBEARING.sd, SINU.NOT_WORKING)
  print("channelsine done")
  result
}

densiometer <- function (Densiometer) {
  Densiometer <- Densiometer[which(Densiometer$AnalyteName %in% c('Canopy Cover')),]
  
  x <- as.character(Densiometer$LocationCode)
  split <- data.frame(do.call('rbind',strsplit(x, ",")))
  colnames(split) <- c("trans", "view")
  Densiometer <- cbind(Densiometer, split)
  
  ###Calculate XCDENBK###
  
  a <- c(which(Densiometer$view == " LeftViewLeft"), which(Densiometer$view == " RightViewRight"))
  XCDENBK_data <- data.frame(cbind(Densiometer$id[a], (Densiometer$Result[a])))
  colnames(XCDENBK_data) <- c("id", "Result")
  transform <- function(data) as.numeric(as.character(data))*(100/17)
  XCDENBK_data$trans <- 1:length(XCDENBK_data$Result)
  XCDENBK_data$trans <- (transform(XCDENBK_data$Result))
  sumna <- function(data)sum(data, na.rm = T)
  XCDENBK_sum <- tapply(XCDENBK_data$trans, XCDENBK_data$id, sumna)
  lengthna <- function(data)sum(!is.na(data))
  XCDENBK.count <- tapply(XCDENBK_data$trans, XCDENBK_data$id, lengthna)
  XCDENBK.result <- XCDENBK_sum/XCDENBK.count
  sdna <- function(data)sd(data, na.rm = T)
  XCDENBK.sd <- tapply(XCDENBK_data$trans, XCDENBK_data$id, sdna)
  
  ###Calculate XCDENMID###
  b <- which(1:length(Densiometer$view) != a)
  XCDENMID_data <- data.frame(cbind(Densiometer$id[b], (Densiometer$Result[b])))
  colnames(XCDENMID_data) <- c("id", "Result")
  XCDENMID_data$trans <- 1:length(XCDENMID_data$Result)
  XCDENMID_data$trans <- (transform(XCDENMID_data$Result))
  XCDENMID_sum <- tapply(XCDENMID_data$trans, XCDENMID_data$id, sumna)
  XCDENMID.count <- tapply(XCDENMID_data$trans, XCDENMID_data$id, lengthna)
  XCDENMID.result <- XCDENMID_sum/XCDENMID.count
  XCDENMID.sd <- tapply(XCDENMID_data$trans, XCDENMID_data$id, sdna)
  
  
  ###Write to file###
  results <- cbind(XCDENMID.result, XCDENMID.count, XCDENMID.sd, XCDENBK.result, XCDENBK.count, XCDENBK.sd)
  print("Densiometer done")
  results
}

flow <- function (data) {
  data <- data[which(data$AnalyteName %in% c('Distance from Bank', 'StationWaterDepth', 'velocity', 'Distance, Float', 'Float time', 'wetted width')),]
  velocity<-data
  flow<-data
  neutral<-data
  library(reshape)
  test <-velocity[, c("StationCode", "SampleDate", "Replicate", "AnalyteName", "Result",  "ResQualCode", "QACode")]
  test2 <- flow[, c("StationCode", "SampleDate", "Replicate", "AnalyteName", "Result",  "ResQualCode", "QACode")]
  
  tempp <- (rbind(test, test2))
  tempp$id <- do.call(paste, c(tempp[c("StationCode", "SampleDate")]))
  vmethod <- (cast(tempp, id + Replicate ~ AnalyteName, value="Result", fun.aggregate=mean))
  
  
  ###Calculate Velocity Area Method###
  vmethod$flow <- rep(NA, length(vmethod$id))
  
  vmethod$flow <-c(NA, unlist(lapply(2:length(vmethod$id), FUN=function(i, d, v, s){((d[i]-d[i-1]))*s[i]*v[i]*0.00107639104},
                                     d=vmethod$"Distance from Bank", s=vmethod$StationWaterDepth, v=vmethod$Velocity)))
  sumna <- function(data){sum(as.numeric(as.character(data)), na.rm = T)}
  FL_Q_F<-tapply(vmethod$flow, vmethod$id, sumna)
  FL_Q_F[which(FL_Q_F==0)] <-NA
  FL_Q_F[which(FL_Q_F<0)] <-0
  FL_Q_M <- FL_Q_F*0.0283168466
  
  ###Format Data Frame###
  neutral$Result[neutral$ResQualCode=="NR"] <- NA
  neutral$id<- do.call(paste, c(neutral[c("StationCode", "SampleDate")]))
  neutral$Location <- rep(NA, length(neutral$id))
  neutral$Location[grep("Upper Section Point", neutral$LocationCode)] <- "Upper"
  neutral$Location[grep("Middle Section Point", neutral$LocationCode)] <- "Middle"
  neutral$Location[grep("Lower Section Point", neutral$LocationCode)] <- "Lower"
  neutral$Location[grep("Float Reach TransUpper", neutral$LocationCode)] <- "Upper"
  neutral$Location[grep("Float Reach TransMiddle, Point", neutral$LocationCode)] <- "Middle"
  neutral$Location[grep("Float Reach TransLower", neutral$LocationCode)] <- "Lower"
  neutral$Location[intersect(grep("Lower Section", neutral$LocationCode), (which(neutral$AnalyteName=="Wetted Width")))] <- "L"
  neutral$Location[intersect(grep("Middle Section", neutral$LocationCode), (which(neutral$AnalyteName=="Wetted Width")))] <- "M"
  neutral$Location[intersect(grep("Upper Section", neutral$LocationCode), (which(neutral$AnalyteName=="Wetted Width")))] <- "U"
  neutral$Location[intersect(grep("Float Reach TransLower", neutral$LocationCode), (which(neutral$AnalyteName=="Wetted Width")))] <- "L"
  neutral$Location[intersect(grep("Float Reach TransMiddle", neutral$LocationCode), (which(neutral$AnalyteName=="Wetted Width")))] <- "M"
  neutral$Location[intersect(grep("Float Reach TransUpper", neutral$LocationCode), (which(neutral$AnalyteName=="Wetted Width")))] <- "U"
  
  ###Calculate neutral buoyant method###
  ncast <- cast(neutral[neutral$LocationCode != "Float Reach",], id~Location, value="Result", fun.aggregate=mean)
  ncast$r <- rep(NA, length(ncast$id))
  ncast$r <- lapply(1:length(ncast$id), function(i, l, lower, m, middle, u, upper){
    mean((l[i]*lower[i]),(m[i]*middle[i]),(u[i]*upper[i]), na.rm=T, trim=0)/100},
                    l=ncast$L, lower=ncast$Lower, m=ncast$M, middle=ncast$Middle, u=ncast$U, upper=ncast$Upper)
  narea <- tapply(ncast$r, ncast$id, invisible)
  
  ncast2 <- cast(neutral[neutral$LocationCode == "Float Reach",], id~ AnalyteName + Replicate, value="Result", fun.aggregate=NULL)
  ncast2$r <- rep(NA, length(ncast2$id))
  ncast2$r <- lapply(1:length(ncast2$id), function(i, d1, d2, d3, t1, t2, t3){
    mean((d1[i]/t1[i]),(d2[i]/t2[i]),(d3[i]/t3[i]))},
                     d1=ncast2$"Distance, Float_1", d2=ncast2$"Distance, Float_2", d3=ncast2$"Distance, Float_3",
                     t1=ncast2$"Float Time_1", t2=ncast2$"Float Time_2", t3=ncast2$"Float Time_3")
  nspeed <- tapply(ncast2$r, ncast2$id, invisible)
  FL_N_M <- as.numeric(narea)*as.numeric(nspeed[which(names(nspeed) %in% names(narea))])
  names(FL_N_M) <- names(narea)
  FL_N_F <- FL_N_M*0.0283168466
  
  ###Format Results###
  result<-as.data.frame(matrix(NA, ncol=11, nrow=length(union(names(FL_N_M), names(FL_Q_M)))))
  rownames(result)<-union(names(FL_N_M), names(FL_Q_M))
  
  
  result[which(rownames(result)%in%names(FL_N_F)), 1]<-FL_N_F
  result[which(rownames(result)%in%names(FL_N_M)), 2]<-FL_N_M
  result[which(rownames(result)%in%names(FL_Q_F)), 3]<-FL_Q_F
  result[which(rownames(result)%in%names(FL_Q_M)), 4]<-FL_Q_M
  colnames(result)<-c("FL_N_F.result",  "FL_N_M.result",  "FL_Q_F.result",  "FL_Q_M.result",  "FL_F.result",  "FL_M.result")
  
  result$FL_F.result <- unlist(lapply(1:length(result$FL_N_F), FUN=function(i, a, b){
    mean(c(a[i], b[i]), na.rm=T)}, a=result$FL_N_F, b=result$FL_Q_F))
  result$FL_M.result <- unlist(lapply(1:length(result$FL_N_F), FUN=function(i, a, b){
    mean(c(a[i], b[i]), na.rm=T)}, a=result$FL_N_M, b=result$FL_Q_M))
  
  ###Mean and max water velocity###
  velocity_Q <- tapply(tempp[tempp$AnalyteName == "Velocity", "Result"], tempp[tempp$AnalyteName == "Velocity", "id"], max)
  velocity_N <- lapply(1:length(ncast$id), function(i, d1, d2, d3, t1, t2, t3){
    max((d1[i]/t1[i]),(d2[i]/t2[i]),(d3[i]/t3[i]))},
                       d1=ncast2$"Distance, Float_1", d2=ncast2$"Distance, Float_2", d3=ncast2$"Distance, Float_3",
                       t1=ncast2$"Float Time_1", t2=ncast2$"Float Time_2", t3=ncast2$"Float Time_3")
  velocity_N <- tapply(velocity_N, ncast2$id, invisible)
  result[which(rownames(result)%in%names(velocity_Q)), 7] <- velocity_Q
  result[which(rownames(result)%in%names(velocity_N)), 7] <- unlist(velocity_N)
  result[[8]]<- result[[7]]/3.2808399
  
  velocity_QM <- tapply(tempp[tempp$AnalyteName == "Velocity", "Result"], tempp[tempp$AnalyteName == "Velocity", "id"], mean)
  result[which(rownames(result)%in%names(velocity_QM)), 9] <- velocity_QM
  result[which(rownames(result)%in%names(nspeed)), 9] <- unlist(nspeed)
  result[[10]]<-result[[9]]/3.2808399
  
  ###Percent zero velocity###
  zerov1 <- tapply(tempp[tempp$AnalyteName == "Velocity", "Result"], tempp[tempp$AnalyteName == "Velocity", "id"], 
                   function(data)sum(data==0))
  lengthna <- function(data){sum(!is.na(data))}
  zerov<- (zerov1/tapply(tempp[tempp$AnalyteName == "Velocity", "Result"], tempp[tempp$AnalyteName == "Velocity", "id"],
                         lengthna))*100
  zeron1<- (lapply(1:length(ncast$id), function(i, d1, d2, d3, t1, t2, t3){
    sum(((d1[i]/t1[i])==0),((d2[i]/t2[i])==0),((d3[i]/t3[i])==0))},
                   d1=ncast2$"Distance, Float_1", d2=ncast2$"Distance, Float_2", d3=ncast2$"Distance, Float_3",
                   t1=ncast2$"Float Time_1", t2=ncast2$"Float Time_2", t3=ncast2$"Float Time_3"))
  zeron <- tapply(lapply(zeron1, function(d)d/3), ncast2$id, invisible)
  result[which(rownames(result)%in%names(zerov)), 11]<-zerov
  result[which(rownames(result)%in%names(zeron)), 11]<-unlist(zeron)
  colnames(result)[7:11] <-c("MWVM_F.result", "MWVM_M.result", "XWV_F.result", "XWV_M.result", "PWVZ.result")
  print("flow done")
  results
}

habitat <- function (habitat) {
  habitat <- habitat[which(habitat$AnalyteName %in% c('Fish Cover Macrophytes', 'Fish Cover Artificial Structures', 'Fish Cover Boulders', 'Fish Cover Filamentous Algae', 'Fish Cover Woody Debris >0.3 m', 'Fish Cover Live Trees/Roots', 'Fish Cover Overhang.Veg', 'Fish Cover Woody Debris <0.3 m', 'Fish Cover Undercut Banks')),]
  habitat$id <- do.call(paste, c(habitat[c("StationCode", "SampleDate")]))
  
  habitat$convert <- rep(NA, length(habitat$StationCode))
  
  habitat$convert<-gsub("1", "5",habitat$VariableResult)
  habitat$convert<-gsub("2", "25",habitat$convert)
  habitat$convert<-gsub("3", "57.5",habitat$convert)
  habitat$convert<-gsub("4", "87.5",habitat$convert)
  
  ###Compute Stats
  analytes <- c("Fish Cover Macrophytes", "Fish Cover Artificial Structures", "Fish Cover Boulders",
                "Fish Cover Filamentous Algae", "Fish Cover Woody Debris >0.3 m", 
                "Fish Cover Live Trees/Roots", "Fish Cover Overhang.Veg", "Fish Cover Woody Debris <0.3 m",
                "Fish Cover Undercut Banks")
  statname <- c("XFC_AQM",  "XFC_HUM",  "XFC_RCK",  "XFC_ALG",	"XFC_LWD",
                "XFC_LTR",	"XFC_OHV",	"XFC_BRS",	"XFC_UCB",	
                "XFC_BIG.result",	"XFC_NAT_EMAP.result",	"XFC_NAT_SWAMP.result",	
                "CFC_AQM.result",	"CFC_HUM.result",	"CFC_RCK.result",	"CFC_ALG.result",	"CFC_LWD.result",
                "CFC_LTR.result",	"CFC_OHV.result",	"CFC_BRS.result",	"CFC_UCB.result",	
                "CFC_ALL_EMAP.result",	"CFC_ALL_SWAMP.result")
  
  result <- matrix(NA, nrow=length(unique(habitat$id)), 
                   ncol=((length(statname[1:9])*3)+length(statname[10:23])))
  result <- as.data.frame(result)
  rownames(result) <- unique(habitat$id)
  snames <- paste(rep(statname[1:9], each=3), c(".result", ".count", ".sd"), sep="")
  colnames(result) <- c(snames, statname[10:23])
  
  sumna <- function(data){sum(data, na.rm=T)}
  lengthna <- function(data){sum(!is.na(data))}
  
  habitat$convert <- as.numeric(habitat$convert)
  
  for(i in 1:9){
    analyte <- subset(habitat, habitat$AnalyteName == analytes[i])
    analytesum <- tapply(analyte$convert, analyte$id, sumna)
    analytetotal <- tapply(analyte$convert, analyte$id, lengthna)
    analytemean <- analytesum/analytetotal
    analytesd <- tapply(analyte$convert, analyte$id, sd)
    result[[((i-1)*3)+1]] <- analytemean
    result[[((i-1)*3)+2]] <- analytetotal
    result[[((i-1)*3)+3]] <- analytesd
  }
  
  result$XFC_BIG.result <-  result$XFC_LWD.result + result$XFC_RCK.result + 
    result$XFC_UCB.result + result$XFC_HUM.result
  
  result$XFC_NAT_EMAP.result <- result$XFC_LWD.result + result$XFC_BRS.result +
    result$XFC_OHV.result + result$XFC_RCK.result + result$XFC_UCB.result
  
  result$XFC_NAT_SWAMP.result <- result$XFC_LWD.result + result$XFC_BRS.result +
    result$XFC_OHV.result + result$XFC_RCK.result + result$XFC_UCB.result +
    result$XFC_LTR.result + result$XFC_AQM.result
  
  habitat$present <- as.numeric(as.character(habitat$VariableResult)) >= 1
  
  for(i in 1:9){
    analyte <- subset(habitat, habitat$AnalyteName == analytes[i])
    analytesum <- tapply(analyte$present, analyte$id, sumna)
    result[[i+30]] <- analytesum
  }
  
  t <- result[,c("XFC_AQM.result", "XFC_HUM.result", "XFC_RCK.result", "XFC_ALG.result", "XFC_LWD.result", "XFC_OHV.result", "XFC_BRS.result", "XFC_UCB.result")]
  result$CFC_ALL_EMAP.result <- rowSums((t>0), na.rm=T)
  
  q <- result[,c("XFC_LTR.result", "XFC_AQM.result", "XFC_HUM.result", "XFC_RCK.result", "XFC_ALG.result", "XFC_LWD.result", "XFC_OHV.result", "XFC_BRS.result", "XFC_UCB.result")]
  result$CFC_ALL_SWAMP.result <- rowSums((q>0), na.rm=T)
  print("habitat done")
  results
}

disturbance <- function (data) {
  data <- data[which(data$AnalyteName %in% c('Riparian Bridges/Abutments', 'Riparian Buildings', 'Riparian Landfill/Trash', 'Riparian Logging', 'Riparian Mining', 'Riparian Orchards/Vineyards', 'Riparian Park/Lawn', 'Riparian Pasture/Range', 'Riparian Pavement', 'Riparian Pipes', 'Riparian Road', 'Riparian Row Crops', 'Riparian Vegetation Management', 'Riparian Wall/Dike')),]
  ###Set up###
  library(reshape)
  data2 <- gsub("(Block|Channel)(\\w)", "\\2", data$LocationCode)
  data2 <- gsub(",\\sLeft", "", data2)
  data2 <- gsub(",\\sRight", "", data2)
  
  reformed <- data.frame(data$id, gsub("(Block|Channel)(\\w)", "\\1", data$LocationCode), data2,
                         data$AnalyteName, data$VariableResult)
  colnames(reformed) <- c("id", "Location", "Trans", "AnalyteName", "VariableResult")
  reformed$VariableResult <- as.character(reformed$VariableResult)
  reformed$VariableResult[reformed$VariableResult=="B"] <- "1.5"
  reformed$VariableResult[reformed$VariableResult=="P"] <- "0.667"
  reformed$VariableResult[reformed$VariableResult=="C"] <- "1"
  reformed$VariableResult[reformed$VariableResult=="N"] <- "100"
  reformed$VariableResult[reformed$VariableResult=="Y"] <- "200"
  reformed$VariableResult <- as.numeric(reformed$VariableResult)
  library(reshape)
  casting <- function(reformed){
    casted <- as.data.frame(cast(reformed, 
                                 id+Trans ~ Location, value = "VariableResult", fun.aggregate=NULL))
    if(is.null(casted$channel)==T){casted[[5]] <- rep(100, times=length(casted$id))}
    colnames(casted) <- c("id", "Trans", "Left", "Right", "Channel")
    casted$sums <- rep(NA, length(casted$Channel))
    for(i in 1:length(casted$Channel)){
      a <- casted$Left[i]
      b <- casted$Right[i]
      if(casted$Channel[i]==200){casted$sums[i]=3} else{
        if(casted$Channel[i]==100){casted$sums[i] <- (a+b)}
      }
    }
    return(casted)
  }
  
  sumna <- function(data){sum(data, na.rm=T)}
  lengthna <- function(data){sum(!is.na(data))}
  
  ###Create the data frame###
  analytes <- unique(as.character(reformed$AnalyteName))
  statname <- c("W1H_BRDG", "W1H_BLDG", "W1H_LDFL", "W1H_LOG", "W1H_MINE", "W1H_ORVY", "W1H_PARK",
                "W1H_PSTR", "W1H_PVMT", "W1H_PIPE", "W1H_ROAD", "W1H_CROP", "W1H_VEGM", "W1H_WALL")
  
  reformed$AnalyteName <- as.character(reformed$AnalyteName)
  
  result <- matrix(NA, nrow=length(unique(reformed$id)), ncol=(length(statname)*3))
  result <- as.data.frame(result)
  rownames(result) <- unique(reformed$id)
  colnames(result) <- paste(rep(statname, each=3), c(".result", ".count", ".sd"), sep="")
  
  
  ###Compute Stats###
  for(i in 1:length(analytes)){
    analyte <- casting(subset(reformed, reformed$AnalyteName == analytes[i]))
    analytesum <- tapply(analyte$sum, analyte$id, sumna)
    analytetotal <- tapply(analyte$sum, analyte$id, lengthna)
    analytemean <- analytesum/(2*analytetotal)
    analytesd <- tapply(analyte$sum, analyte$id, sd)
    result[[((i-1)*3)+1]] <- analytemean
    result[[((i-1)*3)+2]] <- analytetotal
    result[[((i-1)*3)+3]] <- analytesd
  }
  
  W1_HALL_EMAP.result <- result$W1H_BLDG.result + result$W1H_LDFL.result + result$W1H_LOG.result + result$ W1H_MINE.result 
  + result$W1H_PARK.result + result$W1H_PSTR.result + result$W1H_PVMT.result + result$W1H_PIPE.result + 
    result$W1H_ROAD.result + result$W1H_CROP.result + result$W1H_WALL.result
  
  W1_HALL_SWAMP.result <- result$W1H_BLDG.result + result$W1H_LDFL.result + result$W1H_LOG.result + result$ W1H_MINE.result 
  + result$W1H_PARK.result + result$W1H_PSTR.result + result$W1H_PVMT.result + result$W1H_PIPE.result + 
    result$W1H_ROAD.result + result$W1H_CROP.result + result$W1H_WALL.result + result$ W1H_BRDG.result +
    result$W1H_ORVY.result + result$W1H_VEGM.result
  
  results <- cbind(result, W1_HALL_EMAP.result, W1_HALL_SWAMP.result)
  print("disturbance done")
  results
}

misc <- function (misc) {
  misc <- misc[which(misc$AnalyteName %in% c('Riffle/Run Channel Alteration', 'Riffle/Run Epifaunal Substrate', 'Riffle/Run Sediment Deposition', 'Dominant Land Use', 'Evidence of Fire', 'Evidence of Recent Rainfall')),]
  ###Report###
  misc$VariableResult[misc$ResQualCode=="NR"] <- NA
  misc$VariableResult <- as.character(misc$VariableResult)
  misc$id <- do.call(paste, c(misc[c("StationCode", "SampleDate")]))
  NFC_DLU <- tapply(misc$VariableResult[misc$AnalyteName=="Dominant Land Use"],
                    misc$id[misc$AnalyteName=="Dominant Land Use"], invisible)
  NFC_EFR <- tapply(misc$VariableResult[misc$AnalyteName=="Evidence of Fire"],
                    misc$id[misc$AnalyteName=="Evidence of Fire"], invisible)
  NFC_ERN <- tapply(misc$VariableResult[misc$AnalyteName=="Evidence of Recent Rainfall"],
                    misc$id[misc$AnalyteName=="Evidence of Recent Rainfall"], invisible)
  RBP_CHN <- tapply(misc$VariableResult[misc$AnalyteName=="Riffle/Run Channel Alteration"],
                    misc$id[misc$AnalyteName=="Riffle/Run Channel Alteration"], invisible)
  RBP_EPI <- tapply(misc$VariableResult[misc$AnalyteName=="Riffle/Run Epifaunal Substrate"],
                    misc$id[misc$AnalyteName=="Riffle/Run Epifaunal Substrate"], invisible)
  RBP_SED <- tapply(misc$VariableResult[misc$AnalyteName=="Riffle/Run Sediment Deposition"],
                    misc$id[misc$AnalyteName=="Riffle/Run Sediment Deposition"], invisible)
  
  misc_metrics <- as.data.frame(matrix(NA, nrow=length(unique(misc$id)), ncol=6))
  rownames(misc_metrics)<- unique(misc$id)
  misc_metrics[which(rownames(misc_metrics)%in%names(NFC_DLU)), 1]<-NFC_DLU
  misc_metrics[which(rownames(misc_metrics)%in%names(NFC_EFR)), 2]<-NFC_EFR
  misc_metrics[which(rownames(misc_metrics)%in%names(NFC_ERN)), 3]<-NFC_ERN
  misc_metrics[which(rownames(misc_metrics)%in%names(RBP_CHN)), 4]<-RBP_CHN
  misc_metrics[which(rownames(misc_metrics)%in%names(RBP_EPI)), 5]<-RBP_EPI
  misc_metrics[which(rownames(misc_metrics)%in%names(RBP_SED)), 6]<-RBP_SED
  colnames(misc_metrics)<- c("NFC_DLU.result", "NFC_EFR.result", "NFC_ERN.result",
                             "RBP_CHN.result", "RBP_EPI.result", "RBP_SED.result")
  print("misc done")
  misc
}

Bankstability <- function (Bank_stability) {
  Bank_stability <- Bank_stability[which(Bank_stability$AnalyteName %in% c('Bank Stability')),]
  ###PBM_S###
  
  stable <- function(data){
    length(which(data == "stable"))
  }
  total <- function(data){length(c(which(data == "stable"), 
                                   which(data == "vulnerable"), which(data == "eroded")))}
  PBM_S_sum <- tapply(Bank_stability$VariableResult, Bank_stability$id, stable)
  total_obs <- tapply(Bank_stability$VariableResult, Bank_stability$id, total)
  PBM_S.result <- (PBM_S_sum/total_obs)*100
  
  ###PBM_V###
  vulnerable <- function(data){
    length(which(data == "vulnerable"))
  }
  PBM_V_sum <- tapply(Bank_stability$VariableResult, Bank_stability$id, vulnerable)
  PBM_V.result <- (PBM_V_sum/total_obs)*100
  
  ###PBM_E###
  eroded <- function(data){
    length(which(data == "eroded"))
  }
  PBM_E_sum <- tapply(Bank_stability$VariableResult, Bank_stability$id, eroded)
  PBM_E.result <- (PBM_E_sum/total_obs)*100
  
  ###Write to file###
  results <- cbind(PBM_S.result, PBM_V.result, PBM_E.result)
  print("bank stabaility done")
  results
}

quality_metrics <- function (quality) {
  quality <- quality[which(quality$AnalyteName %in% c("Alkalinity as CaCO3", "Oxygen, Dissolved",
                                                      "pH", "Salinity", "SpecificConductivity",
                                                      "Temperature", "Turbidity")),]
  XWAK <- tapply(quality$Result[quality$AnalyteName=="Alkalinity as CaCO3"], 
                 quality$id[quality$AnalyteName=="Alkalinity as CaCO3"], mean)
  XWDO <- tapply(quality$Result[quality$AnalyteName=="Oxygen, Dissolved"], 
                 quality$id[quality$AnalyteName=="Oxygen, Dissolved"], mean)
  XWPH <- tapply(quality$Result[quality$AnalyteName=="pH"], 
                 quality$id[quality$AnalyteName=="pH"], mean)
  XWSL <-  tapply(quality$Result[quality$AnalyteName=="Salinity"], 
                  quality$id[quality$AnalyteName=="Salinity"], mean)
  XWSC <- tapply(quality$Result[quality$AnalyteName=="SpecificConductivity"], 
                 quality$id[quality$AnalyteName=="SpecificConductivity"], mean)
  XWTC <- tapply(quality$Result[quality$AnalyteName=="Temperature"], 
                 quality$id[quality$AnalyteName=="Temperature"], mean)
  XWTF <- tapply(quality$Result[quality$AnalyteName=="Temperature"], 
                 quality$id[quality$AnalyteName=="Temperature"], function(d)(mean(d)*(9/5))+32)
  XWTB <- tapply(quality$Result[quality$AnalyteName=="Turbidity"], 
                 quality$id[quality$AnalyteName=="Turbidity"], mean)
  
  quality_metrics <- as.data.frame(matrix(NA, nrow=length(unique(quality$id)), ncol=8))
  rownames(quality_metrics)<- unique(quality$id)
  quality_metrics[which(rownames(quality_metrics)%in%names(XWAK)), 1]<-XWAK
  quality_metrics[which(rownames(quality_metrics)%in%names(XWDO)), 2]<-XWDO
  quality_metrics[which(rownames(quality_metrics)%in%names(XWPH)), 3]<-XWPH
  quality_metrics[which(rownames(quality_metrics)%in%names(XWSL)), 4]<-XWSL
  quality_metrics[which(rownames(quality_metrics)%in%names(XWSC)), 5]<-XWSC
  quality_metrics[which(rownames(quality_metrics)%in%names(XWTC)), 6]<-XWTC
  quality_metrics[which(rownames(quality_metrics)%in%names(XWTF)), 7]<-XWTF
  quality_metrics[which(rownames(quality_metrics)%in%names(XWTB)), 8]<-XWTB
  colnames(quality_metrics)<- c("XWAK.result", "XWDO.result", "XWPH.result", "XWSL.result", "XWSC.result",
                                "XWTC.result", "XWTF.result", "XWTB.result")
  print("quality done")
  quality_metrics
}

ripveg <- function (ripveg) {
  ripveg <- ripveg[which(ripveg$AnalyteName %in% c('Riparian GroundCover Barren', 'Riparian GroundCover NonWoody Plants', 'Riparian GroundCover Woody Shrubs', 'Riparian Lower Canopy All Vegetation', 'Riparian Upper Canopy All Trees', 'Riparian Lower Canopy All Vegetation', 'Riparian Upper Canopy All Trees', 'Riparian GroundCover Woody Shrubs', 'Riparian GroundCover NonWoody Plants')),]
  ###Slice Riparian GroundCover Barren Data ###
  
  barrenindex <- which(ripveg$AnalyteName == "Riparian GroundCover Barren")
  barren <- data.frame(cbind(ripveg$id[barrenindex], as.character(ripveg$AnalyteName[barrenindex]),
                             as.character(ripveg$VariableResult[barrenindex])))
  colnames(barren) <- c("id", "AnalyteName", "result")
  barren$result <- as.numeric(as.character(barren$result))
  
  ###Compute XGB###
  
  for(i in (1:length(barren$result))[which(!is.na(barren$result))]){
    if(barren$result[i] == 1){barren$result[i] <- 5} else
      if(barren$result[i] == 2){barren$result[i] <- 25} else
        if(barren$result[i] == 3){barren$result[i] <- 57.5} else
          if(barren$result[i] == 4){barren$result[i] <- 87.5}
  }
  sumna <- function(data){
    sum(data, na.rm = T)
  }
  XGB_sum <- tapply(barren$result, barren$id, sumna)
  lengthna <- function(data){
    sum(!is.na(data))
  }
  XGB.count <- tapply(barren$result, barren$id, lengthna)
  XGB.result <- XGB_sum/XGB.count
  sdna <- function(data){
    sd(data, na.rm = T)
  }
  XGB.sd <- tapply(barren$result, barren$id, sdna)
  XGB <- data.frame(cbind(XGB.result, XGB.count, XGB.sd))
  
  ###Slice for Riparian GroundCover Nonwoody Plants###
  
  nonwoodyindex <- which(ripveg$AnalyteName == "Riparian GroundCover NonWoody Plants")
  nonwoody <- data.frame(cbind(ripveg$id[nonwoodyindex], as.character(ripveg$AnalyteName[nonwoodyindex]),
                               as.character(ripveg$VariableResult[nonwoodyindex])))
  colnames(nonwoody) <- c("id", "AnalyteName", "result")
  nonwoody$result <- as.numeric(as.character(nonwoody$result))
  
  ###Compute XGH###
  for(i in (1:length(nonwoody$result))[which(!is.na(nonwoody$result))]){
    if(nonwoody$result[i] == 1){nonwoody$result[i] <- 5} else
      if(nonwoody$result[i] == 2){nonwoody$result[i] <- 25} else
        if(nonwoody$result[i] == 3){nonwoody$result[i] <- 57.5} else
          if(nonwoody$result[i] == 4){nonwoody$result[i] <- 87.5}
  }
  sumna <- function(data){
    sum(data, na.rm = T)
  }
  XGH_sum <- tapply(nonwoody$result, nonwoody$id, sumna)
  lengthna <- function(data){
    sum(!is.na(data))
  }
  XGH.count <- tapply(nonwoody$result, nonwoody$id, lengthna)
  XGH.result <- XGH_sum/XGH.count
  sdna <- function(data){
    sd(data, na.rm = T)
  }
  XGH.sd <- tapply(nonwoody$result, nonwoody$id, sdna)
  XGH <- data.frame(cbind(XGH.result, XGH.count, XGH.sd))
  
  ###Slice for Riparian GroundCover Woody Shrubs###
  
  woodyindex <- which(ripveg$AnalyteName == "Riparian GroundCover Woody Shrubs")
  woody <- data.frame(cbind(ripveg$id[woodyindex], as.character(ripveg$AnalyteName[woodyindex]),
                            as.character(ripveg$VariableResult[woodyindex])))
  colnames(woody) <- c("id", "AnalyteName", "result")
  woody$result <- as.numeric(as.character(woody$result))
  head(woody)
  
  ###Compute XGW###
  
  for(i in (1:length(woody$result))[which(!is.na(woody$result))]){
    if(woody$result[i] == 1){woody$result[i] <- 5} else
      if(woody$result[i] == 2){woody$result[i] <- 25} else
        if(woody$result[i] == 3){woody$result[i] <- 57.5} else
          if(woody$result[i] == 4){woody$result[i] <- 87.5}
  }
  sumna <- function(data){
    sum(data, na.rm = T)
  }
  XGW_sum <- tapply(woody$result, woody$id, sumna)
  lengthna <- function(data){
    sum(!is.na(data))
  }
  XGW.count <- tapply(woody$result, woody$id, lengthna)
  XGW.result <- XGW_sum/XGW.count
  sdna <- function(data){
    sd(data, na.rm = T)
  }
  XGW.sd <- tapply(woody$result, woody$id, sdna)
  XGW <- data.frame(cbind(XGW.result, XGW.count, XGW.sd))
  
  ###Slice for Riparian Lower Canopy All Vegetation###
  
  lowercanopyindex <- which(ripveg$AnalyteName == "Riparian Lower Canopy All Vegetation")
  lowercanopy <- data.frame(cbind(ripveg$id[lowercanopyindex], as.character(ripveg$AnalyteName[lowercanopyindex]),
                                  as.character(ripveg$VariableResult[lowercanopyindex])))
  colnames(lowercanopy) <- c("id", "AnalyteName", "result")
  lowercanopy$result <- as.numeric(as.character(lowercanopy$result))
  head(lowercanopy)
  
  ###Compute XM###
  
  for(i in (1:length(lowercanopy$result))[which(!is.na(lowercanopy$result))]){
    if(lowercanopy$result[i] == 1){lowercanopy$result[i] <- 5} else
      if(lowercanopy$result[i] == 2){lowercanopy$result[i] <- 25} else
        if(lowercanopy$result[i] == 3){lowercanopy$result[i] <- 57.5} else
          if(lowercanopy$result[i] == 4){lowercanopy$result[i] <- 87.5}
  }
  sumna <- function(data){
    sum(data, na.rm = T)
  }
  XM_sum <- tapply(lowercanopy$result, lowercanopy$id, sumna)
  lengthna <- function(data){
    sum(!is.na(data))
  }
  XM.count <- tapply(lowercanopy$result, lowercanopy$id, lengthna)
  XM.result <- XM_sum/XM.count
  sdna <- function(data){
    sd(data, na.rm = T)
  }
  XM.sd <- tapply(lowercanopy$result, lowercanopy$id, sdna)
  XM <- data.frame(cbind(XM.result, XM.count, XM.sd))
  
  ###Slice for Riparian Upper Canopy All Trees###
  
  uppercanopyindex <- which(ripveg$AnalyteName == "Riparian Upper Canopy All Trees")
  uppercanopy <- data.frame(cbind(ripveg$id[uppercanopyindex], as.character(ripveg$AnalyteName[uppercanopyindex]),
                                  as.character(ripveg$VariableResult[uppercanopyindex])))
  colnames(uppercanopy) <- c("id", "AnalyteName", "result")
  uppercanopy$result <- as.numeric(as.character(uppercanopy$result))
  head(uppercanopy)
  
  ###Compute XC###
  
  for(i in (1:length(uppercanopy$result))[which(!is.na(uppercanopy$result))]){
    if(uppercanopy$result[i] == 1){uppercanopy$result[i] <- 5} else
      if(uppercanopy$result[i] == 2){uppercanopy$result[i] <- 25} else
        if(uppercanopy$result[i] == 3){uppercanopy$result[i] <- 57.5} else
          if(uppercanopy$result[i] == 4){uppercanopy$result[i] <- 87.5}
  }
  sumna <- function(data){
    sum(data, na.rm = T)
  }
  XC_sum <- tapply(uppercanopy$result, uppercanopy$id, sumna)
  lengthna <- function(data){
    sum(!is.na(data))
  }
  XC.count <- tapply(uppercanopy$result, uppercanopy$id, lengthna)
  XC.result <- XC_sum/XC.count
  sdna <- function(data){
    sd(data, na.rm = T)
  }
  XC.sd <- tapply(uppercanopy$result, uppercanopy$id, sdna)
  XC <- data.frame(cbind(XC.result, XC.count, XC.sd))
  
  
  ###Compute XG###
  XG.result <- XGW$XGW.result + XGH$XGH.result
  
  ###Compute XCM###
  XCM.result <- XC$XC.result + XM$XM.result
  
  ###Compute XCMG###
  
  XCMG.result <- XG.result + XCM.result
  
  ###Compute XPMID###
  
  lowercanopyindex <- which(ripveg$AnalyteName == "Riparian Lower Canopy All Vegetation")
  lowercanopy <- data.frame(cbind(ripveg$id[lowercanopyindex], as.character(ripveg$AnalyteName[lowercanopyindex]),
                                  as.character(ripveg$VariableResult[lowercanopyindex])))
  colnames(lowercanopy) <- c("id", "AnalyteName", "result")
  lowercanopy$result <- as.numeric(as.character(lowercanopy$result))
  head(lowercanopy)
  
  XPMID_total <- tapply(lowercanopy$result, lowercanopy$id, lengthna)
  
  lowercanopy$result[which(lowercanopy$result == NA)] = 0
  XPMID_subcountf <- function(data){
    length(which(data != 0))
  }
  XPMID_subcount <- tapply(lowercanopy$result, lowercanopy$id, XPMID_subcountf)
  XPMID.result <- XPMID_total/XPMID_subcount
  
  ###Compute XPCAN###
  
  uppercanopyindex <- which(ripveg$AnalyteName == "Riparian Upper Canopy All Trees")
  uppercanopy <- data.frame(cbind(ripveg$id[uppercanopyindex], as.character(ripveg$AnalyteName[uppercanopyindex]),
                                  as.character(ripveg$VariableResult[uppercanopyindex])))
  colnames(uppercanopy) <- c("id", "AnalyteName", "result")
  uppercanopy$result <- as.numeric(as.character(uppercanopy$result))
  head(uppercanopy)
  
  XPCAN_total <- tapply(uppercanopy$result, uppercanopy$id, lengthna)
  
  uppercanopy$result[which(uppercanopy$result == NA)] = 0
  XPCAN_subcountf <- function(data){
    length(which(data != 0))
  }
  XPCAN_subcount <- tapply(uppercanopy$result, uppercanopy$id, XPCAN_subcountf)
  XPCAN.result <- XPCAN_subcount/XPCAN_total
  
  ###Compute XPGVEG###
  woodyindex <- which(ripveg$AnalyteName == "Riparian GroundCover Woody Shrubs")
  woody <- data.frame(cbind(ripveg$id[woodyindex], as.character(ripveg$AnalyteName[woodyindex]),
                            as.character(ripveg$VariableResult[woodyindex])))
  colnames(woody) <- c("id", "AnalyteName", "result")
  woody$result <- as.numeric(as.character(woody$result))
  head(woody)
  nonwoodyindex <- which(ripveg$AnalyteName == "Riparian GroundCover NonWoody Plants")
  nonwoody <- data.frame(cbind(ripveg$id[nonwoodyindex], as.character(ripveg$AnalyteName[nonwoodyindex]),
                               as.character(ripveg$VariableResult[nonwoodyindex])))
  colnames(nonwoody) <- c("id", "AnalyteName", "result")
  nonwoody$result <- as.numeric(as.character(nonwoody$result))
  head(nonwoody)
  woody$XPGVEG<-apply((cbind(woody$result, nonwoody$result)), 1, sum, na.rm=T)
  XPGVEG_subcounting<-(data.frame(!(is.na(woody$result)&(is.na(nonwoody$result))), as.character(woody$id)))
  XPGVEG_total <-tapply(XPGVEG_subcounting[[1]], (XPGVEG_subcounting[[2]]), sum)
  
  woody$XPGVEG[which(woody$XPGVEG == NA)] = 0
  XPGVEG_subcountf <- function(data){
    length(which(data != 0))
  }
  XPGVEG_subcount <- tapply(woody$XPGVEG, woody$id, XPGVEG_subcountf)
  XPGVEG.result <- XPGVEG_subcount/XPGVEG_total
  
  ###XPCM###
  aframe <- as.data.frame(cast(ripveg, id + LocationCode ~ AnalyteName, value = "VariableResult",fun.aggregate=NULL))
  
  for(i in 3:7){
    aframe[[i]] <- as.numeric(as.character(aframe[[i]]))
  }
  aframe[is.na(aframe)] <- (-1)
  aframe[aframe == "Not Recorded"] <- (-1)
  aframe$XPCM <- rep(NA, length(aframe$id))
  for(i in which(!is.na(aframe[[3]]))){
    aframe$XPCM[i] <- if((aframe$"Riparian Upper Canopy All Trees"[i]>0)&(
      aframe$"Riparian Lower Canopy All Vegetation"[i]>0)){T} else
        if((aframe$"Riparian Upper Canopy All Trees"[i]==0)|(
          aframe$"Riparian Lower Canopy All Vegetation"[i]==0)){F}else{NA}
  }
  aframe$"Riparian Upper Canopy All Trees"[which(aframe$"Riparian Upper Canopy All Trees" == -1)] <-NA
  XPCM.result <- tapply(aframe$XPCM, aframe$id, sumna)/tapply(aframe$XPCM, aframe$id, lengthna)
  
  ###XPCMG###
  aframe$"Riparian Upper Canopy All Trees"[is.na(aframe$"Riparian Upper Canopy All Trees")]<- (-1)
  aframe$XPCMG <- rep(NA, length(aframe$id))
  for(i in which(!is.na(aframe[[3]]))){
    aframe$XPCMG[i] <- if(((aframe$"Riparian Upper Canopy All Trees"[i]>0)&(
      aframe$"Riparian Lower Canopy All Vegetation"[i]>0))&(
        (aframe$"Riparian GroundCover Woody Shrubs"[i] >0) |
          (aframe$"Riparian GroundCover NonWoody Plants"[i] >0))){T} else
            if(((aframe$"Riparian Upper Canopy All Trees"[i]==0)|(
              aframe$"Riparian Lower Canopy All Vegetation"[i]==0))|(
                (aframe$"Riparian GroundCover Woody Shrubs"[i] ==0) |
                  (aframe$"Riparian GroundCover NonWoody Plants"[i] ==0))){F}else{NA}
  }
  XPCMG.result <- tapply(aframe$XPCMG, aframe$id, sumna)/tapply(aframe$XPCMG, aframe$id, lengthna)
  
  ###XPMGVEG###
  
  aframe$XPMGVEG <- rep(NA, length(aframe$id))
  for(i in which(!is.na(aframe[[3]]))){
    aframe$XPMGVEG[i] <- if((aframe$"Riparian GroundCover Woody Shrubs"[i] >1) |
      (aframe$"Riparian GroundCover NonWoody Plants"[i] >1)){T} else
        if((aframe$"Riparian GroundCover Woody Shrubs"[i] ==1) |
          (aframe$"Riparian GroundCover NonWoody Plants"[i] ==1)){F}else
            if((aframe$"Riparian GroundCover Woody Shrubs"[i] ==0) |
              (aframe$"Riparian GroundCover NonWoody Plants"[i] ==0)){F} else{NA}
  }
  XPMGVEG.result <- tapply(aframe$XPMGVEG, aframe$id, sumna)/tapply(aframe$XPMGVEG, aframe$id, lengthna)
  
  
  ###Write to file###
  results <- cbind(XGB, XGH, XGW, XM, XC, XG.result, XCM.result, XCMG.result, 
                   XPMID.result, XPCAN.result, XPGVEG.result, XPCM.result, XPCMG.result, XPMGVEG.result)
  print("ripveg done")
  results
}

substrate <- function (substrate) {
  substrate <- substrate[which(substrate$AnalyteName %in% c('Substrate Size Class', 'Embeddedness', 'CPOM')),]
  substrate$VariableResult <- as.character(substrate$VariableResult)
  substrate$VariableResult[substrate$VariableResult=="a"]<-"SA"
  substrate$VariableResult[substrate$VariableResult=="as"]<-"SA"
  substrate$VariableResult[substrate$VariableResult=="ws"]<-"SA"
  substrate$VariableResult[substrate$VariableResult=="sn"]<-"SA" 
  substrate$VariableResult[substrate$VariableResult=="N"]<-"Not Recorded"
  substrate$VariableResult[substrate$VariableResult=="n"]<- "Not Recorded"
  substrate$VariableResult[substrate$VariableResult=="LB"]<-"XB"
  substrate$VariableResult[substrate$VariableResult=="G"]<- "GC"
  substrate$VariableResult[substrate$VariableResult=="FM"]<-"FN"
  substrate$VariableResult[substrate$VariableResult=="fd"]<-"SB"
  substrate$VariableResult[substrate$VariableResult=="fb"]<-"SB"
  
  sub <- substrate[substrate$AnalyteName =="Substrate Size Class",]
  
  sub$VariableResult[sub$Result>= 1000 & sub$Result<4000] <- "XB"
  sub$VariableResult[sub$Result>= 250 & sub$Result<1000] <- "SB"
  sub$VariableResult[sub$Result>= 64 & sub$Result<250] <- "CB"
  sub$VariableResult[sub$Result>= 16 & sub$Result<64] <- "GC"
  sub$VariableResult[sub$Result>= 2 & sub$Result<16] <- "GF"
  sub$VariableResult[sub$Result>= 0.06 & sub$Result<2] <- "SA"
  
  ###Compute
  
  metric <- c('RS', 'RR', 'RC', 'XB', 'SB', 'CB', 'GC', 'GF', 'SA', 'FN', 'HP', 'WD', 'OT')
  
  sub$VariableResult <- lapply(sub$VariableResult, toupper)
  
  lengths <- function(data){
    length(which(((data != "NOT RECORDED") &(data != "NA"))&(data != "FNOT RECORDED")))}
  totals <- tapply(sub$VariableResult, sub$id, lengths)
  tnames <- as.vector(dimnames(totals))
  qq <-unlist(tnames)
  l <- matrix(NA, ncol=length(metric), nrow=length(totals))
  
  for(j in 1:length(qq))
    for(i in 1:length(metric)){
      l[j, i] <- length(which(sub$VariableResult[sub$id == qq[j]] == metric[i]))
    }
  divd <- function(data) data*100/totals
  result <- as.data.frame(apply(l, 2, divd))
  if(length(colnames(result))==1){
    result <- t(result)
    rownames(result) <- unique(sub$id)
    result <-as.data.frame(result)
  }
  colnames(result) <- c("PCT_RS.result","PCT_RR.result","PCT_RC.result","PCT_XB.result","PCT_SB.result","PCT_CB.result",
                        "PCT_GC.result","PCT_GF.result","PCT_SA.result","PCT_FN.result","PCT_HP.result",
                        "PCT_WD.result","PCT_OT.result")
  
  result$PCT_BDRK.result <- result$PCT_RS + result$PCT_RR
  result$PCT_BIGR.result <- result$PCT_RS + result$PCT_RR + result$PCT_XB + result$PCT_SB + result$PCT_CB + result$PCT_GC
  result$PCT_SFGF.result <- result$PCT_GF + result$PCT_SA + result$PCT_FN
  result$PCT_SAFN.result <- result$PCT_SA + result$PCT_FN
  
  ###Second set of computation
  sub$value <- rep(NA, length(sub$id))
  sub$value[which(!is.na(sub$Result))] <- sub$Result[which(!is.na(sub$Result))]
  sub$value[which(sub$VariableResult == "RR")] <- 5660.0
  sub$value[which(sub$VariableResult == "RS")] <- 5660.0
  sub$value[which(sub$VariableResult == "XB")] <- 2500.0
  sub$value[which(sub$VariableResult == "SB")] <- 625.0
  sub$value[which(sub$VariableResult == "CB")] <- 157.0
  sub$value[which(sub$VariableResult == "GC")] <- 40.0
  sub$value[which(sub$VariableResult == "GF")] <- 9.0
  sub$value[which(sub$VariableResult == "SA")] <- 1.03
  sub$value[which(sub$VariableResult == "FN")] <- 0.03
  sub$value[which(sub$VariableResult == "HP")] <- 5660.0
  sub$value[which(sub$VariableResult == "RC")] <- 5660.0
  
  
  
  sub$log <- log10(sub$value)
  
  sumna <- function(data){sum(data, na.rm=T)}
  lengthna <- function(data){sum(!is.na(data))}
  
  XSDGM_sum <- tapply(sub$log, sub$id, sumna)
  XSDGM_count <- tapply(sub$log, sub$id, lengthna)
  XSDGM <- 10^(XSDGM_sum/XSDGM_count)
  result$XSDGM.result <- XSDGM
  
  sub$value2 <- rep(NA, length(sub$id))
  sub$value2[which(!is.na(sub$Result))] <- sub$Result[which(!is.na(sub$Result))]
  sub$value2[which(sub$VariableResult == "RR")] <- NA
  sub$value2[which(sub$VariableResult == "RS")] <- NA
  sub$value2[which(sub$VariableResult == "XB")] <- 2500.0
  sub$value2[which(sub$VariableResult == "SB")] <- 625.0
  sub$value2[which(sub$VariableResult == "CB")] <- 157.0
  sub$value2[which(sub$VariableResult == "GC")] <- 40.0
  sub$value2[which(sub$VariableResult == "GF")] <- 9.0
  sub$value2[which(sub$VariableResult == "SA")] <- 1.03
  sub$value2[which(sub$VariableResult == "FN")] <- 0.03
  sub$value2[which(sub$VariableResult == "HP")] <- NA
  sub$value2[which(sub$VariableResult == "RC")] <- NA
  
  
  
  sub$log2 <- log10(sub$value2)
  
  XSPDGM_sum <- tapply(sub$log2, sub$id, sumna)
  XSPDGM_count <- tapply(sub$log2, sub$id, lengthna)
  XSDPGM <- 10^(XSPDGM_sum/XSPDGM_count)
  result$XSPGM.result <- XSDPGM
  
  qant <- function(data)quantile(data, c(.5, .1, .25, .75, .9), na.rm=T)
  otwd <- which(!(sub$VariableResult %in% c("OT", "WD")))
  temp <-tapply(sub$value[otwd], sub$id[otwd], qant)
  for(i in 1:length(temp)){
    result$SB_PT_D50.result[i] <- temp[[i]][1]
    result$SB_PT_D10.result[i] <- temp[[i]][2]
    result$SB_PT_D25.result[i] <- temp[[i]][3]
    result$SB_PT_D75.result[i] <- temp[[i]][4]
    result$SB_PT_D90.result[i] <- temp[[i]][5]
  }
  OTWDHP <- which(!(sub$VariableResult %in% c("OT", "WD", "HP", "RS", "RR", "RC")))
  temp2 <-tapply(sub$value[OTWDHP], sub$id[OTWDHP], qant)
  for(i in 1:length(temp2)){
    result$SB_PP_D50.result[i] <- temp2[[i]][1]
    result$SB_PP_D10.result[i] <- temp2[[i]][2]
    result$SB_PP_D25.result[i] <- temp2[[i]][3]
    result$SB_PP_D75.result[i] <- temp2[[i]][4]
    result$SB_PP_D90.result[i] <- temp2[[i]][5]
  }
  sdna <- function(data) sd(data, na.rm=T)
  embed <- substrate[which((substrate$AnalyteName=="Embeddedness")&
    (!(substrate$LocationCode=="X"))),]
  XEMBED_sum <- tapply(embed$Result, embed$id, sumna)
  XEMBED_count <- tapply(embed$Result, embed$id, lengthna)
  result$XEMBED.result <- XEMBED_sum/XEMBED_count
  result$XEMBED.count <- XEMBED_count
  result$XEMBED.sd <- tapply(embed$Result, embed$id, sdna)
  
  cpom <- substrate[substrate$AnalyteName=="CPOM",]
  present <- function(data){
    sum(data == "Present")
  }
  cpomtotal <- function(data){
    sum((data == "Present") | (data == "Absent"))
  }
  cpresent <- tapply(cpom$VariableResult, cpom$id, present)
  ctotal <- tapply(cpom$VariableResult, cpom$id, cpomtotal)
  result$PCT_CPOM.result <- cpresent*100/ctotal
  print("substrate done")
  result
}

phabFormat <- function(data){
  data$VariableResult[data$ResQualCode=="NR"] <- "Not Recorded"
  data$Result[data$ResQualCode=="NR"] <- NA
  data$id <- do.call(paste, c(data[c("StationCode", "SampleDate")]))
  data
}

phabMetrics <- function(data){
  data <- phabFormat(data)
  metrics <- list(bankmorph(data), channelmorph(data), channelsinuosity(data),
                  densiometer(data),  habitat(data), disturbance(data), #flow(data),
                  misc(data), Bankstability(data), quality_metrics(data), ripveg(data),
                  substrate(data))#, algae(data))
  cbind.fill<-function(...){
    nm <- list(...) 
    nm<-lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow)) 
    do.call(cbind, lapply(nm, function (x) 
      rbind(x, matrix(, n-nrow(x), ncol(x))))) 
  }
  
  data <- do.call(cbind.fill, metrics)
  data
}

test <- phabMetrics(full_raw_PHABdata)