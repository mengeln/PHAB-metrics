library(RODBC)

connection <- odbcConnectAccess2007("P:/PartTimers/MarkEngeln/SWAMP_RM_112012.mdb")

# sqlQuery(connection, "SELECT Count(*) AS N
# FROM
# (SELECT DISTINCT StationCode FROM Query3_test) AS T")



projects <- sqlQuery(connection, "SELECT DISTINCT ProjectCode, ProtocolName FROM Query3_test WHERE ProtocolName IN 
                     ('CCAMP Field Sampling Protocol 2006', 'CCAMP Field Sampling Protocol 2012', 'DFG-ABL 2005 Wadeable Streams', 
'EMAP 2001 Wadeable Streams', 'EMAP Coastal, MPSL-DFG_Field SOP_v1.0', 'SNARL_1996_WS, SNARL_2003_WS', 'SNARL_2005_WS, SNARL_2007_WS',
'SNARL_2008_WS, SWAMP 07 & EMAP 01 Wadeable Streams combination', 'SWAMP 2007 & SNARL 2007 Wadeable Streams', 'SWAMP_2007_WS')")
projects.sub <- as.character(projects$ProjectCode[projects$ProtocolName != "SWAMP_2007_WS"])

test <- sqlQuery(connection, paste0("SELECT * FROM Query3_test WHERE ProjectCode = '", "RWB1_RuR_FY1011", "'"))
test$SampleID <- with(test, paste0(StationCode, ProjectCode, SampleDate))
# test2 <- subset(test, SampleID == test$SampleID[1])
# 
phabMetrics(test)

phab_test2 <- lapply(projects.sub, function(p){
  try({
    data <- sqlQuery(connection, paste0("SELECT * FROM Query3_test WHERE ProjectCode = '", p, "'"))
    print(p)
    if(nrow(data)==0)NA else{
      data$SampleID <- with(data, paste0(StationCode, SampleDate))
      res <- phabMetrics(data)
      gc()
      res}
  })
})

odbcClose(connection)

phab_result2 <- Filter(is.data.frame, phab_test2)
phab_NON_SWAMP_2007_WS <- Reduce(rbind, phab_result2)

full <- rbind(phab_NON_SWAMP_2007_WS, phab_SWAMP_2007_WS)

save(full, file="full.rdata")