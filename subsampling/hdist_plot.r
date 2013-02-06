library(RODBC)
source("development/metricCalc.r")
source("subsampling/humanDist_subsample.r")
source("subsampling/substrate_plot.r")

connection <- odbcConnectAccess2007("P:/PartTimers/MarkEngeln/SWAMP_RM_112012.mdb")

hdist <- sqlQuery(connection, "SELECT * FROM Query3_test 
                       WHERE AnalyteName IN ('Riparian Bridges/Abutments',
                                       'Riparian Buildings',
                                       'Riparian Landfill/Trash',
                                       'Riparian Logging',
                                       'Riparian Mining',
                                       'Riparian Orchards/Vineyards',
                                       'Riparian Park/Lawn',
                                       'Riparian Pasture/Range',
                                       'Riparian Pavement',
                                       'Riparian Pipes',
                                       'Riparian Road',
                                       'Riparian Row Crops',
                                       'Riparian Vegetation Management',
                                       'Riparian Wall/Dike')")

odbcClose(connection)

hdist$SampleID <- with(hdist, paste0(StationCode, SampleDate, ProjectCode))
hdist_res <- humanDisturbance2(hdist)

substrate_map(hdist_res, 0.1)