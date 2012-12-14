#con <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=T:\\SMCStreamMonitoringProgram\\CurrentDatabases\\SMCReporter1.0.1d.mdb")
query <- "
SELECT sample_entry.stationcode,
       sample_entry.sampledate,
       location_entry.locationcode,
       analytelookup.analytename,
       habitatresult_entry.result,
       habitatresult_entry.variableresult,
       fieldresult_entry.result,
       habitatresult_entry.resqualcode,
       fieldresult_entry.resqualcode
FROM   (((((sample_entry
       INNER JOIN location_entry
               ON sample_entry.samplerowid = location_entry.samplerowid)
       INNER JOIN habitatcollection_entry
               ON location_entry.locationrowid =
                  habitatcollection_entry.locationrowid)
       INNER JOIN habitatresult_entry
               ON habitatcollection_entry.habitatcollectionrowid =
                  habitatresult_entry.habitatcollectionrowid)
       INNER JOIN constituentlookup
               ON habitatresult_entry.constituentrowid =
                  constituentlookup.constituentrowid)
       INNER JOIN analytelookup
               ON constituentlookup.analytecode = 
                  analytelookup.analytecode)
       LEFT OUTER JOIN fieldresult_entry
               ON constituentlookup.constituentrowid =
                  fieldresult_entry.constituentrowid
"
full <- sqlQuery(con, query)
View(full)