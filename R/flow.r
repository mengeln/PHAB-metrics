###Read in requested observations
setwd("L:/Bioassessment Data Management Tools_RM/R Scripts")
rowinput <- read.csv("input.csv", header=T)
options(useFancyQuotes = F)
rowinput$StationCode <- sQuote(rowinput$StationCode)
SC <- paste(rowinput$StationCode, collapse="")
SC <- gsub("''", "', '", SC)

###Construct Query
select <- "SELECT Sample_Entry.ProjectCode, Sample_Entry.SampleDate, StationLookUp.Waterbody, Sample_Entry.StationCode, Sample_Entry.AgencyCode, Geometry_Entry.LocationRowID, Location_Entry.LocationCode, Location_Entry.LocationRowID, Location_Entry.GeometryShape, Geometry_Entry.Latitude, Geometry_Entry.Longitude, FractionLookUp.FractionName, UnitLookUp.UnitCode, UnitLookUp.UnitName, HabitatCollection_Entry.CollectionTime, HabitatCollection_Entry.HabitatCollectionComments, CollectionMethodLookUp.CollectionMethodCode, CollectionMethodLookUp.CollectionMethodName, HabitatCollection_Entry.Replicate, ConstituentLookUp.ConstituentCode, AnalyteLookUp.AnalyteName, MatrixLookUp.MatrixName, HabitatResult_Entry.VariableResult, HabitatResult_Entry.Result, MethodLookUp.MethodName, HabitatResult_Entry.QACode, HabitatResult_Entry.CollectionDeviceCode, HabitatResult_Entry.HabitatResultComments, CollectionDeviceLookUp.CollectionDeviceName, HabitatResult_Entry.ResQualCode, HabitatResult_Entry.ComplianceCode"
from <- "FROM UnitLookUp INNER JOIN (CollectionDeviceLookUp INNER JOIN (FractionLookUp INNER JOIN (AnalyteLookUp INNER JOIN ((CollectionMethodLookUp INNER JOIN (((StationLookUp INNER JOIN Sample_Entry ON StationLookUp.StationCode = Sample_Entry.StationCode) INNER JOIN (Location_Entry LEFT JOIN Geometry_Entry ON Location_Entry.LocationRowID = Geometry_Entry.LocationRowID) ON Sample_Entry.SampleRowID = Location_Entry.SampleRowID) INNER JOIN HabitatCollection_Entry ON Location_Entry.LocationRowID = HabitatCollection_Entry.LocationRowID) ON CollectionMethodLookUp.CollectionMethodCode = HabitatCollection_Entry.CollectionMethodCode) INNER JOIN (MethodLookUp INNER JOIN (MatrixLookUp INNER JOIN (ConstituentLookUp INNER JOIN HabitatResult_Entry ON ConstituentLookUp.ConstituentRowID = HabitatResult_Entry.ConstituentRowID) ON MatrixLookUp.MatrixCode = ConstituentLookUp.MatrixCode) ON MethodLookUp.MethodCode = ConstituentLookUp.MethodCode) ON HabitatCollection_Entry.HabitatCollectionRowID = HabitatResult_Entry.HabitatCollectionRowID) ON AnalyteLookUp.AnalyteCode = ConstituentLookUp.AnalyteCode) ON FractionLookUp.FractionCode = ConstituentLookUp.FractionCode) ON CollectionDeviceLookUp.CollectionDeviceCode = HabitatResult_Entry.CollectionDeviceCode) ON UnitLookUp.UnitCode = ConstituentLookUp.UnitCode"
if(length(rowinput$StationCode) < 50){
where <- paste("WHERE (((Sample_Entry.StationCode) IN (", SC, ")) AND (Location_Entry.LocationCode='X')) AND (MethodLookUp.MethodName In ('Velocity Area', 'Neutral Buoyant Object'))")}else
  {where <- "WHERE (Location_Entry.LocationCode='X') AND (MethodLookUp.MethodName In ('Velocity Area', 'Neutral Buoyant Object'))"}
orderby <- "ORDER BY Sample_Entry.ProjectCode, Sample_Entry.SampleDate, Sample_Entry.StationCode, Location_Entry.LocationCode, ConstituentLookUp.ConstituentCode"

select2 <- "SELECT Sample_Entry.StationCode, Sample_Entry.SampleDate, FieldResult_Entry.CollectionDeviceCode, CollectionDeviceLookUp.CollectionDeviceName, FieldResult_Entry.CalibrationDate, FieldCollection_Entry.CollectionDepth, FieldCollection_Entry.Replicate, AnalyteLookUp.AnalyteName, FieldResult_Entry.Result, FieldResult_Entry.ResQualCode, FieldResult_Entry.QACode"
from2 <- "FROM CollectionDeviceLookUp INNER JOIN (AnalyteLookUp INNER JOIN (((Sample_Entry INNER JOIN Location_Entry ON Sample_Entry.SampleRowID = Location_Entry.SampleRowID) INNER JOIN FieldCollection_Entry ON Location_Entry.LocationRowID = FieldCollection_Entry.LocationRowID) INNER JOIN (ConstituentLookUp INNER JOIN FieldResult_Entry ON ConstituentLookUp.ConstituentRowID = FieldResult_Entry.ConstituentRowID) ON FieldCollection_Entry.FieldCollectionRowID = FieldResult_Entry.FieldCollectionRowID) ON AnalyteLookUp.AnalyteCode = ConstituentLookUp.AnalyteCode) ON CollectionDeviceLookUp.CollectionDeviceCode = FieldResult_Entry.CollectionDeviceCode"
if(length(rowinput$StationCode) < 50){
where2 <- paste("WHERE (((Sample_Entry.StationCode) IN (", SC, ")) AND ((AnalyteLookUp.AnalyteName) In ('Distance from Bank', 'StationWaterDepth', 'velocity', 'Distance, Float', 'Float time', 'wetted width')) AND ((Sample_Entry.EventCode)='ba'))")}else
  {where2 <- "WHERE (AnalyteLookUp.AnalyteName) In ('Distance from Bank', 'StationWaterDepth', 'velocity', 'Distance, Float', 'Float time', 'wetted width') AND ((Sample_Entry.EventCode)='ba')"}
orderby2 <- "ORDER BY Sample_Entry.StationCode, Sample_Entry.SampleDate, AnalyteLookUp.AnalyteName"

###Connect to DB
library(RODBC)
mydsn <- odbcConnect("SMCreporter", uid ="GisUser", pwd = "")

flow <- data.frame(sqlQuery(mydsn, paste(select, from, where, orderby)))
velocity <- data.frame(sqlQuery(mydsn, paste(select2, from2, where2, orderby2)))
odbcClose(mydsn)

###Format Data Frame##
velocity$Result[velocity$ResQualCode=="NR"] <- NA
flow$Result[flow$ResQualCode=="NR"] <- NA

velocity$Result[velocity$Result==-88] <- NA
velocity$Result[velocity$Result<0] <- 0
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

###Query for Neutrally buoyant float method###

select3 <- "SELECT Sample_Entry.StationCode, Sample_Entry.SampleDate, Location_Entry.LocationCode, AnalyteLookUp.AnalyteName, HabitatResult_Entry.Result, HabitatResult_Entry.ResQualCode, HabitatResult_Entry.QACode, HabitatCollection_Entry.Replicate"
from3 <- "FROM AnalyteLookUp INNER JOIN (((Sample_Entry INNER JOIN Location_Entry ON Sample_Entry.SampleRowID = Location_Entry.SampleRowID) INNER JOIN HabitatCollection_Entry ON Location_Entry.LocationRowID = HabitatCollection_Entry.LocationRowID) INNER JOIN (ConstituentLookUp INNER JOIN HabitatResult_Entry ON ConstituentLookUp.ConstituentRowID = HabitatResult_Entry.ConstituentRowID) ON HabitatCollection_Entry.HabitatCollectionRowID = HabitatResult_Entry.HabitatCollectionRowID) ON AnalyteLookUp.AnalyteCode = ConstituentLookUp.AnalyteCode"
if(length(rowinput$StationCode) < 50)
  {where3 <- paste("WHERE (((Sample_Entry.StationCode) IN (", SC, "))  AND ((AnalyteLookUp.AnalyteName)
                   In ('Float Time', 'StationWaterDepth', 'Wetted Width', 'Distance, Float')) AND 
                   ((Sample_Entry.EventCode)='ba')) AND (Location_Entry.LocationCode In 
                   ('Float Reach', 'Lower Section Point 1', 'Upper Section Point3', 'Upper Section Point4',
                   'Lower Section Point 5', 'Middle Section Point 1', 'Upper Section Point2', 
                   'Middle Section Point 5', 'Lower Section Point 4', 'Upper Section Point1', 
                   'Middle Section Point 4', 'Upper Section Point5', 'Middle Section Point 2', 
                   'Lower Section Point 2', 'Middle Section Point 3', 'Lower Section Point 3', 
                   'Upper Section', 'Lower Section', 'Middle Section'))")}else
                   {where3 <- "WHERE (((AnalyteLookUp.AnalyteName)
                   In ('Float Time', 'StationWaterDepth', 'Wetted Width', 'Distance, Float')) AND 
                   ((Sample_Entry.EventCode)='ba')) AND (Location_Entry.LocationCode In 
                   ('Float Reach', 'Lower Section Point 1', 'Upper Section Point3', 'Upper Section Point4',
                   'Lower Section Point 5', 'Middle Section Point 1', 'Upper Section Point2', 
                   'Middle Section Point 5', 'Lower Section Point 4', 'Upper Section Point1', 
                   'Middle Section Point 4', 'Upper Section Point5', 'Middle Section Point 2', 
                   'Lower Section Point 2', 'Middle Section Point 3', 'Lower Section Point 3', 
                   'Upper Section', 'Lower Section', 'Middle Section', 'Float Reach TransUpper, Point1',
                    'Float Reach TransUpper, Point2', 'Float Reach TransUpper, Point3', 'Float Reach TransUpper, Point4', 'Float Reach TransUpper, Point5', 'Float Reach TransMiddle, Point1', 'Float Reach TransMiddle, Point2',
                    'Float Reach TransMiddle, Point3', 'Float Reach TransMiddle, Point4', 'Float Reach TransMiddle, Point5', 'Float Reach TransLower, Point1', 'Float Reach TransLower, Point2', 'Float Reach TransLower, Point3', 'Float Reach TransLower, Point4', 'Float Reach TransLower, Point5', 'Float Reach TransLower', 'Float Reach TransMiddle', 'Float Reach TransUpper'))"}

orderby3 <- "ORDER BY Sample_Entry.StationCode, Sample_Entry.SampleDate, AnalyteLookUp.AnalyteName"


mydsn <- odbcConnect("SMCreporter", uid ="GisUser", pwd = "")
neutral <- data.frame(sqlQuery(mydsn, paste(select3, from3, where3, orderby3)))
odbcClose(mydsn)

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

###Write to file###
fc <- file("flow_metrics.csv", open="w")
write.csv(result, fc)
close(fc)
print("Flow complete")