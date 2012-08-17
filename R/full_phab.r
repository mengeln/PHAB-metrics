flow <- flow[,c("StationCode", "SampleDate", "LocationCode", "Replicate", "AnalyteName", 
                "VariableResult", "Result", "MethodName", "QACode", "ResQualCode")]
bankmorph <- bankmorph[, !(colnames(bankmorph) %in% c("id", "LocationCode2"))]
channel_morph <- channel_morph[, !(colnames(channel_morph) %in% c("id"))]
data <- data[, !(colnames(data) %in% c("id"))]
Densiometer <- Densiometer[, !(colnames(Densiometer) %in% c("id", "view", "trans"))]
habitat <- habitat[, !(colnames(habitat) %in% c("id", "convert", "present"))]
misc <- misc[, !(colnames(misc) %in% c("id"))]
Bank_stability <- Bank_stability[, !(colnames(Bank_stability) %in% c("id"))]
quality <- quality[, !(colnames(quality) %in% c("id", "CalibrationDate", "CollectionDeviceCode", "CollectionDeviceName"))]
ripveg <- ripveg[, !(colnames(ripveg) %in% c("id"))]
velocity <- velocity[, !(colnames(velocity) %in% c("CalibrationDate", "CollectionDeviceName", "CollectionDeviceCode"))]



full_raw_PHABdata <- rbind.fill(algae, bankmorph, channel_morph, data, Densiometer, flow, velocity,
                                neutral, habitat, dataH, misc, Bank_stability, quality, ripveg,
                                substrate1)
colnames(full_raw_PHABdata)

write.csv(full_raw_PHABdata, file="full_raw_PHABdata.csv")
