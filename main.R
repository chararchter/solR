library(ggplot2)

setwd("F:\\Users\\Janis\\VIKA\\data\\")
lstData = list.files(pattern="*.csv")

year = 2019
month = formatC(seq(1, 12), width=2, flag=0)
day = formatC(seq(1, 31), width=2, flag=0)

colNamesKHW  = c("timestamp", "gridToBattery", "gridToConsumers", "PVToBattery", "PVToGrid", "PVToConsumers", "batteryToConsumers", "batteryToGrid", "gensetToConsumers", "gensetToBattery", "gas")
datKWH = read.csv(grep("kwh", lstData, value = TRUE), skip = 2, header = FALSE, col.names = colNamesKHW, sep = ",")

dat = read.csv(grep("main", lstData, value = TRUE), header = TRUE, sep = ",")

print(head(datKWH))