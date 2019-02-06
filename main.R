library(ggplot2)

setwd("F:\\Users\\Janis\\VIKA\\data\\")
lstData = list.files(pattern="*.csv")

# year = 2019
# month = formatC(seq(1, 12), width=2, flag=0)
# day = formatC(seq(1, 31), width=2, flag=0)

dat = read.csv(grep("main", lstData, value = TRUE), header = TRUE, sep = ",")

colNamesKHW  = c("timestamp", "gridToBattery", "gridToConsumers", "PVToBattery", "PVToGrid", "PVToConsumers", "batteryToConsumers", "batteryToGrid", "gensetToConsumers", "gensetToBattery", "gas")

# convert timestamp class from factor to POSIXct
datKWH$timestamp <- as.POSIXct(strptime(datKWH$timestamp, format="%Y-%m-%d %H:%M:%S"))

# check if timestamp is still readable
print(head(datKWH))

# check classes of the columns
print(sapply(datKWH, class))