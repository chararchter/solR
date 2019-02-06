# library(tidyverse)
library(ggplot2)

setwd("F:\\Users\\Janis\\VIKA\\data\\")
lstData = list.files(pattern="*.csv")

# year = 2019
# month = formatC(seq(1, 12), width=2, flag=0)
# day = formatC(seq(1, 31), width=2, flag=0)

# dat = read.csv(grep("main", lstData, value = TRUE), header = TRUE, sep = ",")

colNamesKHW  = c("timestamp", "gridToBattery", "gridToConsumers", "PVToBattery", "PVToGrid", "PVToConsumers", "batteryToConsumers", "batteryToGrid", "gensetToConsumers", "gensetToBattery", "gas")
datKWH = read.csv(grep("kwh", lstData, value = TRUE), skip = 2, header = FALSE, col.names = colNamesKHW, sep = ",")

# convert timestamp class from factor to POSIXct
datKWH$timestamp <- as.POSIXct(strptime(datKWH$timestamp, format="%Y-%m-%d %H:%M:%S"))

print(head(datKWH))

g = ggplot(datKWH, aes(x = datKWH[, 'timestamp'], y = datKWH[, 'gridToBattery'])) + geom_point()
g = g + xlab("Time") + ylab("Grid To Battery, kWh")
# scale_*_datetime for datetimes (class POSIXct),
g = g + scale_x_datetime(breaks='1 week', labels = "%W")

# use tidyverse to get rid of NA values
# datKWH %>%
#   filter(!is.na(datKWH[, 'gridToBattery'])) %>%
#   ggplot(datKWH, aes(x = datKWH[, 'timestamp'], y = datKWH[, 'gridToBattery'])) + geom_point()