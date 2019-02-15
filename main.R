# to get all libraries at once just install the whole tidyverse
library(ggplot2)
library(lubridate)
library(dplyr)

default = "F:\\Users\\Janis\\VIKA\\solR\\"

importData = function(whichData, id, colNames, skipCount){
    setwd(paste(default, "data\\", whichData, "\\", sep=""))
    lstData = list.files(pattern="*.csv")
    if (id == "main"){
        data = read.csv(grep(id, lstData, value = TRUE), skip = skipCount, header = FALSE, sep = ",")\
        # names(data)[names(data) == 'old.var.name'] <- 'new.var.name'
    }
    else {
    data = read.csv(grep(id, lstData, value = TRUE), skip = skipCount, header = FALSE, col.names = colNames, sep = ",")
    }
}

fixDatetime = function(data){
    # convert timestamp class from factor to POSIXct
    data$timestamp = as.POSIXct(strptime(data$timestamp, format="%Y-%m-%d %H:%M:%S"))
    # replace NA values with 0
    data[is.na(data)] = 0
    return(data)
}

colNamesMeteo = c("timestamp", "tz", "wdir", "velocity", "pressure", "humidity", "temperature", "solarIrradiance")
colNamesKWH  = c("timestamp", "gridToBattery", "gridToConsumers", "PVToBattery", "PVToGrid", "PVToConsumers", "batteryToConsumers", "batteryToGrid", "gensetToConsumers", "gensetToBattery", "gas")

datKWH = importData("solar", "kwh", colNamesKWH, 2)
datKWH = fixDatetime(datKWH)

datSol = importData("solar", "main", colNamesKWH, 2)
datSol = fixDatetime(datKWH)

# there is a problem with this one and all others in future.
# meteo has >1 case of specific filenames in folder.
# datMeteo = importData("meteo", "idk", colNamesMeteo, 1)
# datMeteo = fixDatetime(datKWH)

timestamp = datKWH$timestamp
gridToBattery = datKWH$gridToBattery

# Common plot theme
sharedTheme = theme_minimal()
# Change the appearance of the main title
sharedTheme = sharedTheme + theme(plot.title = element_text(size=18, face="bold",margin = margin(10, 0, 10, 0)))
sharedAxis = xlab("Time")

pltWeekStats = function(data, timestamp, dependentVar, nor, i){
    # Summarize gridToBattery by 2 hours in a 2 days interval
    data %>% group_by(timestamp=floor_date(timestamp, "1 hour")) %>%
    summarize(gridToBattery=sum(gridToBattery))  %>%
    ggplot(aes(x = timestamp, y = gridToBattery)) + geom_point() + sharedTheme +
    sharedAxis + ylab("Grid To Battery, kWh") +
    ggtitle(paste('Grid to Battery ', toString(interval(date(nor), (date(nor) + days(2)))))) +
    coord_cartesian(xlim = c(nor, nor + days(2)))
    ggsave(paste('week', toString(i), '.pdf', sep=""), width = 29.7, height = 21.0, units = "cm")
}

weekStats <- function(data, timestamp, dependentVar){
    setwd("F:\\Users\\Janis\\VIKA\\solR\\plots\\")
    nor = min(timestamp)
    i = 1
    while (interval(date(nor), (date(nor) + days(2))) %within% interval(date(min(timestamp)), (date(max(timestamp))))) {
        pltWeekStats(datKWH, timestamp, gridToBattery, nor, i)
        nor =  nor + days(2)
        i = i + 1
    }
}

weekStats(datKWH, timestamp, gridToBattery)

# Plots for a month
datKWH %>% group_by(timestamp=floor_date(timestamp, "2 hours")) %>%
    summarize(gridToBattery=sum(gridToBattery)) %>%
    ggplot(aes(x = timestamp, y = gridToBattery)) + geom_point() + sharedTheme + 
    coord_cartesian(xlim = c(min(timestamp), min(timestamp) + months(1))) + 
    ggtitle(paste('Grid to Battery ', toString(interval(min(timestamp), min(timestamp) + months(1)))))
    ggsave("r2.pdf", width = 29.7, height = 21.0, units = "cm")

datKWH %>% group_by(timestamp=floor_date(timestamp, "day")) %>%
    summarize(gridToBattery=sum(gridToBattery)) %>%
    ggplot(aes(x = timestamp, y = gridToBattery)) + geom_point() + sharedTheme + sharedAxis + 
    ggtitle(paste('Grid to Battery ', toString(interval(min(timestamp), min(timestamp) + months(1)))))
    ggsave("r3.pdf", width = 29.7, height = 21.0, units = "cm")