# to get all libraries at once just install the whole tidyverse
library(ggplot2)
library(lubridate)
library(dplyr)

meteoData = function(){
    setwd("F:\\Users\\Janis\\VIKA\\solR\\data\\meteo\\")
    lstData = list.files(pattern="*.csv")
}

solarSummary = function(){
    setwd("F:\\Users\\Janis\\VIKA\\solR\\data\\solar\\")
    lstData = list.files(pattern="*.csv")
    colNamesKHW  = c("timestamp", "gridToBattery", "gridToConsumers", "PVToBattery", "PVToGrid", "PVToConsumers", "batteryToConsumers", "batteryToGrid", "gensetToConsumers", "gensetToBattery", "gas")
    datKWH = read.csv(grep("main", lstData, value = TRUE), skip = 2, header = FALSE, col.names = colNamesKHW, sep = ",")    
}

solarData = function(){
    setwd("F:\\Users\\Janis\\VIKA\\solR\\data\\solar\\")
    lstData = list.files(pattern="*.csv")
    colNamesKHW  = c("timestamp", "gridToBattery", "gridToConsumers", "PVToBattery", "PVToGrid", "PVToConsumers", "batteryToConsumers", "batteryToGrid", "gensetToConsumers", "gensetToBattery", "gas")
    datKWH = read.csv(grep("kwh", lstData, value = TRUE), skip = 2, header = FALSE, col.names = colNamesKHW, sep = ",")    
}


year = 2019
month = formatC(seq(1, 12), width=2, flag=0)
day = formatC(seq(1, 31), width=2, flag=0)

interpretFilename = function(filename){
    # input: name of the date file
    # output: you know which day month and year it is by parsing filename
    # NEVERMIND THIS IS ATROCIOUS WAY TO IMPLEMENT IT CUZ
    # CONSIDER IN 0101 THERE IS 01 AND ALSO 10
    # TO BE DELETED
    #define possible patterns
    years = 2019
    months = formatC(seq(1, 12), width=2, flag=0)
    days = formatC(seq(1, 31), width=2, flag=0)
    
    year = patternSearch(filename, years)
    month = patternSearch(filename, months)
    day = patternSearch(filename, days)
    return(c(year, month, day))
}

fixDatetime = function(data){
    # convert timestamp class from factor to POSIXct
    data$timestamp <- as.POSIXct(strptime(data$timestamp, format="%Y-%m-%d %H:%M:%S"))
    # replace NA values with 0
    data[is.na(data)] <- 0
    return(data)
}

datKWH = solarData()
datKWH = fixDatetime(datKWH)

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