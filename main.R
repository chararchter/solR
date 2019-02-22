# to get all libraries at once just install the whole tidyverse
library(ggplot2)
library(lubridate)
library(dplyr)
library(gridExtra) # combining 2 plots together in a grid

default = "F:\\Users\\Janis\\VIKA\\solR\\"

importData = function(whichData, id, skipCount){
    setwd(paste(default, "data\\", whichData, "\\", sep=""))
    data = read.csv(grep(id, howMuchFiles(whichData), value = TRUE), skip = skipCount,
                    header = FALSE, col.names = colNames(id), sep = ",")
    return(data)
}

howMuchFiles = function(whichData){
    setwd(paste(default, "data\\", whichData, "\\", sep=""))
    lstData = list.files(pattern="*.csv")
    return(lstData)
}

fixDatetime = function(data){
    # convert timestamp class from factor to POSIXct
    data$timestamp = as.POSIXct(strptime(data$timestamp, format="%Y-%m-%d %H:%M:%S"))
    # replace NA values with 0
    data[is.na(data)] = 0
    return(data)
}

mergeData = function(id){
    dataLst = howMuchFiles(id)
    # create an empty data frame by removing all the rows from existent data frame cuz it has columns i want
    empty_df = importData(id, dataLst[1], colNamesMeteo, 1)
    empty_df = empty_df[FALSE,]
    
    for (i in 1:length(dataLst)){
        dfi = importData(id, dataLst[i], colNamesMeteo, 1)
        total = rbind(empty_df, dfi) 
        empty_df = total
    }
    # sort a data frame by date
    total$timestamp <- lubridate::as_datetime(total$timestamp)
    dplyr::arrange(total, timestamp)
    return(total)
}

pltWeekStats = function(data, timestamp, dependentVar, nor, i){
    # Summarize gridToBattery by 1 hours in a 2 days interval
    data %>% group_by(timestamp=floor_date(timestamp, "1 hour")) %>%
    summarize(gridToBattery=sum(gridToBattery))  %>%
    ggplot(aes(x = timestamp, y = gridToBattery)) + geom_point() + sharedTheme +
    sharedAxis + ylab("Grid To Battery, kWh") +
    ggtitle(paste('Grid to Battery ', toString(interval(date(nor), (date(nor) + days(2)))))) +
    coord_cartesian(xlim = c(nor, nor + days(2)))
    ggsave(paste('week', toString(i), '.pdf', sep=""), width = 29.7, height = 21.0, units = "cm")
}

weekStats = function(data, timestamp, dependentVar){
    setwd(paste(default, "plots\\", sep=""))
    nor = min(timestamp)
    i = 1
    while (interval(date(nor), (date(nor) + days(2))) %within% interval(date(min(timestamp)), (date(max(timestamp))))) {
        pltWeekStats(datKWH, timestamp, gridToBattery, nor, i)
        nor =  nor + days(2)
        i = i + 1
    }
}

setwd(paste(default, "code\\", sep=""))
if(!exists("colNames", mode="function")) source("colNames.R")

# datKWH = importData("solar", "kwh", 2)
# datKWH = fixDatetime(datKWH)

datSol = importData("solar", "main", 3)
datSol = fixDatetime(datSol)

# datMeteo = importData("meteo", "T000000", 1)
# datMeteo = mergeData("meteo")

# Common plot theme
sharedTheme = theme_minimal()
# Change the appearance of the main title
sharedTheme = sharedTheme + theme(plot.title = element_text(size=18, face="bold",margin = margin(10, 0, 10, 0)))
sharedAxis = xlab("Time")
# 
# timestamp = datKWH$timestamp
# gridToBattery = datKWH$gridToBattery
# gridToConsumers = datKWH$gridToConsumers

#######################
# plots
#######################
setwd(paste(default, "plots\\", sep=""))
# weekStats(datKWH, timestamp, gridToBattery)

# Plots for a month
# datSol %>% group_by(timestamp=floor_date(timestamp, "1 day")) %>%
#     summarize(solD40JA_BatV=sum(solD40JA_BatV)) %>%
#     ggplot(aes(x = timestamp, y = solD40JA_BatV)) + geom_point() +
#     sharedTheme +  coord_cartesian(xlim = c(min(datSol$timestamp), min(datSol$timestamp) + days(14))) +
#     ggtitle(paste('PV Voltage', toString(interval(min(datSol$timestamp), min(datSol$timestamp) + days(14)))))
#     ggsave("sol1.pdf", width = 29.7, height = 21.0, units = "cm")

datSol %>% ggplot(aes(x = timestamp, y = solD40JA_BatV)) + geom_point() +
    sharedTheme +  coord_cartesian(xlim = c(min(timestamp), min(timestamp) + days(31))) +
    ggtitle(paste('PV Voltage', toString(interval(min(timestamp), min(timestamp) + days(31)))))
ggsave("sol2.pdf", width = 29.7, height = 21.0, units = "cm")


# Plots for a month
# datKWH %>% group_by(timestamp=floor_date(timestamp, "2 hours")) %>%
#     summarize(gridToBattery=sum(gridToBattery)) %>%
#     ggplot(aes(x = timestamp, y = gridToBattery)) + geom_point() +
#     sharedTheme +  coord_cartesian(xlim = c(min(timestamp), min(timestamp) + months(1))) +
#     ggtitle(paste('Grid to Battery ', toString(interval(min(timestamp), min(timestamp) + months(1)))))
#     ggsave("r2.pdf", width = 29.7, height = 21.0, units = "cm")
    
# datKWH %>% group_by(timestamp=floor_date(timestamp, "day")) %>%
#     summarize(gridToBattery=sum(gridToBattery)) %>%
#     ggplot(aes(x = timestamp, y = gridToBattery)) + geom_point() + sharedTheme + sharedAxis + 
#     ggtitle(paste('Grid to Battery ', toString(interval(min(timestamp), min(timestamp) + months(1)))))
#     ggsave("r3.pdf", width = 29.7, height = 21.0, units = "cm")

# 
# plotKWH = geom_point(data = datKWH, aes(x = timestamp, y = gridToBattery, color = "red"))
# plotKWH2 = geom_point(data = datKWH, aes(x = timestamp, y = gridToConsumers, color = "green"))
# plotMeteo = geom_line(data = datMeteo, aes(x = datMeteo$timestamp, y = datMeteo$solarIrradiance/3000), color = "blue")
# 
# ggplot() +
#     plotKWH +
#     plotMeteo + 
#     # plotKWH2 +
#     sharedTheme +
#     ggtitle(paste('Grid to Battery ', toString(interval(min(timestamp), min(timestamp) + months(1))))) +
#     # grid.arrange(grobs = list(plotKWH, plotMeteo), ncol=1,nrow=2)
#     # grid.arrange(plotKWH, plotMeteo, ncol=1,nrow=2)
# ggsave("r6.pdf", width = 29.7, height = 21.0, units = "cm")
# 
# ggplot() +
#     plotKWH +
#     sharedTheme + sharedAxis + ylab("Grid To Battery, kWh") +
#     ggtitle(paste('Grid to Battery ', toString(interval(min(timestamp), min(timestamp) + months(1))))) +
# ggsave("rkwh.pdf", width = 29.7, height = 21.0, units = "cm")
# 
# ggplot() +
#     plotMeteo +
#     sharedTheme + sharedAxis + ylab("Solar Irradiance, W/m2") +
#     ggtitle(paste('Grid to Battery ', toString(interval(min(timestamp), min(timestamp) + months(1))))) +
# ggsave("rmeteo.pdf", width = 29.7, height = 21.0, units = "cm")