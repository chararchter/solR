# to get all libraries at once just install the whole tidyverse
library(ggplot2)
library(lubridate)
library(dplyr)
library(gridExtra) # combining 2 plots together in a grid

default = "F:\\Users\\Janis\\VIKA\\solR\\"

importData = function(whichData, id, colNames, skipCount){
    setwd(paste(default, "data\\", whichData, "\\", sep=""))
    # this is temporary solution until it gets clear if data in LU is identical to VRM
    # 2 precious previous lines then gonna migrate to howMuchFiles and here will be just import
    # difference in if main is that column names are not specified
    if (id == "main"){
        data = read.csv(grep(id, lstData, value = TRUE), skip = skipCount, header = FALSE, sep = ",")
    }
    else {
    data = read.csv(grep(id, lstData, value = TRUE), skip = skipCount, header = FALSE, col.names = colNames, sep = ",")
    return(data)
    }
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
    # Summarize gridToBattery by 2 hours in a 2 days interval
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



# datKWH = importData("solar", "kwh", colNamesKWH, 2)
# datKWH = fixDatetime(datKWH)
# 
# # datSol = importData("solar", "main", colNamesKWH, 2)
# # datSol = fixDatetime(datSol)
# 

# datMeteo = importData("meteo", "T000000", colNamesMeteo, 2)
# datMeteo = fixDatetime(datMeteo)
# datMeteo = mergeData("meteo")
# 
# # Common plot theme
# sharedTheme = theme_minimal()
# # Change the appearance of the main title
# sharedTheme = sharedTheme + theme(plot.title = element_text(size=18, face="bold",margin = margin(10, 0, 10, 0)))
# sharedAxis = xlab("Time")
# 
# timestamp = datKWH$timestamp
# gridToBattery = datKWH$gridToBattery
# gridToConsumers = datKWH$gridToConsumers

setwd(paste(default, "code\\", sep=""))

if(!exists("colNames", mode="function")) source("colNames.R")

#######################
# plots
#######################
# setwd(paste(default, "plots\\", sep=""))
# weekStats(datKWH, timestamp, gridToBattery)

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