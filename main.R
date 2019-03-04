# to get all libraries at once just install the whole tidyverse
library(ggplot2)
library(lubridate)
library(dplyr)
# library(gridExtra) # combining 2 plots together in a grid
library(pracma)

default = "F:\\Users\\Janis\\VIKA\\solR\\"
solNames = c("solD40", "solD13","solA13", "solR13", "solD90")
width = 29.7
height = 21.0


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

mergeData = function(whichData, id){
    setwd(paste(default, "data\\", whichData, "\\", sep=""))
    dataLst = howMuchFiles(whichData)
    # print(dataLst)
    # create an empty data frame by removing all the rows from existent data frame cuz it has columns i want
    empty_df = read.csv(grep(id, dataLst[1], value = TRUE), skip = 1,
            header = FALSE, col.names = colNames(id), sep = ",")

    empty_df = empty_df[FALSE,]
    for (i in 1:length(dataLst)){
        dfi = read.csv(grep(id, dataLst[i], value = TRUE), skip = 1,
            header = FALSE, col.names = colNames(id), sep = ",")
        total = rbind(empty_df, dfi)
        empty_df = total
    }
    # sort a data frame by date
    total$timestamp <- lubridate::as_datetime(total$timestamp)
    dplyr::arrange(total, timestamp)
    return(total)
}

pltWeekStats = function(data, timestamp, varSol, nor, i){
    # Summarize gridToBattery by 1 hours in a 2 days interval
    data %>% group_by(timestamp=floor_date(timestamp, "1 hour")) %>%
    summarize(gridToBattery=sum(gridToBattery))  %>%
    ggplot(aes(x = timestamp, y = gridToBattery)) + geom_point() + sharedTheme +
    sharedAxis + ylab("Grid To Battery, kWh") +
    ggtitle(paste('Grid to Battery ', toString(interval(date(nor), (date(nor) + days(2)))))) +
    coord_cartesian(xlim = c(nor, nor + days(2)))
    ggsave(paste('week', toString(i), '.pdf', sep=""), width = 29.7, height = 21.0, units = "cm")
}

weekStats = function(data, timestamp, varSol){
    setwd(paste(default, "plots\\", sep=""))
    nor = min(timestamp)
    i = 1
    while (interval(date(nor), (date(nor) + days(2))) %within% interval(date(min(timestamp)), (date(max(timestamp))))) {
        pltWeekStats(data, timestamp, varSol, nor, i)
        nor =  nor + days(2)
        i = i + 1
    }
}

searchPattern = function(name, parameter){
    for (char in 1:length(name)){
        for (i in 1:length(parameter)){
            if (grepl(parameter[i], name[char])){
                return(parameter[i])
            }
        }
    }
}


interpretSolPanel = function(solPanel){
    #define possible patterns
    directions = c('D','A', 'R')
    types = c('JA','LG')
    degrees = c('13', '40', '90')
    devices = c('Bat', 'PV')
    units = c('V', 'A', 'W')
    
    dir = searchPattern(solPanel, directions)
    type = searchPattern(solPanel, types)
    degree = searchPattern(solPanel, degrees)
    device = searchPattern(solPanel, devices)
    unit = searchPattern(solPanel, units)
    
    parameters <- c(dir, degree, type, device, unit)
    names(parameters) <- c("dir", "degree", "type", "device", "unit")
    return(parameters)
}

whichDevice = function(device){
    if (device == "Bat"){
        return("Battery")
    }
    if (device == "PV"){
        return("Photovoltaic")
    }
}

whichMeasure = function(unit){    
    if (unit == "V")
        return(" voltage")
    if (unit == "A")
        return(" current")
    if (unit == "W")
        return(" power")
}


#######################
# import
#######################

setwd(paste(default, "code\\", sep=""))
if(!exists("colNames", mode="function")) source("colNames.R")

# datKWH = importData("solar", "kwh", 2)
# datKWH = fixDatetime(datKWH)

datSol = importData("solar", "main", 3)
datSol = fixDatetime(datSol)

datMeteo = mergeData("meteo", "T000000")


#######################
# plots
#######################
setwd(paste(default, "plots\\", sep=""))

# Common plot theme
sharedTheme = theme_minimal()
# Change the appearance of the main title
sharedTheme = sharedTheme + theme(plot.title = element_text(size=18, face="bold",margin = margin(10, 0, 10, 0)))
sharedAxis = xlab("Time")

pltMonth = function(solName){
    # there is a mistake in the next line. interpretSolPanel argument should change based on function argument but now it's static
    colIndex = which( colnames(datSol)==solName )
    solname = interpretSolPanel(solName)
    panel = paste(toString(solname["dir"]), toString(solname["degree"]), toString(solname["type"]), sep = "")
    panelVerbose = paste("Solar panel ", panel, sep = "")
    labelSolVar = paste(whichDevice(solname["device"]), whichMeasure(solname["unit"]), ", ", solname["unit"], sep = "")
    measurement = paste("_", solname["device"], solname["unit"], sep = "")
    
    intrval1 = c(min(datSol$timestamp), min(datSol$timestamp) + days(31))
    intrval2 = toString(interval(intrval1[1], intrval1[2]))
    width = 29.7
    height = 21.0
    
    solVar = datSol[, colIndex]
    datSol %>% ggplot(aes(x = timestamp, y = solVar)) + geom_point() +
        sharedTheme +  coord_cartesian(xlim = intrval1) +
        ggtitle(paste(panelVerbose, intrval2)) + sharedAxis + ylab(labelSolVar)
    ggsave(paste("sol", panel, measurement, ".pdf",sep=""), width = width, height = height, units = "cm")
}

pltMonth("solD40JA_BatV")

types = c('JA','LG')
devices = c('_Bat', '_PV_')
units = c('V', 'A', 'W')

# for (solName in solNames){
#     for (unit in units){
#         for (device in devices){
#             for (type in types){
#                 pltMonth(paste(solName, type, device, unit, sep=""))
#             }
#         }
#     }
# }

# intrval1 = c(min(datMeteo$timestamp) + days(7), min(datMeteo$timestamp) + hours(2))
# intrval1 = c(min(datMeteo$timestamp), min(datMeteo$timestamp) + days(31))
intrval1 = c(as_datetime("2019-01-19 07:00:00 UTC"), as_datetime("2019-01-19 07:00:00 UTC") + hours(10))

splains = spline(datMeteo$timestamp, datMeteo$solarIrradiance)

# Error: Aesthetics must be either length 1 or the same as the data (133920): x, y
# possible solution - plot spline as data series
# also need to convert x to asdatetime
datMeteo %>% ggplot(aes(x = datMeteo$timestamp, y = datMeteo$solarIrradiance)) + geom_point() +
    sharedTheme +  coord_cartesian(xlim = intrval1) +
    geom_line(data = data.frame(splains)) + 
    ggtitle("Meteo") + sharedAxis + ylab("Solar irradiance, W/m2")
ggsave("meteo.pdf", width = width, height = height, units = "cm")