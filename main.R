library(ggplot2)
library(lubridate)
library(dplyr)
# library(DescTools) # for trapezoid integrating
# library(gridExtra) # combining 2 plots together in a grid
# library(gridGraphics) # cheat solution for only 'grobs' allowed in "gList" error I found on stack exchange
# library(pracma) # cubic spline

default = "F:\\Users\\Janis\\VIKA\\solR\\"
solNames = c("solD40", "solD13","solA13", "solR13", "solD90")
width = 29.7
height = 21.0

#######################
# import functions
#######################

setwd(paste(default, "code\\", sep=""))

if(!exists("importData", mode="function")) source("importData.R")
if(!exists("howMuchFiles", mode="function")) source("howMuchFiles.R")
if(!exists("fixDatetime", mode="function")) source("fixDatetime.R")
if(!exists("mergeData", mode="function")) source("mergeData.R")

if(!exists("colNames", mode="function")) source("colNames.R")
if(!exists("pltWeekStats", mode="function")) source("pltWeekStats.R")
if(!exists("weekStats", mode="function")) source("weekStats.R")

if(!exists("interpretSolPanel", mode="function")) source("interpretSolPanel.R")
if(!exists("searchPattern", mode="function")) source("searchPattern.R")
if(!exists("whichDevice", mode="function")) source("whichDevice.R")
if(!exists("whichMeasure", mode="function")) source("whichMeasure.R")

#######################
# import data
#######################

# datKWH = importData("solar", "kwh", 2)
# datKWH = fixDatetime(datKWH)

datSol = importData("solar", "main", 3)
datSol = fixDatetime(datSol)

# datMeteo = mergeData("meteo", "T000000")


#######################
# plots
#######################
setwd(paste(default, "plots\\", sep=""))

# Common plot theme
sharedTheme = theme_minimal()
# Change the appearance of the main title
sharedTheme = sharedTheme + theme(plot.title = element_text(size=18, face="bold",margin = margin(10, 0, 10, 0)))
sharedAxis = xlab("Time")

trapezoidArea = function(x, y){
    # Calculate the area under the curve formed by connecting all points by a direct line
    # (composite trapezoid rule). The curve must be given by vectors of xy-coordinates.
    # Output - numeric value of area under the curve.
    
    t = 0
    s = 0
    
    for (i in 1:(length(x)-1)){
        deltx = x[i+1] - x[i]
        yvid = (y[i] + y[i+1])/2
        s[i] = deltx * yvid
        t = t+s[i]
    }
    return(t)
}

integrateInterval = function(lowerLimit, delta_t, datTemp){
    # Defines lower and upper limits for trapezoidArea() integral.
    # Output - data frame, where "time" - lower limit of interval;
    # "sumkWh"  - value of area under the curve in that interval.
    
    timestamp = datTemp$timestamp
    upperLimit = lowerLimit + delta_t

    intrBig = (date(max(timestamp))-lowerLimit)
    intrSmall = (upperLimit - lowerLimit)
    itrTimes = floor(as.numeric(intrBig) / as.numeric(intrSmall))
    
    x <- as_datetime(itrTimes)
    y <- numeric(itrTimes)
    count = 0
    i = 1

    while (interval(lowerLimit, upperLimit) %within% interval(lowerLimit, date(max(timestamp)))) {
        strtIndex = which(date(as.POSIXct(timestamp)) == date(as.POSIXct(lowerLimit)))[1]
        endIndex = which(date(as.POSIXct(timestamp)) == date(as.POSIXct(upperLimit)))[1]
        datInt = datTemp[strtIndex:endIndex,]

        t = trapezoidArea(datInt$timestamp, datInt$solVar)
        kWh = t / 3600
        count = count + kWh
        print(paste(interval(lowerLimit, upperLimit),"   ", "kWh =", format(kWh, digits = 2, nsmall=2), sep = " "))

        intLength = int_length(interval(timestamp[strtIndex], timestamp[endIndex]))
        x[i] <- date(timestamp[strtIndex])
        y[i] <- kWh
        
        lowerLimit =  upperLimit
        upperLimit = upperLimit + delta_t
        i = i + 1
    }
    print(count)
    sumInt = data.frame("time" = x, "sumkWh" = y)
    return(sumInt)
}

pltMonth = function(solName){
    colIndex = which( colnames(datSol)==solName )
    solname = interpretSolPanel(solName)
    panel = paste(toString(solname["dir"]), toString(solname["degree"]), toString(solname["type"]), sep = "")
    panelVerbose = paste("Solar panel ", panel, sep = "")
    labelSolVar = paste(whichDevice(solname["device"]), whichMeasure(solname["unit"]), ", ", solname["unit"], sep = "")
    measurement = paste("_", solname["device"], solname["unit"], sep = "")
    
    intrval1 = c(min(datSol$timestamp), min(datSol$timestamp) + days(1))
    intrval2 = toString(interval(intrval1[1], intrval1[2]))
    
    timestamp = datSol$timestamp
    solVar = datSol[, colIndex]
    datTemp = data.frame(timestamp, solVar)
    
    
    if (solname["unit"]=="W"){
        t = trapezoidArea(datTemp$timestamp, datTemp$solVar)
        kwHmonth = t /3600
        print(solName)
        print(paste(interval(date(min(timestamp)), date(max(timestamp))),"   ", "kWh =", format(kwHmonth, digits = 2, nsmall=2), sep = " "))
        
        sumDays = integrateInterval(date(min(timestamp)), days(1), datTemp)
        sumWeeks = integrateInterval(date(min(timestamp)), days(7), datTemp)
        
        plotDaykWh <- sumDays %>% ggplot(aes(x = time, y = sumkWh)) + geom_point() +
            sharedTheme +
            sharedAxis + ylab("kWh") +
            ggtitle(paste(panelVerbose, "kWh"))
        ggsave(paste("sol", panel, measurement, "kwh.pdf",sep=""), width = width, height = height, units = "cm")
        
        plotWeekkWh <- sumWeeks %>% ggplot(aes(x = time, y = sumkWh)) + geom_point() +
            sharedTheme +
            sharedAxis + ylab("kWh") +
            ggtitle(paste(panelVerbose, "kWh"))
        ggsave(paste("sol", panel, measurement, "kwh.pdf",sep=""), width = width, height = height, units = "cm")
    }
    
    smry = datTemp %>% group_by(x2=floor_date(timestamp, "1 day")) %>%
        summarize(y2=sum(solVar))
    
    # plotDaykWh <- sumDays %>% ggplot(aes(x = x, y = y)) + geom_point() +
    #     sharedTheme +
    #     sharedAxis + ylab("kWh") +
    #     ggtitle(paste(panelVerbose, "kWh"))
    # ggsave(paste("sol", panel, measurement, "kwh.pdf",sep=""), width = width, height = height, units = "cm")
    
    # plotSol <- datSol %>% ggplot(aes(x = timestamp, y = solVar)) + geom_line() +
    #     sharedTheme +
    #     coord_cartesian(xlim = intrval1) +
    #     sharedAxis + ylab(labelSolVar) +
    #     ggtitle(paste(panelVerbose, intrval2))
    # ggsave(paste("sol", panel, measurement, ".pdf",sep=""), width = width, height = height, units = "cm")
    # 
    # datTemp %>% group_by(x2=floor_date(timestamp, "1 day")) %>%
    #     summarize(y2=sum(solVar)) %>%
    # ggplot(aes(x = x2, y = y2)) + geom_point() + sharedTheme + sharedAxis + ylab(labelSolVar) +
    #     ggtitle(paste("Summary:", panelVerbose, intrval2))
    # ggsave(paste("sol", panel, measurement, "sumDay.pdf",sep=""), width = width, height = height, units = "cm")
    # 
    # datTemp %>% group_by(x2=floor_date(timestamp, "1 week")) %>%
    #     summarize(y2=sum(solVar)) %>%
    #     ggplot(aes(x = x2, y = y2)) + geom_point() + sharedTheme + sharedAxis + ylab(labelSolVar) +
    #     ggtitle(paste("Summary:", panelVerbose, intrval2))
    # ggsave(paste("sol", panel, measurement, "sumWeek.pdf",sep=""), width = width, height = height, units = "cm")
    
    # return(smry)
    return(sumDays)
}


###########################
# plots solar data
###########################

types = c('JA','LG')
devices = c('_Bat', '_PV_')
units = c('V', 'A', 'W')

checkitout = pltMonth("solD40JA_PV_W")
print(checkitout)
# pltMonth("solD40LG_PV_W")
# pltMonth("solD90JA_PV_W")
# pltMonth("solD90LG_PV_W")

# disable as it produces 60 plots and runs slow
# for (solName in solNames){
#     for (unit in units){
#         for (device in devices){
#             for (type in types){
#                 # print(paste(solName, type, device, unit, sep=""))
#                 pltMonth(paste(solName, type, device, unit, sep=""))
#             }
#         }
#     }
# }