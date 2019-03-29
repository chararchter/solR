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

integrateInterval = function(lowerLimit, delta_t, datTemp, solName){
    # Defines lower and upper limits for trapezoidArea() integral.
    # Output - data frame, where "time" - lower limit of interval;
    # "sumkWh"  - value of area under the curve in that interval.
    
    solname = interpretSolPanel(solName)
    var = produceStr(solname)
    
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
        # print(paste(interval(lowerLimit, upperLimit),"   ", "kWh =", format(kWh, digits = 2, nsmall=2), sep = " "))

        intLength = int_length(interval(timestamp[strtIndex], timestamp[endIndex]))
        x[i] <- date(timestamp[strtIndex])
        y[i] <- kWh
        
        lowerLimit =  upperLimit
        upperLimit = upperLimit + delta_t
        i = i + 1
    }
    z = toString(var["panel"])
    # sumInt = data.frame("time" = x, "sumkWh" = y)
    sumInt = data.frame("day" = x, "solVar" = y)
    colnames(sumInt)[2] <- toString(var["panel"])
    print(sumInt)
    return(sumInt)
}



sumMonth = function(datTemp, solName){
    # Input - data frame where x - timestamp, y - solVar;
    # Output - value of area under the curve in a month interval used for reference
    # to check if it equals sum of kWh integrated by day in same month integral
    solname = interpretSolPanel(solName)
    var = produceStr(solname)
    timestamp = datTemp$timestamp
    
    t = trapezoidArea(timestamp, datTemp$solVar)
    kWhMonth = t /3600
    print(solName)
    print("Reference for a month")
    print(paste(interval(date(min(timestamp)), date(max(timestamp))),"   ", "kWh =", format(kWhMonth, digits = 2, nsmall=2), sep = " "))
    return(kWhMonth)
}


produceStr = function(solname){
    # Input - solname after interpretSolPanel(solName)
    # Output - combines useful strings of different solName parts to be used as titles, axis, and logical tests
    
    panel = paste(toString(solname["dir"]), toString(solname["degree"]), toString(solname["type"]), sep = "")
    panelVerbose = paste("Solar panel ", panel, sep = "")
    labelSolVar = paste(whichDevice(solname["device"]), whichMeasure(solname["unit"]), ", ", solname["unit"], sep = "")
    measurement = paste("_", solname["device"], solname["unit"], sep = "")
    
    parameters = c(panel, panelVerbose, labelSolVar, measurement)
    names(parameters) = c("panel", "panelVerbose", "labelSolVar", "measurement")
    return(parameters)
}



pltMonth = function(solName){
    colIndex = which( colnames(datSol)==solName )
    solname = interpretSolPanel(solName)
    # print(solname)
    
    var = produceStr(solname)
    
    # intrval1 = c(min(datSol$timestamp), min(datSol$timestamp) + days(1))
    # intrval2 = toString(interval(intrval1[1], intrval1[2]))
    
    intrval1 = c(min(datSol$timestamp) + days(28), min(datSol$timestamp) + days(29))
    intrval2 = toString(interval(intrval1[1], intrval1[2]))

    timestamp = datSol$timestamp
    solVar = datSol[, colIndex]
    datTemp = data.frame(timestamp, solVar)

    # smry = datTemp %>% group_by(x2=floor_date(timestamp, "1 day")) %>%
    #     summarize(y2=sum(solVar))
    
    # plotSol <- datSol %>% ggplot(aes(x = timestamp, y = solVar)) + geom_line() +
    #     sharedTheme +
    #     coord_cartesian(xlim = intrval1) +
    #     sharedAxis + ylab(var["labelSolVar"]) +
    #     ggtitle(paste(var["panelVerbose"], intrval2))
    # ggsave(paste("sol", var["panel"], var["measurement"], ".pdf",sep=""), width = width, height = height, units = "cm")
    # 
    # smry = datTemp %>% group_by(x2=floor_date(timestamp, "1 day")) %>%
    #     summarize(y2=sum(solVar))
    # 
    # datTemp %>% group_by(x2=floor_date(timestamp, "1 day")) %>%
    #     summarize(y2=sum(solVar)) %>%
    # ggplot(aes(x = x2, y = y2)) + geom_point() + sharedTheme + sharedAxis + ylab(var["labelSolVar"]) +
    #     ggtitle(paste("Summary:", var["panelVerbose"], intrval2))
    # ggsave(paste("sol", var["panel"], var["measurement"], "sumDay.pdf",sep=""), width = width, height = height, units = "cm")
    # 
    # datTemp %>% group_by(x2=floor_date(timestamp, "1 week")) %>%
    #     summarize(y2=sum(solVar)) %>%
    #     ggplot(aes(x = x2, y = y2)) + geom_point() + sharedTheme + sharedAxis + ylab(var["labelSolVar"]) +
    #     ggtitle(paste("Summary:", var["panelVerbose"], intrval2))
    # ggsave(paste("sol", var["panel"], var["measurement"], "sumWeek.pdf",sep=""), width = width, height = height, units = "cm")
    
    return(datTemp)
}


###########################
# plots solar data
###########################

types = c('JA','LG')
devices = c('_Bat', '_PV_')
units = c('V', 'A', 'W')

testsolName = "solD90LG_PV_W"
datTemp = pltMonth(testsolName)
solname = interpretSolPanel(testsolName)
var = produceStr(solname)

intrval1 = c(min(datSol$timestamp) + days(29) + hours(8), min(datSol$timestamp) + days(30) - hours(7))
intrval2 = toString(interval(intrval1[1], intrval1[2]))

plotSol = ggplot(datSol, aes(x = datSol$timestamp, y = datSol$solD90LG_PV_W)) + geom_line() +
    sharedTheme +
    coord_cartesian(xlim = intrval1) +
    sharedAxis + ylab(var["labelSolVar"]) +
    ggtitle(paste(var["panelVerbose"], intrval2))
ggsave(paste("sol", var["panel"], var["measurement"], ".pdf",sep=""), width = width, height = height, units = "cm")


if (solname["unit"]=="W"){
    kWhMonth = sumMonth(datTemp, testsolName)
    sumDay = integrateInterval(date(min(datTemp$timestamp)), days(1), datTemp, testsolName)
    sumWeek = integrateInterval(date(min(datTemp$timestamp)), days(1), datTemp, testsolName)
    print(kWhMonth)
}

# sumWeeks
# if (solname["unit"]=="W"){
#     ergh = pltSum(datTemp, "solD40JA_PV_W")
# }

# print(produceStr(interpretSolPanel("solD40JA_PV_W")))
# pltMonth("solD40LG_PV_W")


# disable as it produces 60 plots and runs slow
# for (solName in solNames){
#     for (unit in units){
#         for (device in devices){
#             for (type in types){
#                 # print(paste(solName, type, device, unit, sep=""))
#                 solname = paste(solName, type, device, unit, sep="")
#                 pltMonth(solname)
#                 
#             }
#         }
#     }
# }

# i = 0
# for (solName in solNames){
#     for (type in types){
#         for (unit in units){
#             for (device in devices){
#                 # print(paste(solName, type, device, unit, sep=""))
#                 solname = paste(solName, type, device, unit, sep="")
# 
#                 datTemp = pltMonth(solname)
#                 solnameLst = interpretSolPanel(solname)
#                 if (solnameLst["device"] == "PV" & solnameLst["unit"] == "W"){
#                     print(solname)
#                     print("####")
#                     kWhMonth = sumMonth(datTemp, solname)
#                     print(kWhMonth)
#                     sumDay = integrateInterval(date(min(datTemp$timestamp)), days(1), datTemp, solname)
#                     sumWeek = integrateInterval(date(min(datTemp$timestamp)), days(7), datTemp, solname)
# 
#                     if (i == 0){
#                     sumDays = sumDay
#                     sumWeeks = sumWeek
#                     i = i + 1
#                     } else{
#                         sumDays = bind_cols(sumDays, sumDay)
#                         sumWeeks = bind_cols(sumWeeks, sumWeek)
#                     }
#                 }
# 
#             }
#         }
#     }
# }
# 
# ind <- seq(3, ncol(sumDays), by=2) # indices of columns to remove: every 3rd column starting from 1
# sumDays = sumDays[, -ind]
# sumWeeks = sumWeeks[, -ind]
# print(head(sumDays))
# print(sumWeeks)
# 
# p = ggplot(sumDays, aes(x=day, y=value, color = variable)) +
#     geom_point(aes(y = D13JA, col = "D13JA")) +
#     geom_point(aes(y = D13LG, col = "D13LG")) +
#     geom_point(aes(y = D40JA, col = "D40JA")) +
#     geom_point(aes(y = D40LG, col = "D40LG")) +
#     geom_point(aes(y = D40JA, col = "D90JA")) +
#     geom_point(aes(y = D40LG, col = "D90LG")) +
#     theme_minimal() + xlab("Day") + ylab("kWh") + ggtitle("Power generated by each solar panel")
# ggsave("dayKWH_D.pdf", width = width, height = height, units = "cm")
# 
# p = ggplot(sumDays, aes(x=day, y=value, color = variable)) +
#     geom_point(aes(y = A13JA, col = "A13JA")) +
#     geom_point(aes(y = A13LG, col = "A13LG")) +
#     geom_point(aes(y = D13JA, col = "D13JA")) +
#     geom_point(aes(y = D13LG, col = "D13LG")) +
#     geom_point(aes(y = A13JA1, col ="R13JA")) +
#     geom_point(aes(y = R13LG, col = "R13LG")) +
#     theme_minimal() + xlab("Day") + ylab("kWh") + ggtitle("Power generated by each solar panel")
# ggsave("dayKWH_13.pdf", width = width, height = height, units = "cm")
# 
# p = ggplot(sumDays, aes(x=day, y=value, color = variable)) +
#     geom_point(aes(y = A13JA, col = "A13JA")) +
#     geom_point(aes(y = A13LG, col = "A13LG")) +
#     theme_minimal() + xlab("Day") + ylab("kWh") + ggtitle("Power generated by each solar panel")
# ggsave("dayKWH_A13.pdf", width = width, height = height, units = "cm")
# 
# p = ggplot(sumDays, aes(x=day, y=value, color = variable)) +
#     geom_point(aes(y = D13JA, col = "D13JA")) +
#     geom_point(aes(y = D13LG, col = "D13LG")) +
#     theme_minimal() + xlab("Day") + ylab("kWh") + ggtitle("Power generated by each solar panel")
# ggsave("dayKWH_D13.pdf", width = width, height = height, units = "cm")
# 
# # week
# p = ggplot(sumWeeks, aes(x=day, y=value, color = variable)) +
#     geom_point(aes(y = D13JA, col = "D13JA")) +
#     geom_point(aes(y = D13LG, col = "D13LG")) +
#     geom_point(aes(y = D40JA, col = "D40JA")) +
#     geom_point(aes(y = D40LG, col = "D40LG")) +
#     geom_point(aes(y = D40JA, col = "D90JA")) +
#     geom_point(aes(y = D40LG, col = "D90LG")) +
#     theme_minimal() + xlab("Day") + ylab("kWh") + ggtitle("Power generated by each solar panel")
# ggsave("weekKWH_D.pdf", width = width, height = height, units = "cm")
# 
# p = ggplot(sumWeeks, aes(x=day, y=value, color = variable)) +
#     geom_point(aes(y = A13JA, col = "A13JA")) +
#     geom_point(aes(y = A13LG, col = "A13LG")) +
#     geom_point(aes(y = D13JA, col = "D13JA")) +
#     geom_point(aes(y = D13LG, col = "D13LG")) +
#     geom_point(aes(y = A13JA1, col ="R13JA")) +
#     geom_point(aes(y = R13LG, col = "R13LG")) +
#     theme_minimal() + xlab("Day") + ylab("kWh") + ggtitle("Power generated by each solar panel")
# ggsave("weekKWH_13.pdf", width = width, height = height, units = "cm")
# 
# p = ggplot(sumWeeks, aes(x=day, y=value, color = variable)) +
#     geom_point(aes(y = A13JA, col = "A13JA")) +
#     geom_point(aes(y = A13LG, col = "A13LG")) +
#     theme_minimal() + xlab("Day") + ylab("kWh") + ggtitle("Power generated by each solar panel")
# ggsave("weekKWH_A13.pdf", width = width, height = height, units = "cm")
# 
# p = ggplot(sumWeeks, aes(x=day, y=value, color = variable)) +
#     geom_point(aes(y = D13JA, col = "D13JA")) +
#     geom_point(aes(y = D13LG, col = "D13LG")) +
#     theme_minimal() + xlab("Day") + ylab("kWh") + ggtitle("Power generated by each solar panel")
# ggsave("weekKWH_D13.pdf", width = width, height = height, units = "cm")
# 
# base <- ggplot(sumDays, aes(x = day, y = D40JA)) +
#     geom_blank() +
#     xlab("Day") +
#     ylab("kWh")

