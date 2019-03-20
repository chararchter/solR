# to get all libraries at once just install the whole tidyverse
library(ggplot2)
library(lubridate)
library(dplyr)
library(DescTools) # for trapezoid integrating
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
    
    # intrval1 = c(min(datSol$timestamp), min(datSol$timestamp) + days(1))
    x = min(datSol$timestamp) + days(21)
    intrval1 = c(x, x + days(1))
    intrval2 = toString(interval(intrval1[1], intrval1[2]))

    timestamp = datSol$timestamp
    solVar = datSol[, colIndex]
    
    datTemp = data.frame(timestamp, solVar)
    # write.csv(datTemp, file = "datTemp.csv")
    
    
    if (solname["unit"]=="W"){
        # print(head(datTemp))
        # areaUnderTheCurve = AUC(datTemp$timestamp, datTemp$solVar, method = "trapezoid", na.rm = FALSE)
        # print(areaUnderTheCurve)
        
        x = timestamp
        y = solVar
        t = 0
        s = 0
        
        for (i in 1:(length(x)-1)){
            deltx = x[i+1] - x[i]
            yvid = (y[i] + y[i+1])/2
            s[i] = deltx * yvid
            t = t+s[i]
        }
        kwHmonth = t /3600
        
        print("~~~~~~~~~~~~~~~~~~~~~")
        print(paste(solName,"    ", "kWh =", kwHmonth, sep = " "))
        print("~~~~~~~~~~~~~~~~~~~~~")
        
        # timestamp = datSol$timestamp
        # nor = min(timestamp)
        # i = 0
        
        # while (interval(date(nor), (date(nor) + days(7))) %within% interval(date(min(timestamp)), (date(max(timestamp))))) {
        #     print("viens")
        #     # print(length(interval(date(nor), (date(nor) + days(7)))))
        #     # strt = which(nor == timestamp)[[1]]
        #     # print(timestamp[toString(nor):toString(nor + days(7))])
        #     # match(c(nor, nor+days(7)),timestamp)
        #     
        #     dd = nor + days(7)
        #     print(date(dd))
        #     t = 0
        #     s = 0
        # 
        #     for (i in strt:endidx){
        #         deltx = x[i+1] - x[i]
        #         yvid = (y[i] + y[i+1])/2
        #         s[i] = deltx * yvid
        #         t = t+s[i]
        #     }
        #     print(t)
        #     
        #     nor =  nor + days(7)
        #     # i = i + 1
        # }
        
    }
    

    smry = datTemp %>% group_by(x2=floor_date(timestamp, "1 day")) %>%
        summarize(y2=sum(solVar))
    
    plotSol <- datSol %>% ggplot(aes(x = timestamp, y = solVar)) + geom_line() +
        sharedTheme +
        coord_cartesian(xlim = intrval1) +
        sharedAxis + ylab(labelSolVar) +
        ggtitle(paste(panelVerbose, intrval2))
    ggsave(paste("sol", panel, measurement, ".pdf",sep=""), width = width, height = height, units = "cm")
    
    datTemp %>% group_by(x2=floor_date(timestamp, "1 day")) %>%
        summarize(y2=sum(solVar)) %>%
    ggplot(aes(x = x2, y = y2)) + geom_point() + sharedTheme + sharedAxis + ylab(labelSolVar) +
        ggtitle(paste("Summary:", panelVerbose, intrval2))
    ggsave(paste("sol", panel, measurement, "sumDay.pdf",sep=""), width = width, height = height, units = "cm")

    datTemp %>% group_by(x2=floor_date(timestamp, "1 week")) %>%
        summarize(y2=sum(solVar)) %>%
        ggplot(aes(x = x2, y = y2)) + geom_point() + sharedTheme + sharedAxis + ylab(labelSolVar) +
        ggtitle(paste("Summary:", panelVerbose, intrval2))
    ggsave(paste("sol", panel, measurement, "sumWeek.pdf",sep=""), width = width, height = height, units = "cm")
    
    return(smry)
    # return(datTemp)
    # return(c(datTemp, smry))
    
    # lst = c(solName, kWhmonth)
    # return(lst)
}



###########################
# plots solar data
###########################


types = c('JA','LG')
devices = c('_Bat', '_PV_')
units = c('V', 'A', 'W')


kWhmonths = data.frame(solVar = 0, value = 0)

# sumry = pltMonth("solD40JA_PV_W")
smry = pltMonth("solD40JA_PV_W")
# print(smry)
# 
# kWhmonths <- rbind(kWhmonth, data.frame(x = kWhMonth[1], kWhMonth[2]))


# print(sumry)

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