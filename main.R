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
source("colNames.R")
source("processData.R")
source("parseCol.R")
source("integrate.R")

#######################
# import data
#######################

# datKWH = importData("solar", "kwh", 2)
# datKWH = fixDatetime(datKWH)

# datMeteo = mergeData("meteo", "T000000")

datSol = importData("solar", "main", 3)
datSol = fixDatetime(datSol)

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
    # print("###")
    # print(solName)
    colIndex = which( colnames(datSol)==solName )
    solname = interpretSolPanel(solName)
    # print(solname)
    
    var = produceStr(solname)
    
    # intrval1 = c(min(datSol$timestamp), min(datSol$timestamp) + days(7))
    # intrval2 = toString(interval(intrval1[1], intrval1[2]))
    
    intrval1 = c(min(datSol$timestamp) + days(29) + hours(8), min(datSol$timestamp) + days(30) - hours(7))
    intrval2 = toString(interval(intrval1[1], intrval1[2]))
    
    # intrval1 = c(min(datSol$timestamp), min(datSol$timestamp) + days(31))
    # intrval2 = toString(interval(intrval1[1], intrval1[2]))

    timestamp = datSol$timestamp
    solVar = datSol[, colIndex]
    datTemp = data.frame(timestamp, solVar)

    plotSol <- datSol %>% ggplot(aes(x = timestamp, y = solVar)) + geom_line() +
        sharedTheme +
        coord_cartesian(xlim = intrval1) +
        sharedAxis + ylab(var["labelSolVar"]) +
        ggtitle(paste(var["panelVerbose"], intrval2))
    ggsave(paste("sol", var["panel"], var["measurement"], ".pdf",sep=""),
           width = width, height = height, units = "cm")
    
    # Summarize function
    # smry = datTemp %>% group_by(x2=floor_date(timestamp, "1 day")) %>%
    #     summarize(y2=sum(solVar))
    # datTemp %>% group_by(x2=floor_date(timestamp, "1 day")) %>%
    #     summarize(y2=sum(solVar)) %>%
    #     ggplot(aes(x = x2, y = y2)) + geom_point() + sharedTheme + sharedAxis + ylab(var["labelSolVar"]) +
    #     ggtitle(paste("Summary:", var["panelVerbose"], intrval2))
    #     ggsave(paste("sol", var["panel"], var["measurement"], "sumDay.pdf",sep=""),
    #         width = width, height = height, units = "cm")
    # datTemp %>% group_by(x2=floor_date(timestamp, "1 week")) %>%
    #     summarize(y2=sum(solVar)) %>%
    #     ggplot(aes(x = x2, y = y2)) + geom_point() + sharedTheme + sharedAxis + ylab(var["labelSolVar"]) +
    #     ggtitle(paste("Summary:", var["panelVerbose"], intrval2))
    #     ggsave(paste("sol", var["panel"], var["measurement"], "sumWeek.pdf",sep=""),
    #         width = width, height = height, units = "cm")
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

# last peak
# intrval1 = c(min(datSol$timestamp) + days(29) + hours(8), min(datSol$timestamp) + days(30) - hours(7))
# intrval2 = toString(interval(intrval1[1], intrval1[2]))

# intrval1 = c(min(datSol$timestamp), min(datSol$timestamp) + days(31))
# intrval2 = toString(interval(intrval1[1], intrval1[2]))
# 
# plotSol = ggplot(datSol, aes(x = datSol$timestamp, y = datSol$solD90LG_PV_W)) + geom_line() +
#     sharedTheme +
#     coord_cartesian(xlim = intrval1) +
#     sharedAxis + ylab(var["labelSolVar"]) +
#     ggtitle(paste(var["panelVerbose"], intrval2))
# ggsave(paste("sol", var["panel"], var["measurement"], ".pdf",sep=""), width = width, height = height, units = "cm")


# if (solname["unit"]=="W"){
#     kWhMonth = sumMonth(datTemp, testsolName)
#     sumDay = integrateInterval2(date(min(datTemp$timestamp)), date(max(datTemp$timestamp)), days(1), datTemp, testsolName)
#     sumWeek = integrateInterval2(date(min(datTemp$timestamp)), date(max(datTemp$timestamp)), days(7), datTemp, testsolName)
#     # sumDay = integrateInterval(date(min(datTemp$timestamp)), days(1), datTemp, testsolName)
#     # sumWeek = integrateInterval(date(min(datTemp$timestamp)), days(7), datTemp, testsolName)
#     print(kWhMonth)
# }
# 
# p = ggplot(sumDay, aes(x=day, y=value, color = variable)) +
#     geom_point(aes(y = D90LG, col = "D90LG")) +
#     theme_minimal() + xlab("Day") + ylab("Wh") + ggtitle("Power generated by each solar panel")
# ggsave(paste("sol", var["panel"], var["measurement"], "sumDay.pdf",sep=""), width = width, height = height, units = "cm")

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

i = 0
for (solName in solNames){
    for (type in types){
        for (unit in units){
            for (device in devices){
                # print(paste(solName, type, device, unit, sep=""))
                solname = paste(solName, type, device, unit, sep="")

                datTemp = pltMonth(solname)
                solnameLst = interpretSolPanel(solname)
                if (solnameLst["device"] == "PV" & solnameLst["unit"] == "W"){
                    print(solname)
                    print("####")
                    kWhMonth = sumMonth(datTemp, solname)
                    print(kWhMonth)
                    # sumDay = integrateInterval(date(min(datTemp$timestamp)), days(1), datTemp, solname)
                    # sumWeek = integrateInterval(date(min(datTemp$timestamp)), days(7), datTemp, solname)
                    sumDay = integrateInterval2(date(min(datTemp$timestamp)), date(max(datTemp$timestamp)), days(1), datTemp, solname)
                    sumWeek = integrateInterval2(date(min(datTemp$timestamp)), date(max(datTemp$timestamp)), days(7), datTemp, solname)

                    if (i == 0){
                    sumDays = sumDay
                    sumWeeks = sumWeek
                    i = i + 1
                    } else{
                        sumDays = bind_cols(sumDays, sumDay)
                        sumWeeks = bind_cols(sumWeeks, sumWeek)
                    }
                }

            }
        }
    }
}

ind <- seq(3, ncol(sumDays), by=2) # indices of columns to remove: every 3rd column starting from 1
sumDays = sumDays[, -ind]
sumWeeks = sumWeeks[, -ind]
print(head(sumDays))
print(sumWeeks)

p = ggplot(sumDays, aes(x=day, y=value, color = variable)) +
    geom_point(aes(y = D13JA, col = "D13JA")) +
    geom_point(aes(y = D13LG, col = "D13LG")) +
    geom_point(aes(y = D40JA, col = "D40JA")) +
    geom_point(aes(y = D40LG, col = "D40LG")) +
    geom_point(aes(y = D40JA, col = "D90JA")) +
    geom_point(aes(y = D40LG, col = "D90LG")) +
    theme_minimal() + xlab("Day") + ylab("Wh") + ggtitle("Power generated by each solar panel")
ggsave("dayKWH_D.pdf", width = width, height = height, units = "cm")

p = ggplot(sumDays, aes(x=day, y=value, color = variable)) +
    geom_point(aes(y = A13JA, col = "A13JA")) +
    geom_point(aes(y = A13LG, col = "A13LG")) +
    geom_point(aes(y = D13JA, col = "D13JA")) +
    geom_point(aes(y = D13LG, col = "D13LG")) +
    geom_point(aes(y = A13JA1, col ="R13JA")) +
    geom_point(aes(y = R13LG, col = "R13LG")) +
    theme_minimal() + xlab("Day") + ylab("Wh") + ggtitle("Power generated by each solar panel")
ggsave("dayKWH_13.pdf", width = width, height = height, units = "cm")

p = ggplot(sumDays, aes(x=day, y=value, color = variable)) +
    geom_point(aes(y = A13JA, col = "A13JA")) +
    geom_point(aes(y = A13LG, col = "A13LG")) +
    theme_minimal() + xlab("Day") + ylab("Wh") + ggtitle("Power generated by each solar panel")
ggsave("dayKWH_A13.pdf", width = width, height = height, units = "cm")

p = ggplot(sumDays, aes(x=day, y=value, color = variable)) +
    geom_point(aes(y = D13JA, col = "D13JA")) +
    geom_point(aes(y = D13LG, col = "D13LG")) +
    theme_minimal() + xlab("Day") + ylab("Wh") + ggtitle("Power generated by each solar panel")
ggsave("dayKWH_D13.pdf", width = width, height = height, units = "cm")

# week
p = ggplot(sumWeeks, aes(x=day, y=value, color = variable)) +
    geom_point(aes(y = D13JA, col = "D13JA")) +
    geom_point(aes(y = D13LG, col = "D13LG")) +
    geom_point(aes(y = D40JA, col = "D40JA")) +
    geom_point(aes(y = D40LG, col = "D40LG")) +
    geom_point(aes(y = D90JA, col = "D90JA")) +
    geom_point(aes(y = D90LG, col = "D90LG")) +
    theme_minimal() + xlab("Day") + ylab("Wh") + ggtitle("Power generated by each solar panel")
ggsave("weekKWH_D.pdf", width = width, height = height, units = "cm")

p = ggplot(sumWeeks, aes(x=day, y=value, color = variable)) +
    geom_point(aes(y = A13JA, col = "A13JA")) +
    geom_point(aes(y = A13LG, col = "A13LG")) +
    geom_point(aes(y = D13JA, col = "D13JA")) +
    geom_point(aes(y = D13LG, col = "D13LG")) +
    geom_point(aes(y = A13JA1, col ="R13JA")) +
    geom_point(aes(y = R13LG, col = "R13LG")) +
    theme_minimal() + xlab("Day") + ylab("Wh") + ggtitle("Power generated by each solar panel")
ggsave("weekKWH_13.pdf", width = width, height = height, units = "cm")

p = ggplot(sumWeeks, aes(x=day, y=value, color = variable)) +
    geom_point(aes(y = A13JA, col = "A13JA")) +
    geom_point(aes(y = A13LG, col = "A13LG")) +
    theme_minimal() + xlab("Day") + ylab("Wh") + ggtitle("Power generated by each solar panel")
ggsave("weekKWH_A13.pdf", width = width, height = height, units = "cm")

p = ggplot(sumWeeks, aes(x=day, y=value, color = variable)) +
    geom_point(aes(y = D13JA, col = "D13JA")) +
    geom_point(aes(y = D13LG, col = "D13LG")) +
    theme_minimal() + xlab("Day") + ylab("Wh") + ggtitle("Power generated by each solar panel")
ggsave("weekKWH_D13.pdf", width = width, height = height, units = "cm")

base <- ggplot(sumDays, aes(x = day, y = D40JA)) +
    geom_blank() +
    xlab("Day") +
    ylab("Wh")