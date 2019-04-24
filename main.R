library(ggplot2)
library(lubridate)
library(dplyr)
library(wesanderson)
# library(DescTools) # for trapezoid integrating
# library(gridExtra) # combining 2 plots together in a grid
# library(gridGraphics) # cheat solution for only 'grobs' allowed in "gList" error I found on stack exchange
# library(pracma) # cubic spline

default = "F:\\Users\\Janis\\VIKA\\solR\\"
solNames = c("solD40", "solD13","solA13", "solR13", "solD90")
width = 15.45
height = 9.549
# Define extension in which to save plot, e.g. .pdf or .png
extension = ".png"

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

datSol = importData("solar", "main", 3)
datSol = fixDatetime(datSol)

# datSol = importDataRaw("solar", "main")
# indxSubSol = findSolChargerCol(datSol)
# subSol = datSol[,indxSubSol] # subset of datSol with Solar Charger & PV power columns
# subSol = renameSubSol(subSol, datSol[1])
# subSol = apply(subSol, 2, function(x) gsub("^$|^ $", NA, x)) # change empty values to NA
# subSol = as.data.frame(subSol)
# subSol = fixDatetime(subSol)

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
    colIndex = which( colnames(datSol)==solName )
    solname = interpretSolPanel(solName)
    var = produceStr(solname)
    intrval1 = c(min(datSol$timestamp) + days(1) + hours(8), min(datSol$timestamp) + days(21) - hours(7))
    intrval2 = toString(interval(intrval1[1], intrval1[2]))

    timestamp = datSol$timestamp
    solVar = datSol[, colIndex]
    datTemp = data.frame(timestamp, solVar)

    plotSol <- datTemp %>% ggplot(aes(x = timestamp, y = solVar)) + geom_line() +
        sharedTheme +
        # coord_cartesian(xlim = intrval1) +
        sharedAxis + ylab(var["labelSolVar"]) +
        # ggtitle(paste(var["panelVerbose"], date(min(datSol$timestamp)+days(21)) ))
    ggtitle(paste(var["panelVerbose"], date(min(datSol$timestamp)) ))
    ggsave(paste("sol", var["panel"], var["measurement"], extension, sep=""),
           width = width, height = height, units = "cm")
    return(datTemp)
}


###########################
# plots solar data
###########################

types = c('JA','LG')
devices = c('_Bat', '_PV_')
units = c('V', 'A', 'W')

i = 0
for (solName in solNames){
    for (type in types){
        for (unit in units){
            for (device in devices){
                solname = paste(solName, type, device, unit, sep="")
                solname2 = paste(" ", solName, type, device, unit, sep="")
                # datTemp = pltMonth(solname)
                solnameLst = interpretSolPanel(solname)
                # print(solnameLst)
                if (solnameLst["device"] == "PV" & solnameLst["unit"] == "W"){
                    print(solname)
                    datTemp = pltMonth(solname)
                    # print(head(datTemp))
                    kWhMonth = sumMonth(datTemp, solname)
                    print(kWhMonth)
                    sumDay = integrateInterval2(date(min(datTemp$timestamp)), date(max(datTemp$timestamp)), days(1), datTemp, solname)
                    sumWeek = integrateInterval2(date(min(datTemp$timestamp)), date(max(datTemp$timestamp)), days(7), datTemp, solname)
                # 
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

# cumulative integral
cumSumDays = cumsum(sumDays[,2:ncol(sumDays)])
cumSumWeeks = cumsum(sumWeeks[,2:ncol(sumWeeks)])
cumSumDays = bind_cols(sumDays[1], cumSumDays)
cumSumWeeks = bind_cols(sumWeeks[1], cumSumWeeks)

sumDays2 = data.frame("day"=sumDays$day, "panel"=rep("D40JA", each=nrow(sumDays)), "Wh"=sumDays$D40JA)
sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$day, "panel"=rep("D40LG", each=nrow(sumDays)), "Wh"=sumDays$D40LG))
sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$day, "panel"=rep("D13JA", each=nrow(sumDays)), "Wh"=sumDays$D13JA))
sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$day, "panel"=rep("D13LG", each=nrow(sumDays)), "Wh"=sumDays$D13LG))
sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$day, "panel"=rep("A13JA", each=nrow(sumDays)), "Wh"=sumDays$A13JA))
sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$day, "panel"=rep("A13LG", each=nrow(sumDays)), "Wh"=sumDays$A13LG))
sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$day, "panel"=rep("R13JA", each=nrow(sumDays)), "Wh"=sumDays$R13JA))
sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$day, "panel"=rep("R13LG", each=nrow(sumDays)), "Wh"=sumDays$R13LG))
sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$day, "panel"=rep("D90JA", each=nrow(sumDays)), "Wh"=sumDays$D90JA))
sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$day, "panel"=rep("D90LG", each=nrow(sumDays)), "Wh"=sumDays$D90LG))
as.factor(sumDays2$panel)

setwd(paste(default, "code\\", sep=""))
source("plots.R")