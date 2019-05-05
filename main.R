library(ggplot2)
library(lubridate)
library(dplyr)
library(wesanderson)
# library(stringr)
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
tz(datSol$timestamp) = "UTC"
datSol$timestamp = with_tz(datSol$timestamp, tzone = "EET")

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

    # plotSol <- datTemp %>% ggplot(aes(x = timestamp, y = solVar)) + geom_line() +
    #     sharedTheme +
    #     # coord_cartesian(xlim = intrval1) +
    #     sharedAxis + ylab(var["labelSolVar"]) +
    #     # ggtitle(paste(var["panelVerbose"], date(min(datSol$timestamp)+days(21)) ))
    # ggtitle(paste(var["panelVerbose"], date(min(datSol$timestamp)) ))
    # ggsave(paste("sol", var["panel"], var["measurement"], extension, sep=""),
    #        width = width, height = height, units = "cm")
    return(datTemp)
}


###########################
# plots solar data
###########################

types = c('JA','LG')
devices = c('_Bat', '_PV_')
units = c('V', 'A', 'W')
col_headings = list("timestamp")

# solname = "solD40JA_PV_W"
# kWhMonth = sumMonth(datTemp, solname)
# datTemp = pltMonth(solname)
# sumHour = integrateIntervalH(min(datTemp$timestamp), max(datTemp$timestamp),
#     hours(1), datTemp, solname)

i = 0
for (solName in solNames){
    for (type in types){
        for (unit in units){
            for (device in devices){
                solname = paste(solName, type, device, unit, sep="")
                # solname2 = paste(" ", solName, type, device, unit, sep="")
                solnameLst = interpretSolPanel(solname)
                # print(solname)
                # print(solnameLst)
                if (solnameLst["device"] == "PV" & solnameLst["unit"] == "W"){
                    # print(solname)
                    # print(solnameLst)
                    datTemp = pltMonth(solname)
                    whMonth = sumMonth(datTemp, solname)
                    col_headings<-c(col_headings,solname)
                    sumHour = integrateIntervalH(datTemp$timestamp, datTemp$solVar, hours(1), solname)
                    # sumDay = integrateInterval2(date(min(datTemp$timestamp)), date(max(datTemp$timestamp)),
                    #                             days(1), datTemp, solname)
                    # sumWeek = integrateInterval2(date(min(datTemp$timestamp)), date(max(datTemp$timestamp)),
                    #                              days(7), datTemp, solname)
                    if (i == 0){
                    # subSol = datTemp
                    sumHours = sumHour
                    # sumDays = sumDay
                    # sumWeeks = sumWeek
                    date_ym = paste0(year(datTemp$timestamp[1]), "-", format(datTemp$timestamp[1],"%m"))
                    panel = paste0(solnameLst["dir"], solnameLst["degree"], solnameLst["type"])
                    dfMonth = data.frame("ym"=date_ym,"panel"=panel,"Wh"=whMonth)
                    i = i + 1
                    } else{
                        # subSol = bind_cols(subSol, datTemp)
                        sumHours = bind_cols(sumHours, sumHour)
                        # sumDays = bind_cols(sumDays, sumDay)
                        # sumWeeks = bind_cols(sumWeeks, sumWeek)
                        panel = paste0(solnameLst["dir"], solnameLst["degree"], solnameLst["type"])
                        print(panel)
                        monthi = data.frame("ym"=date_ym,"panel"=panel,"Wh"=whMonth)
                        dfMonth = bind_rows(dfMonth, monthi)
                    }
                }

            }
        }
    }
}

# ind <- seq(3, ncol(sumDays), by=2) # indices of columns to remove: every 3rd column starting from 1
# sumDays = sumDays[, -ind]
# sumWeeks = sumWeeks[, -ind]
# subSol = subSol[, -ind]
# names(subSol) <- unlist(col_headings)

ind <- seq(3, ncol(sumHours), by=2) # indices of columns to remove: every 3rd column starting from 1
sumHours = sumHours[, -ind]
# 
# # round
# sumDays[, -1]=round(sumDays[, -1], 2)
# sumWeeks[, -1]=round(sumWeeks[, -1], 2)
# 
# # cumulative integral
# cumSumDays = cumsum(sumDays[,2:ncol(sumDays)])
# cumSumWeeks = cumsum(sumWeeks[,2:ncol(sumWeeks)])
# cumSumDays = bind_cols(sumDays[1], cumSumDays)
# cumSumWeeks = bind_cols(sumWeeks[1], cumSumWeeks)
# 
# # save data frames to csv
# date_ym = paste0(year(datTemp$timestamp[1]), "-", format(datTemp$timestamp[1],"%m"), "_")
# month = tolower(month(min(datSol$timestamp), label=TRUE))
# write.csv(subSol, file = paste0(date_ym,"subSol.csv"), row.names=FALSE)
# write.csv(sumDays, file = paste0(date_ym,"whDays.csv"), row.names=FALSE)
# write.csv(sumWeeks, file = paste0(date_ym,"whWeeks.csv"), row.names=FALSE)
# write.csv(dfMonth, file = paste0(date_ym,"whMonths.csv"), row.names=FALSE)
# write.csv(cumSumDays, file = paste0(date_ym,"cumWhDays.csv"), row.names=FALSE)
# write.csv(cumSumWeeks, file = paste0(date_ym,"cumWhWeeks.csv"), row.names=FALSE)
write.csv(sumHours, file = paste0(date_ym,"whHours.csv"), row.names=FALSE)

# sumDays2 = data.frame("day"=sumDays$day, "panel"=rep("D40JA", each=nrow(sumDays)), "Wh"=sumDays$D40JA)
# sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$day, "panel"=rep("D40LG", each=nrow(sumDays)), "Wh"=sumDays$D40LG))
# sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$day, "panel"=rep("D13JA", each=nrow(sumDays)), "Wh"=sumDays$D13JA))
# sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$day, "panel"=rep("D13LG", each=nrow(sumDays)), "Wh"=sumDays$D13LG))
# sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$day, "panel"=rep("A13JA", each=nrow(sumDays)), "Wh"=sumDays$A13JA))
# sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$day, "panel"=rep("A13LG", each=nrow(sumDays)), "Wh"=sumDays$A13LG))
# sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$day, "panel"=rep("R13JA", each=nrow(sumDays)), "Wh"=sumDays$R13JA))
# sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$day, "panel"=rep("R13LG", each=nrow(sumDays)), "Wh"=sumDays$R13LG))
# sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$day, "panel"=rep("D90JA", each=nrow(sumDays)), "Wh"=sumDays$D90JA))
# sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$day, "panel"=rep("D90LG", each=nrow(sumDays)), "Wh"=sumDays$D90LG))
# as.factor(sumDays2$panel)
# 
# cumSumDays2 = data.frame("day"=cumSumDays$day, "panel"=rep("D40JA", each=nrow(cumSumDays)), "Wh"=cumSumDays$D40JA)
# cumSumDays2 = rbind(cumSumDays2,data.frame("day"=cumSumDays$day, "panel"=rep("D40LG", each=nrow(cumSumDays)), "Wh"=cumSumDays$D40LG))
# cumSumDays2 = rbind(cumSumDays2,data.frame("day"=cumSumDays$day, "panel"=rep("D13JA", each=nrow(cumSumDays)), "Wh"=cumSumDays$D13JA))
# cumSumDays2 = rbind(cumSumDays2,data.frame("day"=cumSumDays$day, "panel"=rep("D13LG", each=nrow(cumSumDays)), "Wh"=cumSumDays$D13LG))
# cumSumDays2 = rbind(cumSumDays2,data.frame("day"=cumSumDays$day, "panel"=rep("A13JA", each=nrow(cumSumDays)), "Wh"=cumSumDays$A13JA))
# cumSumDays2 = rbind(cumSumDays2,data.frame("day"=cumSumDays$day, "panel"=rep("A13LG", each=nrow(cumSumDays)), "Wh"=cumSumDays$A13LG))
# cumSumDays2 = rbind(cumSumDays2,data.frame("day"=cumSumDays$day, "panel"=rep("R13JA", each=nrow(cumSumDays)), "Wh"=cumSumDays$R13JA))
# cumSumDays2 = rbind(cumSumDays2,data.frame("day"=cumSumDays$day, "panel"=rep("R13LG", each=nrow(cumSumDays)), "Wh"=cumSumDays$R13LG))
# cumSumDays2 = rbind(cumSumDays2,data.frame("day"=cumSumDays$day, "panel"=rep("D90JA", each=nrow(cumSumDays)), "Wh"=cumSumDays$D90JA))
# cumSumDays2 = rbind(cumSumDays2,data.frame("day"=cumSumDays$day, "panel"=rep("D90LG", each=nrow(cumSumDays)), "Wh"=cumSumDays$D90LG))
# as.factor(cumSumDays2$panel)


# size adjustment coef for each type of panel
k_ja = 1.63515
k_lg = 1.7272

# check again what you divide
# sumDaysJA = dplyr::filter(sumDays2, grepl('JA', panel))
# sumDaysLG = dplyr::filter(sumDays2, grepl('LG', panel))
# sumDaysJA$Wh = sumDaysJA$Wh / k_ja
# sumDaysLG$Wh = sumDaysLG$Wh / k_lg
# sumDaysm2 = rbind(sumDaysJA, sumDaysLG)
# 
# cumDaysJA = dplyr::filter(cumSumDays2, grepl('JA', panel))
# cumDaysLG = dplyr::filter(cumSumDays2, grepl('LG', panel))
# cumDaysJA$Wh = cumDaysJA$Wh / k_ja
# cumDaysLG$Wh = cumDaysLG$Wh / k_lg
# cumDaysm2 = rbind(cumDaysJA, cumDaysLG)
# # names(subSol) <- unlist(col_headings)
# 
# write.csv(sumDaysm2, file = paste0(date_ym,"whDaysm2.csv"), row.names=FALSE)
# write.csv(cumDaysm2, file = paste0(date_ym,"cumWhDaysm2.csv"), row.names=FALSE)

# setwd(paste(default, "code\\", sep=""))
# source("plots.R")