library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)

default = "F:\\Users\\Janis\\VIKA\\solR\\"
solNames = c("solD40", "solD13","solA13", "solR13", "solD90")

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

datSol = importDataRaw("solar", "main")
# write.csv(datSol, file = "datSol.csv", row.names=FALSE)
subSol = findSolPV(datSol)
num = findNum(subSol)

map = read.csv("numToCol.csv", header = TRUE, sep = ",")
map$numCharger = as.character(map$numCharger)
# col_headings = list(11)
# col_headings = c(col_headings, "timestamp")

col_headings = numeric(11)
col_headings[1] = "timestamp"

for (i in 1:length(num)){
    index = which(map$numCharger == num[i])[1]
    # print(i)
    print(index)
    print(map$newColName[index])
    # col_headings<-c(col_headings,map$newColName[index])
    col_headings[i+1] = map$newColName[index]
}


# datSol = fixDatetime(datSol)
# tz(datSol$timestamp) = "UTC"
# datSol$timestamp = with_tz(datSol$timestamp, tzone = "EET")

#######################
# choose columns
#######################
# setwd(paste(default, "data\\feb\\", sep=""))
# 
# pltMonth = function(solName){
#     colIndex = which( colnames(datSol)==solName )
#     solname = interpretSolPanel(solName)
#     var = produceStr(solname)
#     intrval1 = c(min(datSol$timestamp) + days(1) + hours(8), min(datSol$timestamp) + days(21) - hours(7))
#     intrval2 = toString(interval(intrval1[1], intrval1[2]))
# 
#     timestamp = datSol$timestamp
#     solVar = datSol[, colIndex]
#     datTemp = data.frame(timestamp, solVar)
#     return(datTemp)
# }


###########################
# plots solar data
###########################

# types = c('JA','LG')
# devices = c('_Bat', '_PV_')
# units = c('V', 'A', 'W')
# col_headings = list("timestamp")
# 
# i = 0
# for (solName in solNames){
#     for (type in types){
#         for (unit in units){
#             for (device in devices){
#                 solname = paste(solName, type, device, unit, sep="")
#                 solnameLst = interpretSolPanel(solname)
#                 if (solnameLst["device"] == "PV" & solnameLst["unit"] == "W"){
#                     datTemp = pltMonth(solname)
#                     whMonth = sumMonth(datTemp, solname)
#                     col_headings<-c(col_headings,solname)
#                     sumHour = integrateIntervalH(datTemp$timestamp, datTemp$solVar, hours(1), "hour", solname)
#                     sumDay = integrateIntervalH(date(datTemp$timestamp), date(datTemp$solVar), days(1), "day", solname)
#                     if (i == 0){
#                     subSol = datTemp
#                     sumHours = sumHour
#                     sumDays = sumDay
# 
#                     date_ym = paste0(year(datTemp$timestamp[1]), "-", format(datTemp$timestamp[1],"%m"))
#                     panel = paste0(solnameLst["dir"], solnameLst["degree"], solnameLst["type"])
#                     dfMonth = data.frame("ym"=date_ym,"panel"=panel,"Wh"=whMonth)
#                     i = i + 1
#                     } else{
#                         subSol = bind_cols(subSol, datTemp)
#                         sumHours = bind_cols(sumHours, sumHour)
#                         sumDays = bind_cols(sumDays, sumDay)
#                         panel = paste0(solnameLst["dir"], solnameLst["degree"], solnameLst["type"])
#                         monthi = data.frame("ym"=date_ym,"panel"=panel,"Wh"=whMonth)
#                         dfMonth = bind_rows(dfMonth, monthi)
#                     }
#                 }
# 
#             }
#         }
#     }
# }
# 
# ind <- seq(3, ncol(sumDays), by=2) # indices of columns to remove: every 3rd column starting from 1
# sumHours = sumHours[, -ind]
# sumDays = sumDays[, -ind]
# subSol = subSol[, -ind]
# names(subSol) <- unlist(col_headings)
# 
# # cumulative integral
# cumHours = cumsum(sumHours[,2:ncol(sumHours)])
# cumHours = bind_cols(sumHours[1], cumHours)
# cumDays = cumsum(sumDays[,2:ncol(sumDays)])
# cumDays = bind_cols(sumDays[1], cumDays)
# 
# save_csv = function(what, name){
#     date_ym = paste0(year(datTemp$timestamp[1]),"-",format(datTemp$timestamp[1],"%m"), "_")
#     write.csv(what, file = paste0(date_ym, name,".csv"), row.names=FALSE)
# }
# 
# # save data frames to csv
# save_csv(subSol, "subSol")
# save_csv(sumHours, "whHours")
# save_csv(sumDays, "whDays")
# save_csv(dfMonth, "whMonth")
# save_csv(cumHours, "whCumHours")
# save_csv(cumDays, "whCumDays")
# 
# 
# # size adjustment coef for each type of panel
# k_ja = 1.63515
# k_lg = 1.7272
# 
# sumHours_LG = sumHours[, -seq(2, ncol(sumDays), by=2)]
# sumHours_LG = format(sumHours_LG[, -1] / k_lg, digits = 2, nsmall=2)
# sumHours_JA = format(sumHours[, -seq(1, ncol(sumDays), by=2)] / k_ja, digits = 2, nsmall=2)
# sumHoursm2 = cbind(sumHours[1], sumHours_JA, sumHours_LG)
# 
# sumDays_LG = sumDays[, -seq(2, ncol(sumDays), by=2)]
# sumDays_LG = format(sumDays_LG[, -1] / k_lg, digits = 2, nsmall=2)
# sumDays_JA = format(sumDays[, -seq(1, ncol(sumDays), by=2)] / k_ja, digits = 2, nsmall=2)
# sumDaysm2 = cbind(sumDays[1], sumDays_JA, sumDays_LG)
# 
# cumHoursm2 = cumsum(sumHours[,2:ncol(sumHours)])
# cumHoursm2 = bind_cols(sumHours[1], cumHoursm2)
# cumDaysm2 = cumsum(sumDays[,2:ncol(sumDays)])
# cumDaysm2 = bind_cols(sumDays[1], cumDaysm2)
# 
# save_csv(sumHoursm2, "whm2Hours")
# save_csv(sumDaysm2, "whm2Days")
# save_csv(cumHoursm2, "whm2CumHours")
# save_csv(cumDaysm2, "whm2CumDays")
# 
# ##############################
# # adjust dataframes to plotting
# ##############################
# sumDays2 = data.frame("day"=sumDays$timestamp, "panel"=rep("D40JA", each=nrow(sumDays)), "Wh"=sumDays$D40JA)
# sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$timestamp, "panel"=rep("D40LG", each=nrow(sumDays)), "Wh"=sumDays$D40LG))
# sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$timestamp, "panel"=rep("D13JA", each=nrow(sumDays)), "Wh"=sumDays$D13JA))
# sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$timestamp, "panel"=rep("D13LG", each=nrow(sumDays)), "Wh"=sumDays$D13LG))
# sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$timestamp, "panel"=rep("A13JA", each=nrow(sumDays)), "Wh"=sumDays$A13JA))
# sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$timestamp, "panel"=rep("A13LG", each=nrow(sumDays)), "Wh"=sumDays$A13LG))
# sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$timestamp, "panel"=rep("R13JA", each=nrow(sumDays)), "Wh"=sumDays$R13JA))
# sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$timestamp, "panel"=rep("R13LG", each=nrow(sumDays)), "Wh"=sumDays$R13LG))
# sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$timestamp, "panel"=rep("D90JA", each=nrow(sumDays)), "Wh"=sumDays$D90JA))
# sumDays2 = rbind(sumDays2,data.frame("day"=sumDays$timestamp, "panel"=rep("D90LG", each=nrow(sumDays)), "Wh"=sumDays$D90LG))
# as.factor(sumDays2$panel)
# cumDays2 = data.frame("day"=cumDays$timestamp, "panel"=rep("D40JA", each=nrow(cumDays)), "Wh"=cumDays$D40JA)
# cumDays2 = rbind(cumDays2,data.frame("day"=cumDays$timestamp, "panel"=rep("D40LG", each=nrow(cumDays)), "Wh"=cumDays$D40LG))
# cumDays2 = rbind(cumDays2,data.frame("day"=cumDays$timestamp, "panel"=rep("D13JA", each=nrow(cumDays)), "Wh"=cumDays$D13JA))
# cumDays2 = rbind(cumDays2,data.frame("day"=cumDays$timestamp, "panel"=rep("D13LG", each=nrow(cumDays)), "Wh"=cumDays$D13LG))
# cumDays2 = rbind(cumDays2,data.frame("day"=cumDays$timestamp, "panel"=rep("A13JA", each=nrow(cumDays)), "Wh"=cumDays$A13JA))
# cumDays2 = rbind(cumDays2,data.frame("day"=cumDays$timestamp, "panel"=rep("A13LG", each=nrow(cumDays)), "Wh"=cumDays$A13LG))
# cumDays2 = rbind(cumDays2,data.frame("day"=cumDays$timestamp, "panel"=rep("R13JA", each=nrow(cumDays)), "Wh"=cumDays$R13JA))
# cumDays2 = rbind(cumDays2,data.frame("day"=cumDays$timestamp, "panel"=rep("R13LG", each=nrow(cumDays)), "Wh"=cumDays$R13LG))
# cumDays2 = rbind(cumDays2,data.frame("day"=cumDays$timestamp, "panel"=rep("D90JA", each=nrow(cumDays)), "Wh"=cumDays$D90JA))
# cumDays2 = rbind(cumDays2,data.frame("day"=cumDays$timestamp, "panel"=rep("D90LG", each=nrow(cumDays)), "Wh"=cumDays$D90LG))
# as.factor(cumDays2$panel)
# sumDaysJA = dplyr::filter(sumDays2, grepl('JA', panel))
# sumDaysLG = dplyr::filter(sumDays2, grepl('LG', panel))
# sumDaysJA$Wh = format(sumDaysJA$Wh / k_ja, digits = 2, nsmall=2)
# sumDaysLG$Wh =  format(sumDaysLG$Wh / k_lg, digits = 2, nsmall=2)
# sumDaysm2 = rbind(sumDaysJA, sumDaysLG)
# cumDaysJA = dplyr::filter(cumDays2, grepl('JA', panel))
# cumDaysLG = dplyr::filter(cumDays2, grepl('LG', panel))
# cumDaysJA$Wh = format(cumDaysJA$Wh / k_ja, digits = 2, nsmall=2)
# cumDaysLG$Wh = format(cumDaysLG$Wh / k_lg, digits = 2, nsmall=2)
# cumDaysm2 = rbind(cumDaysJA, cumDaysLG)
# 
# save_csv(sumDays2, "whDaysT")
# save_csv(cumDays2, "whCumDaysT")
# save_csv(sumDaysm2, "whm2DaysT")
# save_csv(cumDaysm2, "whm2CumDaysT")
# 
# sumHours2 = data.frame("day"=sumHours$timestamp, "panel"=rep("D40JA", each=nrow(sumHours)), "Wh"=sumHours$D40JA)
# sumHours2 = rbind(sumHours2,data.frame("day"=sumHours$timestamp, "panel"=rep("D40LG", each=nrow(sumHours)), "Wh"=sumHours$D40LG))
# sumHours2 = rbind(sumHours2,data.frame("day"=sumHours$timestamp, "panel"=rep("D13JA", each=nrow(sumHours)), "Wh"=sumHours$D13JA))
# sumHours2 = rbind(sumHours2,data.frame("day"=sumHours$timestamp, "panel"=rep("D13LG", each=nrow(sumHours)), "Wh"=sumHours$D13LG))
# sumHours2 = rbind(sumHours2,data.frame("day"=sumHours$timestamp, "panel"=rep("A13JA", each=nrow(sumHours)), "Wh"=sumHours$A13JA))
# sumHours2 = rbind(sumHours2,data.frame("day"=sumHours$timestamp, "panel"=rep("A13LG", each=nrow(sumHours)), "Wh"=sumHours$A13LG))
# sumHours2 = rbind(sumHours2,data.frame("day"=sumHours$timestamp, "panel"=rep("R13JA", each=nrow(sumHours)), "Wh"=sumHours$R13JA))
# sumHours2 = rbind(sumHours2,data.frame("day"=sumHours$timestamp, "panel"=rep("R13LG", each=nrow(sumHours)), "Wh"=sumHours$R13LG))
# sumHours2 = rbind(sumHours2,data.frame("day"=sumHours$timestamp, "panel"=rep("D90JA", each=nrow(sumHours)), "Wh"=sumHours$D90JA))
# sumHours2 = rbind(sumHours2,data.frame("day"=sumHours$timestamp, "panel"=rep("D90LG", each=nrow(sumHours)), "Wh"=sumHours$D90LG))
# as.factor(sumHours2$panel)
# cumHours2 = data.frame("day"=cumHours$timestamp, "panel"=rep("D40JA", each=nrow(cumHours)), "Wh"=cumHours$D40JA)
# cumHours2 = rbind(cumHours2,data.frame("day"=cumHours$timestamp, "panel"=rep("D40LG", each=nrow(cumHours)), "Wh"=cumHours$D40LG))
# cumHours2 = rbind(cumHours2,data.frame("day"=cumHours$timestamp, "panel"=rep("D13JA", each=nrow(cumHours)), "Wh"=cumHours$D13JA))
# cumHours2 = rbind(cumHours2,data.frame("day"=cumHours$timestamp, "panel"=rep("D13LG", each=nrow(cumHours)), "Wh"=cumHours$D13LG))
# cumHours2 = rbind(cumHours2,data.frame("day"=cumHours$timestamp, "panel"=rep("A13JA", each=nrow(cumHours)), "Wh"=cumHours$A13JA))
# cumHours2 = rbind(cumHours2,data.frame("day"=cumHours$timestamp, "panel"=rep("A13LG", each=nrow(cumHours)), "Wh"=cumHours$A13LG))
# cumHours2 = rbind(cumHours2,data.frame("day"=cumHours$timestamp, "panel"=rep("R13JA", each=nrow(cumHours)), "Wh"=cumHours$R13JA))
# cumHours2 = rbind(cumHours2,data.frame("day"=cumHours$timestamp, "panel"=rep("R13LG", each=nrow(cumHours)), "Wh"=cumHours$R13LG))
# cumHours2 = rbind(cumHours2,data.frame("day"=cumHours$timestamp, "panel"=rep("D90JA", each=nrow(cumHours)), "Wh"=cumHours$D90JA))
# cumHours2 = rbind(cumHours2,data.frame("day"=cumHours$timestamp, "panel"=rep("D90LG", each=nrow(cumHours)), "Wh"=cumHours$D90LG))
# as.factor(cumHours2$panel)
# sumHoursJA = dplyr::filter(sumHours2, grepl('JA', panel))
# sumHoursLG = dplyr::filter(sumHours2, grepl('LG', panel))
# sumHoursJA$Wh = format(sumHoursJA$Wh / k_ja, digits = 2, nsmall=2)
# sumHoursLG$Wh = format(sumHoursLG$Wh / k_lg, digits = 2, nsmall=2)
# sumHoursm2 = rbind(sumHoursJA, sumHoursLG)
# cumHoursJA = dplyr::filter(cumHours2, grepl('JA', panel))
# cumHoursLG = dplyr::filter(cumHours2, grepl('LG', panel))
# cumHoursJA$Wh = format(cumHoursJA$Wh / k_ja, digits = 2, nsmall=2)
# cumHoursLG$Wh = format(cumHoursLG$Wh / k_lg, digits = 2, nsmall=2)
# cumHoursm2 = rbind(cumHoursJA, cumHoursLG)
# 
# save_csv(sumHours2, "whHoursT")
# save_csv(cumHours2, "whCumHoursT")
# save_csv(sumHoursm2, "whm2HoursT")
# save_csv(cumHoursm2, "whm2CumHoursT")
##############################

# setwd(paste(default, "code\\", sep=""))
# source("plots.R")