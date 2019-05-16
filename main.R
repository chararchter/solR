library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)

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
colNames = mapColNames(num)

subSol = subSol[-c(1,2,3),]
write.csv(subSol, file = "datSol.csv", row.names=FALSE)
datSol = read.csv("datSol.csv", skip = 1, header = FALSE, col.names = colNames, sep = ",")
print(head(datSol$timestamp))
datSol = fixDatetime(datSol)
tz(datSol$timestamp) = "UTC"
datSol$timestamp = with_tz(datSol$timestamp, tzone = "EET")
# datSol$timestamp = with_tz(datSol$timestamp, tzone = "Asia/Amman")
print(head(datSol$timestamp))

#######################
# choose columns
#######################
setwd(paste(default, "data\\mar\\", sep=""))

pltMonth = function(solName){
    colIndex = which( colnames(datSol)==solName )
    solname = interpretSolPanel(solName)
    var = produceStr(solname)
    intrval1 = c(min(datSol$timestamp) + days(1) + hours(8), min(datSol$timestamp) + days(21) - hours(7))
    intrval2 = toString(interval(intrval1[1], intrval1[2]))

    timestamp = datSol$timestamp
    solVar = datSol[, colIndex]
    datTemp = data.frame(timestamp, solVar)
    return(datTemp)
}

# marts 31 2:00 ir datSol$timestamp[60755]
# 60934

###########################
# plots solar data
###########################

types = c('JA','LG')
devices = c('_Bat', '_PV_')
units = c('V', 'A', 'W')
col_headings = list("timestamp")
col_heads = list("timestamp")

i = 0
for (solName in solNames){
    for (type in types){
        for (unit in units){
            for (device in devices){
                solname = paste(solName, type, device, unit, sep="")
                solnameLst = interpretSolPanel(solname)
                if (solnameLst["device"] == "PV" & solnameLst["unit"] == "W"){
                    datTemp = pltMonth(solname)
                    # whMonth = sumMonth(datTemp, solname)
                    col_headings<-c(col_headings,solname)
                    # sumMin = integrateIntervalH(datTemp$timestamp, datTemp$solVar, 
                    #                             dminutes(5), "min", solname)
                    sumMin1 = integrateIntervalH(datTemp$timestamp, datTemp$solVar,
                                    dminutes(5), "min", solname, min(datTemp$timestamp), datSol$timestamp[60934])
                    sumMin2 = integrateIntervalH(datTemp$timestamp, datTemp$solVar,
                                                dminutes(5), "min", solname, datSol$timestamp[60935], max(datSol$timestamp))
                    # sumHour = integrateIntervalH(datTemp$timestamp, datTemp$solVar, dhours(1), "hour", solname)
                    # sumDay = integrateIntervalH(date(datTemp$timestamp), date(datTemp$solVar), ddays(1), "day", solname)
                    
                    sumMin = rbind(sumMin1, sumMin2)
                    if (i == 0){
                    # subSol = datTemp
                    sumMins = sumMin
                    # sumHours = sumHour
                    # sumDays = sumDay

                    # date_ym = paste0(year(datTemp$timestamp[1]), "-", format(datTemp$timestamp[1],"%m"))
                    # panel = paste0(solnameLst["dir"], solnameLst["degree"], solnameLst["type"])
                    # col_heads<-c(col_heads,paste0(solnameLst["dir"], solnameLst["degree"], solnameLst["type"]))
                    # dfMonth = data.frame("ym"=date_ym,"panel"=panel,"Wh"=whMonth)
                    i = i + 1
                    } else{
                        # subSol = bind_cols(subSol, datTemp)
                        sumMins = bind_cols(sumMins, sumMin)
                        # sumHours = bind_cols(sumHours, sumHour)
                        # sumDays = bind_cols(sumDays, sumDay)
                        # panel = paste0(solnameLst["dir"], solnameLst["degree"], solnameLst["type"])
                        # monthi = data.frame("ym"=date_ym,"panel"=panel,"Wh"=whMonth)
                        # dfMonth = bind_rows(dfMonth, monthi)
                    }
                }

            }
        }
    }
}
# 
ind <- seq(3, ncol(sumMins), by=2) # indices of columns to remove: every 3rd column starting from
sumMins = sumMins[, -ind]
# sumHours = sumHours[, -ind]
# sumDays = sumDays[, -ind]
# subSol = subSol[, -ind]
# names(subSol) <- unlist(col_headings)

# cumulative integral
cumMins = cumsum(sumMins[,2:ncol(sumMins)])
cumMins = bind_cols(sumMins[1], cumMins)
# cumHours = cumsum(sumHours[,2:ncol(sumHours)])
# cumHours = bind_cols(sumHours[1], cumHours)
# cumDays = cumsum(sumDays[,2:ncol(sumDays)])
# cumDays = bind_cols(sumDays[1], cumDays)

save_csv = function(what, name){
    date_ym = paste0(year(datTemp$timestamp[1]),"-",format(datTemp$timestamp[1],"%m"), "_")
    write.csv(what, file = paste0(date_ym, name,".csv"), row.names=FALSE)
}

# sumMonth = apply(sumDays[,-1], 2, function(x) sum(x))

# save data frames to csv
# save_csv(subSol, "subSol")
save_csv(sumMins, "whMins")
# save_csv(sumHours, "whHours")
# save_csv(sumDays, "whDays")
# save_csv(dfMonth, "whMonth")
save_csv(cumMins, "whCumMins")
# save_csv(cumHours, "whCumHours")
# save_csv(cumDays, "whCumDays")





# size adjustment coef for each type of panel
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

##############################

# setwd(paste(default, "code\\", sep=""))
# source("plots.R")