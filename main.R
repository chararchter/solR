library(ggplot2)
library(lubridate)
library(dplyr)
library(wesanderson)
library(latex2exp)
# library(DescTools) # for trapezoid integrating
# library(gridExtra) # combining 2 plots together in a grid
# library(gridGraphics) # cheat solution for only 'grobs' allowed in "gList" error I found on stack exchange
# library(pracma) # cubic spline

default = "F:\\Users\\Janis\\VIKA\\solR\\"
solNames = c("solD40", "solD13","solA13", "solR13", "solD90")
width = 15.45
height = 9.549

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

setwd(paste(default, "data\\solar\\", sep=""))
datSol = importData("solar", "main", 3)

ind <- seq(2, 66, by=1) # indices of columns to remove
datSol = datSol[, -ind]

# for (i in 2:ncol(datSol)){
#     datSol[,i] <- as.character(datSol[,i])
#     datSol[,i][datSol[,i]==""] <- "NA"
#     datSol[,i] <- as.factor(datSol[,i])
# }

id = "march"
write.csv(datSol, file = paste(id, ".csv"))

datSol = read.csv(paste(id, ".csv"), col.names = colNames(id),
                  header = FALSE, sep = ",")

ind <- seq(163, 188, by=1) # indices of columns to remove
datSol = datSol[, -ind]
datSol = datSol[, -1]
datSol = datSol[125:nrow(datSol),]

datSol = apply(datSol, 2, function(x) gsub("^$|^ $", NA, x))

datSol = fixDatetime(datSol)
datSol[, "timestamp"] = as.numeric(datSol[, "timestamp"])
print(head(datSol))


datSol = data.frame("timestamp" = as.numeric(datSol[, "timestamp"]),
                 "solD13JA_PV_W" = as.numeric(datSol[, "solD13JA_PV_W"]),
                 "solD13LG_PV_W" = as.numeric(datSol[, "solD13LG_PV_W"]),
                 "solD40JA_PV_W" = as.numeric(datSol[, "solD40JA_PV_W"]),
                 "solD40LG_PV_W" = as.numeric(datSol[, "solD40LG_PV_W"]),
                 "solD90JA_PV_W" = as.numeric(datSol[, "solD90JA_PV_W"]),
                 "solD90LG_PV_W" = as.numeric(datSol[, "solD90LG_PV_W"]),
                 "solA13JA_PV_W" = as.numeric(datSol[, "solA13JA_PV_W"]),
                 "solA13LG_PV_W" = as.numeric(datSol[, "solA13LG_PV_W"]),
                 "solR13JA_PV_W" = as.numeric(datSol[, "solR13JA_PV_W"]),
                 "solR13LG_PV_W" = as.numeric(datSol[, "solR13LG_PV_W"]) )

datSol$timestamp = as_datetime(datSol$timestamp)

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
    print(solname)
    
    var = produceStr(solname)
    strt = min(datSol$timestamp)
    # strt = as.numeric(strt)
    intrval1 = c(strt + days(26) + hours(6), strt + days(27) - hours(5))
    intrval2 = toString(interval(intrval1[1], intrval1[2]))

    timestamp = datSol[,"timestamp"]
    solVar = datSol[, colIndex]
    datTemp = data.frame(timestamp, solVar)

    plotSol <- datSol %>% ggplot(aes(x = timestamp, y = solVar)) + geom_line() +
        sharedTheme +
        coord_cartesian(xlim = intrval1) +
        sharedAxis + ylab(var["labelSolVar"]) +
        ggtitle(paste(var["panelVerbose"], date(min(datSol$timestamp)+days(26)) ))
    ggsave(paste("sol", var["panel"], var["measurement"], ".png",sep=""),
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
                # datTemp = pltMonth(solname)
                solnameLst = interpretSolPanel(solname)
                if (solnameLst["device"] == "PV" & solnameLst["unit"] == "W"){
                    datTemp = pltMonth(solname)
                    # print(datTemp)
                    kWhMonth = sumMonth(datTemp, solname)
                    
                    if (solnameLst["type"] == "JA"){
                        wh = data.frame("panel" = paste(solnameLst["dir"], solnameLst["degree"], sep=""),
                                        "type" = solnameLst["type"], "Wh" = kWhMonth/1.63515)
                    }
                    if (solnameLst["type"] == "LG"){
                        wh = data.frame("panel" = paste(solnameLst["dir"], solnameLst["degree"], sep=""),
                                        "type" = solnameLst["type"], "Wh" = kWhMonth/1.7272)
                    }
            
                    # print(kWhMonth)
                    sumDay = integrateInterval2(date(min(datTemp$timestamp)), date(max(datTemp$timestamp)), days(1), datTemp, solname)
                    sumWeek = integrateInterval2(date(min(datTemp$timestamp)), date(max(datTemp$timestamp)), days(7), datTemp, solname)

                    if (i == 0){
                    sumwh = wh
                    sumDays = sumDay
                    sumWeeks = sumWeek
                    i = i + 1
                    } else{
                        sumwh = bind_rows(sumwh, wh)
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

# m = ggplot(datMeteo, aes(x=timestamp, y=value, color = variable, shape = variable)) +
#     geom_line(aes(y = solarIrradiance*1.63515, col = "solarIrradianceJA", shape = "solarIrradianceJA")) +
#     geom_line(aes(y = solarIrradiance*1.7272, col = "solarIrradianceLG", shape = "solarIrradianceLG")) +
#     scale_color_manual(values = wes_palette("BottleRocket1", 2)) +
#     theme_minimal() + xlab("Timestamp") + ylab("Watts/ Panel area") + ggtitle("Solar irradiance")
# ggsave("meteo.pdf", width = width, height = height, units = "cm")

# p = ggplot(sumDays, aes(x=day, y=value, color = variable, shape = variable)) +
#     geom_point(aes(y = D13JA, col = "D13JA", shape = "D13JA")) +
#     geom_point(aes(y = D13LG, col = "D13LG", shape = "D13LG")) +
#     geom_point(aes(y = D40JA, col = "D40JA", shape = "D40JA")) +
#     geom_point(aes(y = D40LG, col = "D40LG", shape = "D40LG")) +
#     geom_point(aes(y = D40JA, col = "D90JA", shape = "D90JA")) +
#     geom_point(aes(y = D40LG, col = "D90LG", shape = "D90LG")) +
#     scale_color_manual(values = wes_palette("BottleRocket1", 6)) +
#     theme_minimal() + xlab("Day") + ylab("Wh") + ggtitle("Power generated by each solar panel")
# ggsave("dayWH_D.pdf", width = width, height = height, units = "cm")
# 
p = ggplot(sumDays, aes(x=day, y=value, color = panel, shape = panel)) +
    geom_point(aes(y = D13JA/1000, col = "D13JA", shape = "D13JA")) +
    # geom_point(aes(y = D13LG/1000, col = "D13LG", shape = "D13LG")) +
    geom_point(aes(y = D40JA/1000, col = "D40JA", shape = "D40JA")) +
    # geom_point(aes(y = D40LG/1000, col = "D40LG", shape = "D40LG")) +
    geom_point(aes(y = D90JA/1000, col = "D90JA", shape = "D90JA")) +
    # geom_point(aes(y = D40LG/1000, col = "D90LG", shape = "D90LG")) +
    scale_color_manual(values = wes_palette("Darjeeling1", 3)) +
    theme_minimal() + xlab("Day") + ylab("kWh")
    # ggtitle("Power generated by each solar panel")
ggsave("dayKWH_DJA.png", width = width, height = height, units = "cm")

p = ggplot(sumDays, aes(x=day, y=value, color = panel, shape = panel)) +
    # geom_point(aes(y = D13JA/1000, col = "D13JA", shape = "D13JA")) +
    geom_point(aes(y = D13LG/1000, col = "D13LG", shape = "D13LG")) +
    # geom_point(aes(y = D40JA/1000, col = "D40JA", shape = "D40JA")) +
    geom_point(aes(y = D40LG/1000, col = "D40LG", shape = "D40LG")) +
    # geom_point(aes(y = D90JA/1000, col = "D90JA", shape = "D90JA")) +
    geom_point(aes(y = D90LG/1000, col = "D90LG", shape = "D90LG")) +
    scale_color_manual(values = wes_palette("Darjeeling1", 3)) +
    theme_minimal() + xlab("Day") + ylab("kWh")
# ggtitle("Power generated by each solar panel")
ggsave("dayKWH_DLG.png", width = width, height = height, units = "cm")

p = ggplot(sumDays, aes(x=day, y=value, color = panel, shape = panel)) +
    geom_point(aes(y = A13JA, col = "A13JA", shape = "A13JA")) +
    # geom_point(aes(y = A13LG, col = "A13LG", shape = "A13LG")) +
    geom_point(aes(y = D13JA, col = "D13JA", shape = "D13JA")) +
    # geom_point(aes(y = D13LG, col = "D13LG", shape = "D13LG")) +
    geom_point(aes(y = R13JA, col ="R13JA", shape = "R13JA")) +
    # geom_point(aes(y = R13LG, col = "R13LG", shape = "R13LG")) +
    scale_color_manual(values = wes_palette("Darjeeling1", 3)) +
    theme_minimal() + xlab("Day") + ylab("Wh")
    # ggtitle("Power generated by each solar panel")
ggsave("dayWH_13JA.png", width = width, height = height, units = "cm")

p = ggplot(sumDays, aes(x=day, y=value, color = panel, shape = panel)) +
    # geom_point(aes(y = A13JA, col = "A13JA", shape = "A13JA")) +
    geom_point(aes(y = A13LG, col = "A13LG", shape = "A13LG")) +
    # geom_point(aes(y = D13JA, col = "D13JA", shape = "D13JA")) +
    geom_point(aes(y = D13LG, col = "D13LG", shape = "D13LG")) +
    # geom_point(aes(y = R13JA, col ="R13JA", shape = "R13JA")) +
    geom_point(aes(y = R13LG, col = "R13LG", shape = "R13LG")) +
    scale_color_manual(values = wes_palette("Darjeeling1", 3)) +
    theme_minimal() + xlab("Day") + ylab("Wh")
# ggtitle("Power generated by each solar panel")
ggsave("dayWH_13LG.png", width = width, height = height, units = "cm")

# p = ggplot(sumDays, aes(x=day, y=value, color = variable, shape = variable)) +
#     geom_point(aes(y = A13JA/1000, col = "A13JA", shape = "A13JA")) +
#     geom_point(aes(y = A13LG/1000, col = "A13LG", shape = "A13LG")) +
#     geom_point(aes(y = D13JA/1000, col = "D13JA", shape = "D13JA")) +
#     geom_point(aes(y = D13LG/1000, col = "D13LG", shape = "D13LG")) +
#     geom_point(aes(y = R13JA/1000, col ="R13JA", shape = "R13JA")) +
#     geom_point(aes(y = R13LG/1000, col = "R13LG", shape = "R13LG")) +
#     scale_color_manual(values = wes_palette("BottleRocket1", 6)) +
#     theme_minimal() + xlab("Day") + ylab("kWh") + ggtitle("Power generated by each solar panel")
# ggsave("dayKWH_13.pdf", width = width, height = height, units = "cm")
# 
p = ggplot(sumDays, aes(x=day, y=value, color = panel, shape = panel)) +
    geom_point(aes(y = D90JA, col = "D90JA", shape = "D90JA")) +
    geom_point(aes(y = D90LG, col = "D90LG", shape = "D90LG")) +
    scale_color_manual(values = wes_palette("BottleRocket1", 2)) +
    theme_minimal() + xlab("Day") + ylab("Wh") 
    # ggtitle("Power generated by each solar panel")
ggsave("dayKWH_D90.png", width = width, height = height, units = "cm")

p = ggplot(sumDays, aes(x=day, y=value, color = panel, shape = panel)) +
    geom_point(aes(y = D13JA, col = "D13JA", shape = "D13JA")) +
    geom_point(aes(y = D13LG, col = "D13LG", shape = "D13LG")) +
    scale_color_manual(values = wes_palette("BottleRocket1", 2)) +
    theme_minimal() + xlab("Day") + ylab("Wh") 
    # ggtitle("Power generated by each solar panel")
ggsave("dayKWH_D13.png", width = width, height = height, units = "cm")

p = ggplot(sumDays, aes(x=day, y=value, color = panel, shape = panel)) +
    geom_point(aes(y = D40JA, col = "D40JA", shape = "D40JA")) +
    geom_point(aes(y = D40LG, col = "D40LG", shape = "D40LG")) +
    scale_color_manual(values = wes_palette("BottleRocket1", 2)) +
    theme_minimal() + xlab("Day") + ylab("Wh") 
# ggtitle("Power generated by each solar panel")
ggsave("dayKWH_D40.png", width = width, height = height, units = "cm")

p = ggplot(sumDays, aes(x=day, y=value, color = panel, shape = panel)) +
    geom_point(aes(y = R13JA, col = "R13JA", shape = "R13JA")) +
    geom_point(aes(y = R13LG, col = "R13LG", shape = "R13LG")) +
    scale_color_manual(values = wes_palette("BottleRocket1", 2)) +
    theme_minimal() + xlab("Day") + ylab("Wh") 
# ggtitle("Power generated by each solar panel")
ggsave("dayKWH_R13.png", width = width, height = height, units = "cm")

p = ggplot(sumDays, aes(x=day, y=value, color = panel, shape = panel)) +
    geom_point(aes(y = A13JA, col = "A13JA", shape = "A13JA")) +
    geom_point(aes(y = A13LG, col = "A13LG", shape = "A13LG")) +
    scale_color_manual(values = wes_palette("BottleRocket1", 2)) +
    theme_minimal() + xlab("Day") + ylab("Wh") 
# ggtitle("Power generated by each solar panel")
ggsave("dayKWH_A13.png", width = width, height = height, units = "cm")

# p = ggplot(sumDays, aes(x=day, y=value, color = variable, shape = variable)) +
#     geom_point(aes(y = D90JA, col = "D90JA", shape = "D90JA")) +
#     geom_point(aes(y = D90LG, col = "D90LG", shape = "D90LG")) +
#     scale_color_manual(values = wes_palette("BottleRocket1", 2)) +
#     theme_minimal() + xlab("Day") + ylab("Wh") + ggtitle("Power generated by each solar panel")
# ggsave("dayKWH_D90.pdf", width = width, height = height, units = "cm")
# 
# sumDayJA = data.frame("day" = sumDays$day, "type" = rep("JA", each = nrow(sumDays)), "Wh" = sumDays$D90JA)
# sumDayLG = data.frame("day" = sumDays$day, "type" = rep("LG", each = nrow(sumDays)),"Wh" = sumDays$D90LG)
# 
# sumD90 = bind_rows(sumDayJA, sumDayLG)
# p = ggplot(sumD90, aes(x=day, y=Wh, fill = type)) +
#     geom_bar(stat = "identity", position=position_dodge()) +
#     # scale_color_manual(values = wes_palette("BottleRocket1", 2)) +
#     theme_minimal() + xlab("Day") + ylab("Wh") +
# ggtitle("D90, March")
# ggsave("dayWH_D90bar.png", width = width, height = height, units = "cm")


p = ggplot(sumwh, aes(x=panel, y=Wh/1000, fill = type)) +
    geom_bar(stat = "identity", position=position_dodge()) +
    # scale_color_manual(values = wes_palette("BottleRocket1", 2)) +
    theme_minimal() + xlab("Panel") + ylab(TeX("$\\frac{kWh}{m^2}$")) +
ggtitle("February")
ggsave("dayWH_D90bar.png", width = width, height = height, units = "cm")
write.csv(sumwh, file = "sumWhm2_feb.csv")

write.csv(sumDays, file = "sumDays_feb.csv")
write.csv(sumWeeks, file = "sumWeeks_feb.csv")
write.csv(datSol, file = "datSol_mar.feb")

# # week
# p = ggplot(sumWeeks, aes(x=day, y=value, color = variable, shape = variable)) +
#     geom_point(aes(y = D13JA, col = "D13JA", shape = "D13JA")) +
#     geom_point(aes(y = D13LG, col = "D13LG", shape = "D13LG")) +
#     geom_point(aes(y = D40JA, col = "D40JA", shape = "D40JA")) +
#     geom_point(aes(y = D40LG, col = "D40LG", shape = "D40LG")) +
#     geom_point(aes(y = D90JA, col = "D90JA", shape = "D90JA")) +
#     geom_point(aes(y = D90LG, col = "D90LG", shape = "D90LG")) +
#     scale_color_manual(values = wes_palette("BottleRocket1", 6)) +
#     theme_minimal() + xlab("Day") + ylab("Wh") + ggtitle("Power generated by each solar panel")
# ggsave("weekKWH_D.pdf", width = width, height = height, units = "cm")
# 
# p = ggplot(sumWeeks, aes(x=day, y=value, color = variable, shape = variable)) +
#     geom_point(aes(y = A13JA, col = "A13JA", shape = "A13JA")) +
#     geom_point(aes(y = A13LG, col = "A13LG", shape = "A13LG")) +
#     geom_point(aes(y = D13JA, col = "D13JA", shape = "D13JA")) +
#     geom_point(aes(y = D13LG, col = "D13LG", shape = "D13LG")) +
#     geom_point(aes(y = R13JA, col ="R13JA", shape = "R13JA")) +
#     geom_point(aes(y = R13LG, col = "R13LG", shape = "R13LG")) +
#     scale_color_manual(values = wes_palette("BottleRocket1", 6)) +
#     theme_minimal() + xlab("Day") + ylab("Wh") + ggtitle("Power generated by each solar panel")
# ggsave("weekKWH_13.pdf", width = width, height = height, units = "cm")
# 
# p = ggplot(sumWeeks, aes(x=day, y=value, color = variable, shape = variable)) +
#     geom_point(aes(y = A13JA, col = "A13JA", shape = "A13JA")) +
#     geom_point(aes(y = A13LG, col = "A13LG", shape = "A13LG")) +
#     scale_color_manual(values = wes_palette("BottleRocket1", 2)) +
#     theme_minimal() + xlab("Day") + ylab("Wh") + ggtitle("Power generated by each solar panel")
# ggsave("weekKWH_A13.pdf", width = width, height = height, units = "cm")
# 
# p = ggplot(sumWeeks, aes(x=day, y=value, color = variable, shape = variable)) +
#     geom_point(aes(y = D13JA, col = "D13JA", shape = "D13JA")) +
#     geom_point(aes(y = D13LG, col = "D13LG", shape = "D13LG")) +
#     scale_color_manual(values = wes_palette("BottleRocket1", 2)) +
#     theme_minimal() + xlab("Day") + ylab("Wh") + ggtitle("Power generated by each solar panel")
# ggsave("weekKWH_D13.pdf", width = width, height = height, units = "cm")
# 
# base <- ggplot(sumDays, aes(x = day, y = D40JA)) +
#     geom_blank() +
#     xlab("Day") +
#     ylab("Wh")