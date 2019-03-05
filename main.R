# to get all libraries at once just install the whole tidyverse
library(ggplot2)
library(lubridate)
library(dplyr)
library(gridExtra) # combining 2 plots together in a grid
library(gridGraphics) # cheat solution for only 'grobs' allowed in "gList" error I found on stack exchange
library(pracma) # cubic spline

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
    
    intrval1 = c(min(datSol$timestamp), min(datSol$timestamp) + days(31))
    intrval2 = toString(interval(intrval1[1], intrval1[2]))
    width = 29.7
    height = 21.0
    
    solVar = datSol[, colIndex]
    plotSol <- datSol %>% ggplot(aes(x = timestamp, y = solVar)) + geom_point() +
        sharedTheme +
        coord_cartesian(xlim = intrval1) +
        sharedAxis + ylab(labelSolVar) + 
        ggtitle(paste(panelVerbose, intrval2))
    ggsave(paste("sol", panel, measurement, ".pdf",sep=""), width = width, height = height, units = "cm")
    
    plotSol2 = datSol %>% group_by(timestamp=floor_date(timestamp, "6 hours")) %>%
        summarize(solVar=sum(solVar)) %>%
        ggplot(aes(x = timestamp, y = solVar)) + geom_point() + sharedTheme + sharedAxis +
        ggtitle(paste('Summary ', toString(interval(min(datSol$timestamp), min(datSol$timestamp) + months(1)))))
        ggsave(paste("sol", panel, measurement, "sum.pdf",sep=""), width = 29.7, height = 21.0, units = "cm")
    return(plotSol)
}



###########################
# plots solar data
###########################

pltMonth("solD40JA_BatV")

types = c('JA','LG')
devices = c('_Bat', '_PV_')
units = c('V', 'A', 'W')

# disable as it produces 60 plots and runs slow
# for (solName in solNames){
#     for (unit in units){
#         for (device in devices){
#             for (type in types){
#                 print(paste(solName, type, device, unit, sep=""))
#                 # pltMonth(paste(solName, type, device, unit, sep=""))
#             }
#             print("###################")
#         }
#     }
# }


# plt <- vector("list", 5)
# i = 1
# 
# grab_grob <- function(){
#     grid.echo()
#     grid.grab()
# }

# Error in gList(list(list(data = list(timestamp = c(1546294086, 1546294985,  : 
                                                       # only 'grobs' allowed in "gList"
# for (device in devices){
#     for (unit in units){
#         for (type in types){
#             for (solName in solNames){
#                 # pltMonth(paste(solName, type, device, unit, sep=""))
#                 plots = pltMonth(paste(solName, type, device, unit, sep=""))
#                 plt[[i]] <- plots
#                 i <- i + 1
#             }
#             pdf(paste(type, device, unit, ".pdf", sep=""),
#                 width=8,
#                 height=15)
#             grid.arrange(
#                 arrangeGrob(plt[1],plt[2],plt[3],plt[4],plt[5],nrow=5,heights=c(.2,.2,.2,.2,.2))
#             )
#             dev.off()
#             plt[] <- NULL
#             i = 0
#             break
#         }
#         break
#     }
#     break
# }

###########################
#splines
###########################

intrval1 = c(as_datetime("2019-01-19 06:00:00 UTC"), as_datetime("2019-01-19 07:00:00 UTC") + hours(110))

# how many entries are there in intrval1? needed to decide nknots, which determines smoothing
intrRow = nrow(datMeteo %>%  filter(between(timestamp, min(intrval1), max(intrval1))))

spl_Meteo1 = spline(datMeteo$timestamp, datMeteo$solarIrradiance)
# spl_Meteo1 = smooth.spline(datMeteo$timestamp, datMeteo$solarIrradiance,spar = 1e-7, tol = 1e-2, all.knots = TRUE)
spl_Meteo2 = smooth.spline(datMeteo$timestamp, datMeteo$solarIrradiance,spar = 1e-7, tol = 1e-3, all.knots = TRUE)
spl_Meteo3 = smooth.spline(datMeteo$timestamp, datMeteo$solarIrradiance,spar = 1e-7, tol = 1e-3, nknots = floor(0.45*intrRow))
spl_Meteo4 = smooth.spline(datMeteo$timestamp, datMeteo$solarIrradiance,spar = 1e-7, tol = 1e-3, nknots = floor(0.35*intrRow))
# spl_Meteo4 = smooth.spline(datMeteo$timestamp, datMeteo$solarIrradiance,spar = 1e-7, tol = 1e-3, nknots = intrRow-10)

# spl_Meteo1 = data.frame(spl_Meteo1)
# spl_Meteo1$x <- lubridate::as_datetime(spl_Meteo1$x)
# spl_Meteo1 = dplyr::arrange(spl_Meteo1, x)
# 
# spl_Meteo2 = data.frame(x = spl_Meteo2$x, y = spl_Meteo2$y)
# spl_Meteo2$x <- lubridate::as_datetime(spl_Meteo2$x)
# spl_Meteo2 = dplyr::arrange(spl_Meteo2, x)
# 
# spl_Meteo3 = data.frame(x = spl_Meteo3$x, y = spl_Meteo3$y)
# spl_Meteo3$x <- lubridate::as_datetime(spl_Meteo3$x)
# spl_Meteo3 = dplyr::arrange(spl_Meteo3, x)
# 
# spl_Meteo4 = data.frame(x = spl_Meteo4$x, y = spl_Meteo4$y)
# spl_Meteo4$x <- lubridate::as_datetime(spl_Meteo4$x)
# spl_Meteo4 = dplyr::arrange(spl_Meteo4, x)
# 
# plot0 <- datMeteo %>%
#     ggplot() +
#     geom_point(aes(x = datMeteo$timestamp, y = datMeteo$solarIrradiance)) +
#     ylab("Data") + coord_cartesian(xlim = intrval1) +
#     theme_minimal() +
#     theme(axis.title.x = element_blank())
# 
# plot1 <- spl_Meteo1 %>%
#     ggplot() +
#     geom_line(aes(x = x, y = y), size = 0.5, alpha = 0.75) +
#     ylab("Spline") + coord_cartesian(xlim = intrval1) +
#     theme_minimal() +
#     theme(axis.title.x = element_blank())
# 
# plot2 <- spl_Meteo2 %>%
#     ggplot() +
#     geom_line(aes(x = x, y = y), size = 0.5, alpha = 0.75) + 
#     ylab("Smoothing spline all knots") + coord_cartesian(xlim = intrval1) +
#     theme_minimal() +
#     theme(axis.title.x = element_blank())
# 
# plot3 <- spl_Meteo3 %>%
#     ggplot() +
#     geom_line(aes(x = x, y = y), size = 0.5, alpha = 0.75) +
#     ylab("Smoothing spline nknots = 45% all knots") + coord_cartesian(xlim = intrval1) +
#     theme_minimal() +
#     theme(axis.title.x = element_blank())
# 
# plot4 <- spl_Meteo4 %>%
#     ggplot() +
#     geom_line(aes(x = x, y = y), size = 0.5, alpha = 0.75) +
#     ylab("Smoothing spline nknots = 35% all knot") + coord_cartesian(xlim = intrval1) +
#     theme_minimal() +
#     theme(axis.title.x = element_blank())

##########################
# cubic spline doesnt work. datetime seems at fault. fix it.
# pp = cubicspline (datMeteo$timestamp , datMeteo$solarIrradiance )
# ppfun = function ( xspl ) ppval (pp , xspl )
# # xspline = seq (0 ,150 ,0.5)
# xspl = seq.POSIXt(from=as_datetime(min(intrval1)), to=as_datetime(max(intrval1)), by="hour")
# cubicspline (datMeteo$timestamp , datMeteo$solarIrradiance , xi = xspl )
# yspl = cubicspline (datMeteo$timestamp , datMeteo$solarIrradiance , xspl )

# plot5 <- ggplot() +
#     geom_line(aes(x = xspl, y = yspl), size = 0.5, alpha = 0.75) +
#     ylab("Cubic spline") + coord_cartesian(xlim = intrval1) +
#     theme_minimal() +
#     theme(axis.title.x = element_blank())
##########################

# pdf('testSplines.pdf',
#     width=8,
#     height=12)
# grid.arrange(
#     arrangeGrob(plot0,plot2,plot3,plot4,nrow=4,heights=c(.25,.25,.25,.25))
# )
# dev.off()