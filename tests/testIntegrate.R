default = "F:\\Users\\Janis\\VIKA\\solR\\code\\"
setwd(default)
source("integrate.R")

############################
# trapezoidArea
############################

# test1: now doesnt work on floats, cuz deltx calculates difftime(x[i+1], + x[i], unit="secs") now and needs POSIXct
# x = seq(1,7)
# y = c(0,0,2,2,2,0,0)
# plot(x,y)
# 
# area = trapezoidArea(x,y)
# print(area)

# test2 w POSIXt values & min interval
timestamp <- seq.POSIXt(from=as.POSIXct("2019-01-01 00:00:00"),
                        to=as.POSIXct("2019-01-02 02:00:00"), by= '30 min')
solVar = seq(from = 1, to = 10, length.out = length(timestamp))
plot(timestamp,solVar)
df = data.frame(timestamp, solVar)

area2 = trapezoidArea(timestamp,solVar)
print(paste("Ws =   ", area2, sep=""))
print(paste("Wh =   ", area2/(60*60), sep=""))

# test3 w POSIXt values & seconds interval
# timestamp <- seq.POSIXt(from=as.POSIXct("2019-01-01 00:00:00"),
#                         to=as.POSIXct("2019-01-01 00:06:00"), by= '30 sec')
# solVar = seq(from = 1, to = 10, length.out = length(timestamp))
# df = data.frame(timestamp, solVar)
# plot(timestamp,solVar)
# area3 = trapezoidArea(timestamp,solVar)
# print(paste("Ws =   ", area3, sep=""))
# print(paste("Wh =   ", area3/(60*60), sep=""))

# test how difftime works
# print(as.numeric(difftime(as.POSIXct("2019-01-01 00:00:00"), + as.POSIXct("2019-01-01 00:02:00"), unit="secs")))


############################
# integrateInterval
############################


testMin = integrateIntervalTest(min(df$timestamp), max(df$timestamp), minutes(60), df)