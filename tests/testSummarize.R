library(ggplot2)
library(lubridate)
library(dplyr)

timestamp <- seq.POSIXt(from=as.POSIXct("2019-01-01 00:00:00"), to=as.POSIXct("2019-01-01 23:00:00"), by= '15 min')
solVar <- rnorm(length(timestamp))
solVar = seq(from = 1, to = 100, length.out = length(timestamp))
data <- data.frame(timestamp, solVar)

# tab1 = data %>% group_by(timestamp=floor_date(timestamp, "6 hours")) %>%
#   summarize(solVar=sum(solVar))
# 
# plt = data %>% group_by(timestamp=floor_date(timestamp, "6 hours")) %>%
# summarize(solVar=sum(solVar)) %>%
# ggplot(aes(x = timestamp, y = solVar)) + geom_point()


default = "F:\\Users\\Janis\\VIKA\\solR\\data\\mar\\"
setwd(default)

lstData = list.files(pattern="*.csv")
data = read.csv("2019-03_whMins.csv", header = TRUE, sep = ",")
data$timestamp = as.POSIXct(strptime(data$timestamp, format="%Y-%m-%d %H:%M:%S"))

sum = function(delta){
    # period = string describing period e.g. hour, day, week, month
    delta = as.period(delta, unit = "hour")
    data %>% group_by(timestamp=floor_date(timestamp, unit = delta)) %>%
        summarise_if(is.numeric,funs(sum))
}

sum(hours("hour"))
# sumHour = sum("hour")
# sumHour = sum("hour")
# sumDay = sum("day")
# sumWeek = sum("7 days")
# sumMonth = sum("month")