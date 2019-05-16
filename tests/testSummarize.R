library(ggplot2)
library(lubridate)
library(dplyr)

timestamp <- seq.POSIXt(from=as.POSIXct("2019-01-01 00:00:00"), to=as.POSIXct("2019-01-01 23:00:00"), by= '15 min')
solVar <- rnorm(length(timestamp))
solVar = seq(from = 1, to = 100, length.out = length(timestamp))
data <- data.frame(timestamp, solVar)

tab1 = data %>% group_by(timestamp=floor_date(timestamp, "6 hours")) %>%
  summarize(solVar=sum(solVar))

plt = data %>% group_by(timestamp=floor_date(timestamp, "6 hours")) %>%
summarize(solVar=sum(solVar)) %>%
ggplot(aes(x = timestamp, y = solVar)) + geom_point() 


default = "F:\\Users\\Janis\\VIKA\\solR\\data\\mar\\"

lstData = list.files(pattern="*.csv")
data = read.csv("2019-03_whMins.csv", header = TRUE, sep = ",")
data$timestamp = as.POSIXct(strptime(data$timestamp, format="%Y-%m-%d %H:%M:%S"))

sumHour = data %>% group_by(timestamp=floor_date(timestamp, "hour")) %>%
    summarize(solVar=format(sum(D.40.JA), digits = 2, nsmall = 2))

sumDay = data %>% group_by(timestamp=floor_date(timestamp, "day")) %>%
    summarize(solVar=format(sum(D.40.JA), digits = 2, nsmall = 2))

sumWeek = data %>% group_by(timestamp=floor_date(timestamp, "week")) %>%
    summarize(solVar=format(sum(D.40.JA), digits = 2, nsmall = 2))

sumMonth = apply(sumMins[,-1], 2, function(x) sum(x))