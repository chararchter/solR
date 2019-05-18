library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)

default = "F:\\Users\\Janis\\VIKA\\solR\\data\\mar\\"
setwd(default)

data = read.csv("2019-03_whMins.csv", header = TRUE, sep = ",")
data$timestamp = as.POSIXct(strptime(data$timestamp, format="%Y-%m-%d %H:%M:%S"))

sumHour = data.frame(data %>% group_by(timestamp=floor_date(timestamp, "hour")) %>%
    summarise_if(is.numeric,funs(sum)))

sumDay = data.frame(data %>% group_by(timestamp=floor_date(timestamp, "day")) %>%
    summarise_if(is.numeric,funs(sum)))

sumWeek = data.frame(data %>% group_by(timestamp=floor_date(timestamp, "7 days")) %>%
    summarise_if(is.numeric,funs(sum)))

sumMonth = data.frame(data %>% group_by(timestamp=floor_date(timestamp, "month")) %>%
    summarise_if(is.numeric,funs(sum)))

tidy = function(data){
    return(data %>%
    gather(key, Wh, -timestamp) %>%
    separate(key, c("Dir", "Degree", "Type"), "\\."))
}

sumMin.tidy = tidy(data)
sumHour.tidy = tidy(sumHour)
sumDay.tidy = tidy(sumDay)
sumWeek.tidy = tidy(sumWeek)
sumMonth.tidy = tidy(sumMonth)

# calculate differences between two values
dif = numeric(nrow(sumMin.tidy))
dif[1] = 0
dif[2:length(dif)] = diff(sumMin.tidy$Wh)

sumMin.dif = sumMin.tidy %>% mutate(Wh = dif)

whToWhm2 = function(data.tidy){
    # Adjusts Wh to Wh/m2 based on solar panel surface area
    k_ja = 1.63515 #m2
    k_lg = 1.7272 #m2
    data.m2 = data.tidy %>%
        mutate(Wh = ifelse(Type == "JA", Wh / k_ja, Wh / k_lg))
    colnames(data.m2)[5] <- "Whm2"
    return(data.m2)
}

sumMinm2 = whToWhm2(sumMin.tidy)
sumHourm2 = whToWhm2(sumHour.tidy)
sumDaym2 = whToWhm2(sumDay.tidy)
sumWeekm2 = whToWhm2(sumWeek.tidy)
sumMonthm2 = whToWhm2(sumMonth.tidy)