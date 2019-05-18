library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)

default = "F:\\Users\\Janis\\VIKA\\solR\\data\\mar\\"
setwd(default)

lstData = list.files(pattern="*.csv")
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
diff = apply(sumHour[,-1], 2, function(x) diff(x))
diff = cbind(sumHour[2:nrow(sumHour),1], diff)