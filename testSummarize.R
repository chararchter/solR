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