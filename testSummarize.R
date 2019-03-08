library(ggplot2)
library(lubridate)
library(dplyr)

timestamp <- seq(ymd('2019-01-01'),ymd('2019-01-12'), by = '30 min')
# solVar <- rnorm(length(timestamp))
solVar = seq(from = 1, to = 100, length.out = length(timestamp))
data <- data.frame(timestamp, solVar)

plt = data %>% group_by(timestamp=floor_date(timestamp, "6 hours")) %>%
    summarize(solVar=sum(solVar)) %>%
    ggplot(aes(x = timestamp, y = solVar)) + geom_point() 