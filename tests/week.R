weekStats = function(data, timestamp, varSol){
    setwd(paste(default, "plots\\", sep=""))
    nor = min(timestamp)
    i = 1
    while (interval(date(nor), (date(nor) + days(2))) %within% interval(date(min(timestamp)), (date(max(timestamp))))) {
        pltWeekStats(data, timestamp, varSol, nor, i)
        nor =  nor + days(2)
        i = i + 1
    }
}

pltWeekStats = function(data, timestamp, varSol, nor, i){
    # Summarize gridToBattery by 1 hours in a 2 days interval
    data %>% group_by(timestamp=floor_date(timestamp, "1 hour")) %>%
        summarize(gridToBattery=sum(gridToBattery))  %>%
        ggplot(aes(x = timestamp, y = gridToBattery)) + geom_point() + sharedTheme +
        sharedAxis + ylab("Grid To Battery, kWh") +
        ggtitle(paste('Grid to Battery ', toString(interval(date(nor), (date(nor) + days(2)))))) +
        coord_cartesian(xlim = c(nor, nor + days(2)))
    ggsave(paste('week', toString(i), '.pdf', sep=""), width = 29.7, height = 21.0, units = "cm")
}