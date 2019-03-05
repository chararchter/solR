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