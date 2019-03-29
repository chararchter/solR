trapezoidArea = function(x, y){
    # Calculate the area under the curve formed by connecting all points by a direct line
    # (composite trapezoid rule). The curve must be given by vectors of xy-coordinates.
    # Output - numeric value of area under the curve.   
    t = 0
    s = 0
    
    for (i in 1:(length(x)-1)){
        deltx = x[i+1] - x[i]
        yvid = (y[i] + y[i+1])/2
        s[i] = deltx * yvid
        t = t+s[i]
    }
    return(t)
}

integrateInterval = function(lowerLimit, delta_t, datTemp, solName){
    # Defines lower and upper limits for trapezoidArea() integral.
    # Output - data frame, where "time" - lower limit of interval;
    # "sumkWh"  - value of area under the curve in that interval.   
    solname = interpretSolPanel(solName)
    var = produceStr(solname)
    
    timestamp = datTemp$timestamp
    upperLimit = lowerLimit + delta_t
    intrBig = (date(max(timestamp))-lowerLimit)
    intrSmall = (upperLimit - lowerLimit)
    itrTimes = floor(as.numeric(intrBig) / as.numeric(intrSmall))
    
    x <- as_datetime(itrTimes)
    y <- numeric(itrTimes)
    count = 0
    i = 1
    while (interval(lowerLimit, upperLimit) %within% interval(lowerLimit, date(max(timestamp)))) {
        strtIndex = which(date(as.POSIXct(timestamp)) == date(as.POSIXct(lowerLimit)))[1]
        endIndex = which(date(as.POSIXct(timestamp)) == date(as.POSIXct(upperLimit)))[1]
        datInt = datTemp[strtIndex:endIndex,]

        t = trapezoidArea(datInt$timestamp, datInt$solVar)
        kWh = t / 3600
        count = count + kWh
        # print(paste(interval(lowerLimit, upperLimit),"   ", "kWh =", format(kWh, digits = 2, nsmall=2), sep = " "))

        intLength = int_length(interval(timestamp[strtIndex], timestamp[endIndex]))
        x[i] <- date(timestamp[strtIndex])
        y[i] <- kWh
        
        lowerLimit =  upperLimit
        upperLimit = upperLimit + delta_t
        i = i + 1
    }
    z = toString(var["panel"])
    sumInt = data.frame("day" = x, "solVar" = y)
    colnames(sumInt)[2] <- toString(var["panel"])
    print(sumInt)
    return(sumInt)
}

sumMonth = function(datTemp, solName){
    # Input - data frame where x - timestamp, y - solVar;
    # Output - value of area under the curve in a month interval used for reference
    # to check if it equals sum of kWh integrated by day in same month integral
    solname = interpretSolPanel(solName)
    var = produceStr(solname)
    timestamp = datTemp$timestamp
    
    t = trapezoidArea(timestamp, datTemp$solVar)
    kWhMonth = t /3600
    print(solName)
    print("Reference for a month")
    print(paste(interval(date(min(timestamp)), date(max(timestamp))),"   ", "kWh =", format(kWhMonth, digits = 2, nsmall=2), sep = " "))
    return(kWhMonth)
}