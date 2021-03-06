trapezoidArea = function(x, y){
    # Calculate the area under the curve formed by connecting all points by a direct line
    # (composite trapezoid rule). The curve must be given by vectors of xy-coordinates.
    # Output - numeric value of area under the curve.   
    t = 0
    s = 0
    
    for (i in 1:(length(x)-1)){
        deltx = as.numeric(difftime(x[i+1], + x[i], unit="secs"))
        yvid = (y[i] + y[i+1])/2
        s[i] = deltx * yvid
        t = t+s[i]
    }
    return(t)
}

findIndex = function(x, lower, delta_t, timeUnit){
    upper = lower + delta_t
    if (timeUnit == "min"){
    strtIndex = which(date(x) == date(lower) & hour(x) == hour(lower) & minute(x) == minute(lower))[1]
    endIndex = which(date(x) == date(upper) & hour(x) == hour(upper) & minute(x) == minute(upper))[1]
    } else if (timeUnit == "hour"){
    strtIndex = which(date(x) == date(lower) & hour(x) == hour(lower))[1]
    endIndex = which(date(x) == date(upper) & hour(x) == hour(upper))[1]
    } else if (timeUnit == "day"){
        strtIndex = which(date(x) == date(lower))[1]
        endIndex = which(date(x) == date(upper))[1]  
    } else {
    strtIndex = which(date(x) == date(lower))[1]
    endIndex = which(date(x) == date(upper))[1]  
    }
    return(c(strtIndex, endIndex))
}

integrateIntervalH = function(x, y, delta_t, timeUnit, solName, from = min(x, na.rm=TRUE), to = max(x, na.rm=TRUE)){
    solname = interpretSolPanel(solName)
    var = produceStr(solname)
    
    lowerLimit = from
    upperLimit = lowerLimit + delta_t
    intrBig = to - from # whole interval
    intrSmall = upperLimit - lowerLimit #small interval
    itrTimes = floor(as.numeric(intrBig) / as.numeric(intrSmall)) # how many small interval in big interval

    xres <- as_datetime(itrTimes)
    yres <- numeric(itrTimes)
    count = 0
    i = 1
    while (interval(lowerLimit, upperLimit) %within% interval(from, to)) {

        indices = findIndex(x, lowerLimit, delta_t, timeUnit)
        datInt = datTemp[indices[1]:indices[2],]
        t = trapezoidArea(datInt$timestamp, datInt$solVar)
        
        wh = t / 3600
        count = count + wh
        xres[i] <- floor_date(x[indices[1]], unit = timeUnit)
        yres[i] <- round(wh, digits=2)

        lowerLimit =  upperLimit
        upperLimit = upperLimit + delta_t
        i = i + 1
    }
    
    z = toString(var["panel"])
    sumInt = data.frame("timestamp" = xres, "solVar" = yres)
    colnames(sumInt)[2] <- toString(var["panel"])
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
    whMonth = t /3600
    whMonth = format(whMonth, digits = 2, nsmall=2)
    print(solName)
    print("Reference for a month")
    print(paste(interval(date(min(timestamp)), date(max(timestamp))),"   ", "wh =", whMonth))
    return(whMonth)
}