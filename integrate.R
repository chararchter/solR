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
        # print(paste("delta x    ", deltx, sep=""))
        # print(paste("vid y    ", yvid, sep=""))
        # print(paste("s[i]    ", s[i], sep=""))
    }
    return(t)
}

integrateInterval = function(lowerLimit, delta_t, datTemp, solName){
    # Defines lower and upper (atm end of the month) limits for trapezoidArea() integral.
    # Output - data frame, where "time" - lower limit of interval;
    # "sumkWh"  - value of area under the curve in that interval.   
    solname = interpretSolPanel(solName)
    var = produceStr(solname)
    
    timestamp = datTemp$timestamp
    upperLimit = lowerLimit + delta_t
    intrBig = (date(max(timestamp))-lowerLimit) # whole interval
    intrSmall = (upperLimit - lowerLimit) #small interval
    itrTimes = floor(as.numeric(intrBig) / as.numeric(intrSmall)) # how many small interval contains in big interval
    
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
    # print(sumInt)
    return(sumInt)
}

integrateInterval2 = function(gLowerLimit, gUpperLimit, delta_t, datTemp, solName){
    # Defines lower and upper limits for trapezoidArea() integral.
    # Output - data frame, where "time" - lower limit of interval;
    # "sumkWh"  - value of area under the curve in that interval.
    # differs from integrateIntegral by introducing global and local limits. 
    solname = interpretSolPanel(solName)
    var = produceStr(solname)
    
    timestamp = datTemp$timestamp
    locLowerLimit = gLowerLimit
    locUpperLimit = gLowerLimit + delta_t
    
    diffBig = as.numeric(difftime(gUpperLimit, gLowerLimit, units="secs"))
    diffSmall = lubridate::period_to_seconds(delta_t)
    itrTimes = floor(diffBig / diffSmall) # how many small interval contains in big interval
    
    x <- as_datetime(itrTimes)
    y <- numeric(itrTimes)
    count = 0
    i = 1
    while (interval(locLowerLimit, locUpperLimit) %within% interval(gLowerLimit, gUpperLimit)) {
        if (i == 1){
            strtIndex = 1
            endIndex = which(date(as.POSIXct(timestamp)) == date(as.POSIXct(locUpperLimit)))[1]
        } else {
            strtIndex = which(date(as.POSIXct(timestamp)) == date(as.POSIXct(locLowerLimit)))[1] + 1
            endIndex = which(date(as.POSIXct(timestamp)) == date(as.POSIXct(locUpperLimit)))[1]
        }
        datInt = datTemp[strtIndex:endIndex,]
        
        t = trapezoidArea(datInt$timestamp, datInt$solVar)
        Wh = t / 3600
        count = count + Wh
        # print(paste(interval(lowerLimit, upperLimit),"   ", "Wh =", format(kWh, digits = 2, nsmall=2), sep = " "))
        
        intLength = int_length(interval(timestamp[strtIndex], timestamp[endIndex]))
        x[i] <- date(timestamp[strtIndex])
        y[i] <- Wh
        
        locLowerLimit =  locUpperLimit
        locUpperLimit = locUpperLimit + delta_t
        i = i + 1
    }
    z = toString(var["panel"])
    sumInt = data.frame("day" = x, "solVar" = y)
    colnames(sumInt)[2] <- toString(var["panel"])
    print(sumInt)
    return(sumInt)
}



integrateIntervalTest = function(gLowerLimit, gUpperLimit, delta_t, datTemp){
    # Defines lower and upper (atm end of the month) limits for trapezoidArea() integral.
    # Output - data frame, where "time" - lower limit of interval;
    # "sumkWh"  - value of area under the curve in that interval.
    # g means global
    # loc means local

    timestamp = datTemp$timestamp
    locLowerLimit + gLowerLimit
    locUpperLimit = gLowerLimit + delta_t
    
    diffBig = as.numeric(difftime(gUpperLimit, gLowerLimit, units="secs"))
    diffSmall = lubridate::period_to_seconds(delta_t)
    itrTimes = floor(diffBig / diffSmall) # how many small interval contains in big interval
    
    x <- as_datetime(itrTimes)
    y <- numeric(itrTimes)
    count = 0
    i = 1
    
    for (i in 1:itrTimes){
        
    }
    
    # while (interval(locLowerLimit, locUpperLimit) %within% interval(gLowerLimit, gUpperLimit)) {
    #     # find the index of the first datetime element in a vector where the date component matches with input date
    #     # this wont work if start and end date are the same
    #     # strtIndex = which(date(as.POSIXct(timestamp)) == date(as.POSIXct(locLowerLimit)))[1]
    #     # endIndex = which(date(as.POSIXct(timestamp)) == date(as.POSIXct(locUpperLimit)))[1]
    # 
    #     strtIndex = (which(date(as.POSIXct(timestamp)) == date(as.POSIXct(locLowerLimit)))[1])
    #     endIndex = which(date(as.POSIXct(timestamp)) == date(as.POSIXct(locUpperLimit)))[1]
    #     
    #     datInt = datTemp[strtIndex:endIndex,]
    #     print(datInt)
    #     t = trapezoidArea(datInt$timestamp, datInt$solVar)
    #     # print(t)
    #     Wh = t / 3600    # convert Ws to Wh
    #     count = count + Wh
    #     # print(paste(interval(lowerLimit, upperLimit),"   ", "kWh =", format(kWh, digits = 2, nsmall=2), sep = " "))
    # 
    #     intLength = int_length(interval(timestamp[strtIndex], timestamp[endIndex]))
    #     x[i] <- date(timestamp[strtIndex])
    #     y[i] <- Wh
    # 
    #     lowerLimit =  upperLimit
    #     upperLimit = upperLimit + delta_t
    #     i = i + 1
    # }
    # print(x)
    # print(y)
    # sumInt = data.frame("day" = x, "solVar" = y)
    # return(sumInt)
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
    print(paste(interval(date(min(timestamp)), date(max(timestamp))),"   ", "Wh =", format(kWhMonth, digits = 2, nsmall=2), sep = " "))
    return(kWhMonth)
}