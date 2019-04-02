interpretSolPanel = function(solPanel){
    # Input - string with parameter names integrated
    # Output - vector with split parameter names
    # Description - parses string by matching parameters
    
    #define possible patterns
	# e.g. vector 'directions' define all possible direction patterns: 'D','A', 'R'
    dir = substr(solname, 4, 4)
    type = substr(solname, 7, 8)
    degree = substr(solname, 5, 6)
    device = substr(solname, 10, 10)
    unit = substr(solname, 13, 13)
    
    if (device == "P"){
        device = substr(solname, 10, 11)
    }
    if (device == "B"){
        device = substr(solname, 10, 12)
    }
    
    parameters <- c(dir, degree, type, device, unit)
    names(parameters) <- c("dir", "degree", "type", "device", "unit")
    return(parameters)
}

whichDevice = function(device){
	# Input - device which applies to current column
	# Output - full name of that device to be used as plot label
    if (device == "Bat"){
        return("Battery")
    }
    if (device == "PV"){
        return("Photovoltaic")
    }
}

whichMeasure = function(unit){
	# Input - unit which applies to current column
	# Output - full name of that unit to be used as plot label    
    if (unit == "V")
        return(" voltage")
    if (unit == "A")
        return(" current")
    if (unit == "W")
        return(" power")
}

produceStr = function(solname){
    # Input - solname after interpretSolPanel(solName)
    # Output - combines useful strings of different solName parts to be used as titles, axis, and logical tests
    panel = paste(toString(solname["dir"]), toString(solname["degree"]), toString(solname["type"]), sep = "")
    panelVerbose = paste("Solar panel ", panel, sep = "")
    labelSolVar = paste(whichDevice(solname["device"]), whichMeasure(solname["unit"]), ", ", solname["unit"], sep = "")
    measurement = paste("_", solname["device"], solname["unit"], sep = "")
    
    parameters = c(panel, panelVerbose, labelSolVar, measurement)
    names(parameters) = c("panel", "panelVerbose", "labelSolVar", "measurement")
    return(parameters)
}