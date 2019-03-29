searchPattern = function(name, parameter){
	# Input
	# 	name - string to be parsed e.g. "solD90LG_PV_W"
	# 	parameter - vector with all possible parameters of a name component for which to search for
	# Output - one parameter which is defined in the name
    for (char in 1:length(name)){
        for (i in 1:length(parameter)){
            if (grepl(parameter[i], name[char])){
                return(parameter[i])
            }
        }
    }
}

interpretSolPanel = function(solPanel){
    # Input - string with parameter names integrated
    # Output - vector with split parameter names
    # Description - parses string by matching parameters
    
    #define possible patterns
	# e.g. vector 'directions' define all possible direction patterns: 'D','A', 'R'
    directions = c('D','A', 'R')
    types = c('JA','LG')
    degrees = c('13', '40', '90')
    devices = c('Bat', 'PV')
    units = c('V', 'A', 'W')
    
    dir = searchPattern(solPanel, directions)
    type = searchPattern(solPanel, types)
    degree = searchPattern(solPanel, degrees)
    device = searchPattern(solPanel, devices)
    unit = searchPattern(solPanel, units)

    if (device == 'PV'){
        splitBy_ = strsplit(solPanel, "_")
        unit = splitBy_[[1]][3]
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