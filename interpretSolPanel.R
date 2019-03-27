interpretSolPanel = function(solPanel){
    # Input - string with parameter names integrated
    # Output - vector with split parameter names
    # Description - parses string by matching parameters
    
    #define possible patterns
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