interpretSolPanel = function(solPanel){
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
    
    parameters <- c(dir, degree, type, device, unit)
    names(parameters) <- c("dir", "degree", "type", "device", "unit")
    return(parameters)
}