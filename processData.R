howMuchFiles = function(whichData){
    setwd(paste(default, "data\\", whichData, "\\", sep=""))
    lstData = list.files(pattern="*.csv")
    return(lstData)
}

importData = function(whichData, id, skipCount){
    setwd(paste(default, "data\\", whichData, "\\", sep=""))
    data = read.csv(grep(id, howMuchFiles(whichData), value = TRUE), skip = skipCount,
                    header = FALSE, col.names = colNames(id), sep = ",")
    return(data)
}

importDataRaw = function(whichData, id){
    setwd(paste(default, "data\\", whichData, "\\", sep=""))
    data = read.csv(grep(id, howMuchFiles(whichData), value = TRUE),
                    header = FALSE, sep = ",")
    return(data)
}

findSolChargerCol = function(datSol){
    # Input - datSol data frame
    # Output - vector of column indices where in the native header
    # Solar Charger & PV power == TRUE
    indxSubSol = list()
    j = 1
    for (i in 1:ncol(datSol)){
        if (grepl("Solar.Charger",datSol[1,i])){
            if (datSol[2,i] == "PV power"){
                indxSubSol[[j]] = i
                j = j + 1
            }
        }
    }
    return(unlist(indxSubSol))
}

renameSubSol = function(subSol, timestamp){
    # Input - subSol data frame
    # Output - subSol data frame with code consistent headers
    setwd(paste(default, "data\\solar\\", sep=""))
    cipher = read.csv("numToCol.csv", header = TRUE, sep = ",")
    col_headings = list()
    col_headings[[1]] = "timestamp"
    for (i in 1:ncol(subSol)){
        splitBy_ = strsplit(toString(subSol[1,i]), " ")
        numOfPanel = splitBy_[[1]][3]
        indxOfPanel = match(numOfPanel, cipher$numCharger)
        col_headings[[i+1]] = toString(cipher$newColName[indxOfPanel])
    }
    
    col_headings = unlist(col_headings)
    subSol = bind_cols(timestamp, subSol)
    colnames(subSol) <- col_headings
    # remove native headers
    subSol = subSol[4:nrow(subSol),]
    return(subSol)
}

fixDatetime = function(data){
    # convert timestamp class from factor to POSIXct
    data$timestamp = as.POSIXct(strptime(data$timestamp, format="%Y-%m-%d %H:%M:%S"))
    # replace NA values with 0
    data[is.na(data)] = 0
    return(data)
}

mergeData = function(whichData, id){
    setwd(paste(default, "data\\", whichData, "\\", sep=""))
    dataLst = howMuchFiles(whichData)
    # create an empty data frame by removing all the rows from existent data frame cuz it has columns i want
    empty_df = read.csv(grep(id, dataLst[1], value = TRUE), skip = 1,
                        header = FALSE, col.names = colNames(id), sep = ",")
    
    empty_df = empty_df[FALSE,]
    for (i in 1:length(dataLst)){
        dfi = read.csv(grep(id, dataLst[i], value = TRUE), skip = 1,
                       header = FALSE, col.names = colNames(id), sep = ",")
        total = rbind(empty_df, dfi)
        empty_df = total
    }
    # sort a data frame by date
    total$timestamp <- lubridate::as_datetime(total$timestamp)
    dplyr::arrange(total, timestamp)
    return(total)
}