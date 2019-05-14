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


findSolPV= function(data){
    # Input - datSol data frame after importRaw
    # Output - subset of DatSol with Solar Charger PV power columns
    lst1 = unlist(as.list(data[1,]))
    lst2 = unlist(as.list(data[2,]))
    indices = (1)
    
    solPV = stringr::str_detect(lst1, "Solar") & stringr::str_detect(lst2, "PV power")
    indices = c(indices, which(solPV == TRUE))
    subSol = data[indices]
    return(subSol)
}

findNum= function(data){
    # Input - datSol data frame after importRaw
    # Output - subset of DatSol with Solar Charger PV power columns
    lst1 = unlist(as.list(data[1,]))
    solNum = unlist(stringr::str_split(lst1, " "))
    # print(solNum)
    ind = seq(4,length(solNum), 3)
    return(solNum[ind])
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

mapColNames = function(num){
    # Input - num is list of indices in correct order
    # Output - list of correct col names according to charger number
    map = read.csv("numToCol.csv", header = TRUE, sep = ",")
    map$newColName = as.character(map$newColName)
    
    col_headings = list()
    col_headings[1] = "timestamp"
    
    for (i in 1:length(num)){
        index = which(map$numCharger == num[i])[1]
        col_headings[i+1] = map$newColName[index]
    }
    return(unlist(col_headings))
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
