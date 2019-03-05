mergeData = function(whichData, id){
    setwd(paste(default, "data\\", whichData, "\\", sep=""))
    dataLst = howMuchFiles(whichData)
    # print(dataLst)
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