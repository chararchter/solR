# a script that takes meteo data each month and merges it into one data frame
# and writes that to csv file
# make a cron job to do that automatically once a month
# possible jobs:
#     copy data from z drive
#     create a new folder with name of the month
#     paste there
#     call R script which merges all data sets into one csv file
#     delete unnecessary files


# check if the directory exists
# dir.exists(file.path(mainDir, subDir))


# default = "F:\\Users\\Janis\\VIKA\\solR\\"


# source = "\\Zeus3www\\modlab_disks\\GRUPAS\\bddati\\MET\\"
# source = "\\Zeus3www\\modlab_disks\\"
source = "\\\\Zeus3www\\modlab_disks\\GRUPAS\\bddati\\MET\\"
setwd(source)

# splitBy_ = strsplit(solPanel, "_")
# unit = splitBy_[[1]][3]

# mergeData = function(whichData, id){
#     setwd(paste(default, "data\\", whichData, "\\", sep=""))
#     dataLst = howMuchFiles(whichData)
#     # create an empty data frame by removing all the rows from existent data frame cuz it has columns i want
#     empty_df = read.csv(grep(id, dataLst[1], value = TRUE), skip = 1,
#                         header = FALSE, col.names = colNames(id), sep = ",")
# 
#     empty_df = empty_df[FALSE,]
#     for (i in 1:length(dataLst)){
#         dfi = read.csv(grep(id, dataLst[i], value = TRUE), skip = 1,
#                        header = FALSE, col.names = colNames(id), sep = ",")
#         total = rbind(empty_df, dfi)
#         empty_df = total
#     }
#     # sort a data frame by date
#     total$timestamp <- lubridate::as_datetime(total$timestamp)
#     dplyr::arrange(total, timestamp)
#     return(total)
#     
# datMeteo = mergeData("meteo", "T000000")

# splitBy_ = strsplit(solPanel, "_")
# unit = splitBy_[[1]][3]


howMuchFiles = function(whichData){
    setwd(paste(default, "data\\", whichData, "\\", sep=""))
    lstData = list.files(pattern="*.csv")
    return(lstData)
}

mergeData = function(whichData, id){
    setwd(paste(default, "data\\", whichData, "\\", sep=""))
    dataLst = list.files(pattern="*.csv")
    print(dataLst)
    print(dataLst[1])    
    splitBy_ = strsplit(dataLst, "T")
    unit = splitBy_[[1]][1]

    
    # create an empty data frame by removing all the rows from existent data frame cuz it has columns i want
    # empty_df = read.csv(grep(id, dataLst[1], value = TRUE), skip = 1,
    #                     header = FALSE, col.names = colNames(id), sep = ",")
    # 
    # empty_df = empty_df[FALSE,]
    # for (i in 1:length(dataLst)){
    #     dfi = read.csv(grep(id, dataLst[i], value = TRUE), skip = 1,
    #                    header = FALSE, col.names = colNames(id), sep = ",")
    #     total = rbind(empty_df, dfi)
    #     empty_df = total
    # }
    # # sort a data frame by date
    # total$timestamp <- lubridate::as_datetime(total$timestamp)
    # dplyr::arrange(total, timestamp)
    # return(total)
}

# datMeteo = mergeData("meteo", "T000000")