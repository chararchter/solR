library(ggplot2)

setwd("F:\\Users\\Janis\\VIKA\\data\\")
lstData = list.files(pattern="*.csv")

year = 2019
month = formatC(seq(1, 12), width=2, flag=0)
day = formatC(seq(1, 31), width=2, flag=0)

csvInput = function(i){
  filename = read.csv(lstData[i], header = TRUE, sep = ",")
  return(filename)
}

for (i in length(lstData)){
  filename = csvInput(i)
  kwh = grep("kwh", lstData, value = TRUE)
  # if (is.empty(grep("kwh", lstData, value = TRUE)) == FALSE){
  #   print(head(filename))
  # }
}