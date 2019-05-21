library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(pracma)

# change font to serif so it can display latvian language correctly
theme_set(theme_classic())
theme_map <- theme_get()
theme_map$text$family = "serif"
theme_map$text$size = 12
theme_map$plot.caption$family = "serif"
theme_map$plot.caption$size = 10
theme_map$plot.caption$colour = "gray30"

default = "F:\\Users\\Janis\\VIKA\\solR\\data\\mar\\"
setwd(default)

export_pdf <- function(p, namePanel){
    month = "mar"
    ggsave(filename = paste0(month, "_", namePanel, ".pdf"), plot = p,
           height=9, width=16, units = "cm")
}

tidy = function(data){
    return(data %>%
               gather(key, Wh, -timestamp) %>%
               separate(key, c("Dir", "Degree", "Type"), "\\."))
}

findGradient = function(mycol, col){
    # col = b for blue, t for turquoise, else green
    if (col == "b"){
        dumpCol = "#012b6c"
    } else if (col == "t"){
        dumpCol = "#005a43"
    }
    else{
        dumpCol = "#034c25"
    }
    colfunc <- colorRampPalette(c(dumpCol, mycol), space = "Lab")
    hgh = colfunc(10)[5]
    colfunc <- colorRampPalette(c("white", hgh), space = "Lab")
    ncolor = 30
    low = colfunc(ncolor)[3]
    return(c(low, hgh))
}

datMin = read.csv("2019-03_whMins_atstarpes.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
datMin$timestamp = as.POSIXct(datMin$timestamp)

datMin = data.frame(datMin %>% group_by(timestamp=floor_date(timestamp, "10 minutes")) %>%
                         summarise_if(is.numeric,funs(sum)))


datMin.tidy = tidy(datMin)
datMin.tidy = datMin.tidy  %>% separate(timestamp, c("date", "time"), sep=" ")

dummyDate = datMin.tidy$date[1]
datMin.tidy$date = ymd(datMin.tidy$date)
datMin.tidy$date = format(datMin.tidy$date, format="%y%m-%d")

datMin.tidier <- datMin.tidy %>%
    separate(date, into = c("YearMonth", "Date"), sep = "-")

datMin.tidier$fakeTime <- as.POSIXct(paste(dummyDate, datMin.tidy$time))

datMin.tidier = datMin.tidier[-1]

col13 = c("#00B788", "#03C133", "#00612E")
names(col13) <- c("D", "A", "R")
colD = c("#00B788", "#036C9B", "#003554")
names(colD) <- c("13", "40", "90")
axTime = scale_x_datetime(date_labels = "%H:%M", date_breaks = "5 hours")
labDeg2 = labs(x = "t, h", y = "E, Wh", shape = "Tips", col = "Lenkis")
labDir2 = labs(x = "t, h", y = "E, Wh", shape = "Tips", col = "Virziens")
shp = scale_shape_manual(values=c(1, 2))


data.13 = datMin.tidier %>% filter(Degree == "13")
data.D = datMin.tidier %>% filter(Dir == "D")

# d = ggplot(data.D, aes(x = fakeTime, y = Wh, col = Degree, linetype = Type)) +
#     geom_line(alpha = 0.5) + axTime + labDeg2 + shp + ggtitle("Direction: D") + scaleColD
# d2 = d + facet_grid(Type ~ Degree)
# export_pdf(d2, "test")

datMin.tidier$Date = as.numeric(datMin.tidier$Date)
datMin.tidier$Degree = as.numeric(datMin.tidier$Degree)

data.test = datMin.tidier %>% filter(Dir == "D" & Degree == 40 & Type == "JA" )
# data.test = data.test %>% filter(Date < 3)
# data.test = data.test %>% filter(Date %% 2 == 0)

month = month(data.test$fakeTime[1], label = TRUE, abbr = TRUE, locale = "Latvian")
month = paste0(toupper(substring(month, 1, 1)), substring(month, 2)) # start with uppercase letter
date = paste0(month, ", ", year(data.test$fakeTime[1]))

# summarizes wh in each day
group_by(data.test, as.numeric(Date)) %>% summarize(m = mean(Wh))
# summarizes wh over all days
mean = as.data.frame(group_by(data.test, fakeTime) %>% summarize(Wh = mean(Wh)))


mean1 = data.test %>% filter(Date < 6) %>% group_by(fakeTime) %>% summarize(Wh = mean(Wh))
mean2 = data.test %>% filter(Date < 11 & Date > 5) %>% group_by(fakeTime) %>% summarize(Wh = mean(Wh))
mean3 = data.test %>% filter(Date < 16 & Date > 10) %>% group_by(fakeTime) %>% summarize(Wh = mean(Wh))
mean4 = data.test %>% filter(Date < 21 & Date > 15) %>% group_by(fakeTime) %>% summarize(Wh = mean(Wh))
mean5 = data.test %>% filter(Date < 26 & Date > 20) %>% group_by(fakeTime) %>% summarize(Wh = mean(Wh))
mean6 = data.test %>% filter(Date < 31 & Date > 25) %>% group_by(fakeTime) %>% summarize(Wh = mean(Wh))

mean1 = cbind(mean1, Date = rep(1, nrow(mean1)))
mean2 = cbind(mean2, Date = rep(5, nrow(mean2)))
mean3 = cbind(mean3, Date = rep(10, nrow(mean3)))
mean4 = cbind(mean4, Date = rep(15, nrow(mean4)))
mean5 = cbind(mean5, Date = rep(20, nrow(mean5)))
mean6 = cbind(mean6, Date = rep(25, nrow(mean6)))

mean = rbind(mean1, mean2, mean3, mean4, mean5, mean6)


mean1 = data.test %>% filter(Date < 11) %>% group_by(fakeTime) %>% summarize(Wh = mean(Wh))
mean2 = data.test %>% filter(Date < 21 & Date > 10) %>% group_by(fakeTime) %>% summarize(Wh = mean(Wh))
mean3 = data.test %>% filter(Date < 31 & Date > 20) %>% group_by(fakeTime) %>% summarize(Wh = mean(Wh))

mean1 = cbind(mean1, Date = rep(10, nrow(mean1)))
mean2 = cbind(mean2, Date = rep(20, nrow(mean2)))
mean3 = cbind(mean3, Date = rep(30, nrow(mean3)))
mean = rbind(mean1, mean2, mean3)

mean1 = data.test %>% filter(Date < 31 & Date > 25) %>% group_by(fakeTime) %>% summarize(Wh = mean(Wh))
mean2 = data.test %>% filter(Date < 31 & Date > 25) %>% group_by(fakeTime) %>% summarize(Wh = mean(Wh))

mean1 = cbind(mean1, Date = rep(7, nrow(mean1)))
mean2 = cbind(mean2, Date = rep(22, nrow(mean2)))

mean = rbind(mean1, mean2)

mean1 = data.test %>% filter(Date < 5 & Date > 0) %>% group_by(fakeTime) %>% summarize(Wh = mean(Wh))
mean2 = data.test %>% filter(Date < 30 & Date > 25) %>% group_by(fakeTime) %>% summarize(Wh = mean(Wh))

mean1 = cbind(mean1, Date = rep(7, nrow(mean1)))
mean2 = cbind(mean2, Date = rep(22, nrow(mean2)))

mean = rbind(mean1, mean2)

mean$Wh = round(mean$Wh, digits = 2)


# data.test = data.test %>% filter(Date %% 5 == 0)
data.test = data.test %>% filter(Date < 10 & Date > 4)

mycol = "#036C9B"

scTest  = scale_colour_gradient(low = findGradient(mycol, "b")[1], high = findGradient(mycol, "b")[2],
                      space = "Lab", na.value = "grey50", guide = "colourbar",
                      aesthetics = "colour")

# d = ggplot(data.test, aes(x = fakeTime, y = Wh, group = as.numeric(Date))) +
#     geom_line(aes(col = Date)) +
#     geom_line(data = mean, aes(x = time, y = Wh)) +
#     scTest +
#     labs(x = "t, h", col = "Datums", title = "D.40.JA", caption = date) + 
#     theme_map 
# export_pdf(d, "testlab2")
# d0 = geom_line(data = mean, aes(x = fakeTime, y = Wh))

spl1 = smooth.spline(1:length(mean1$fakeTime), mean1$Wh,spar = 0.5, tol = 1e-2, nknots = floor(0.45*length(mean1$fakeTime)))
spl = data.frame(fakeTime = mean1$fakeTime, Wh = spl1$data$y)
spl = cbind(spl, Date = rep(22, nrow(spl)))

pp = cubicspline(1:length(mean$fakeTime), mean$Wh)
ppfun = function(xs) ppval(pp, xs)

dmean = geom_line(data = mean, aes(x = fakeTime, y = Wh)) 
dspl = geom_line(data = spl, aes(x = fakeTime, y = Wh), col = "red")
d = ggplot(data.test, aes(x = fakeTime, y = Wh, group = as.numeric(Date))) +
    geom_line(aes(col = Date), alpha = 0.5) +
    scTest + labs(x = "t, h", col = "Datums", title = "D.40.JA", caption = date) 
d = d + dmean
d = d + dspl
export_pdf(d, "testMeanS")


spl1 = smooth.spline(1:length(mean1$fakeTime), mean1$Wh,spar = 0.5, tol = 1e-2, nknots = floor(0.45*length(mean1$fakeTime)))
spl = data.frame(fakeTime = mean1$fakeTime, Wh = spl1$data$y)
spl = cbind(spl, Date = rep(22, nrow(spl)))

d = ggplot(data.test, aes(x = fakeTime, y = Wh, group = as.numeric(Date))) +
    geom_line(aes(col = Date), alpha = 0.5) +
    scTest + labs(x = "t, h", col = "Datums", title = "D.40.JA", caption = date) 
d = d + dmean
d = d + dspl
export_pdf(d, "testMeanS")



# directions = c('D')
# degrees = c(13, 40, 90)
# types = c('JA','LG')
# 
# sep = "."
# i = 0
# for (dir in directions){
#     for (degree in degrees){
#         for (type in types){
#             data.test = datMin.tidier %>% filter(Dir == dir & Degree == degree & Type == type )
#             data.test = data.test %>% filter(Date %% 2 == 0)
#             
#             mycol = colD[toString(degree)]
#             
#             scTest  = scale_colour_gradient(low = findGradient(mycol, "b")[1], high = findGradient(mycol, "b")[2],
#                                             space = "Lab", na.value = "grey50", guide = "colourbar",
#                                             aesthetics = "colour")
#             
#             d = ggplot(data.test, aes(x = fakeTime, y = Wh, group = as.numeric(Date))) +
#                 geom_line(aes(col = Date)) +
#                 scTest +
#                 labs(x = "t, h", col = "Datums", title = paste0(dir, sep, degree, sep, type), caption = date) + 
#                 theme_map 
#             export_pdf(d, paste0(dir, degree, type))
#             
#         }
#     }
#     }

