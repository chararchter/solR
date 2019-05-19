library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)

default = "F:\\Users\\Janis\\VIKA\\solR\\data\\mar\\"
setwd(default)

data = read.csv("2019-03_whMins.csv", header = TRUE, sep = ",")
data$timestamp = as.POSIXct(strptime(data$timestamp, format="%Y-%m-%d %H:%M:%S"))


tidy = function(data){
    return(data %>%
               gather(key, Wh, -timestamp) %>%
               separate(key, c("Dir", "Degree", "Type"), "\\."))
}

whToWhm2 = function(data.tidy){
    # Adjusts Wh to Wh/m2 based on solar panel surface area
    k_ja = 1.63515 #m2
    k_lg = 1.7272 #m2
    data.m2 = data.tidy %>%
        mutate(Wh = ifelse(Type == "JA", Wh / k_ja, Wh / k_lg))
    colnames(data.m2)[5] <- "Whm2"
    return(data.m2)
}

export_pdf <- function(p, namePanel){
    month = "mar"
    ggsave(filename = paste0(month, "_", namePanel, ".pdf"), plot = p,
           height=9, width=16, units = "cm")
}

sumHour = data.frame(data %>% group_by(timestamp=floor_date(timestamp, "hour")) %>%
                         summarise_if(is.numeric,funs(sum)))

sumDay = data.frame(data %>% group_by(timestamp=floor_date(timestamp, "day")) %>%
                        summarise_if(is.numeric,funs(sum)))

sumWeek = data.frame(data %>% group_by(timestamp=floor_date(timestamp, "7 days")) %>%
                         summarise_if(is.numeric,funs(sum)))

sumMonth = data.frame(data %>% group_by(timestamp=floor_date(timestamp, "month")) %>%
                          summarise_if(is.numeric,funs(sum)))

sumMin.tidy = tidy(data)
sumHour.tidy = tidy(sumHour)
sumDay.tidy = tidy(sumDay)
sumWeek.tidy = tidy(sumWeek)
sumMonth.tidy = tidy(sumMonth)

# calculate differences between two values
dif = numeric(nrow(sumMin.tidy))
dif[1] = 0
dif[2:length(dif)] = diff(sumMin.tidy$Wh)

sumMin.dif = sumMin.tidy %>% mutate(Wh = dif)
sumMinm2 = whToWhm2(sumMin.tidy)
sumHourm2 = whToWhm2(sumHour.tidy)
sumDaym2 = whToWhm2(sumDay.tidy)
sumWeekm2 = whToWhm2(sumWeek.tidy)
sumMonthm2 = whToWhm2(sumMonth.tidy)

datMin = read.csv("2019-03_whMins_atstarpes.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
datMin.tidy = tidy(datMin)
datMin.tidy = datMin.tidy  %>% separate(timestamp, c("date", "time"), sep=" ")
datMin.tidy$date = ymd(datMin.tidy$date)
datMin.tidy$time = hms(datMin.tidy$time)

######################################
# plot
######################################

col13 = c("#00B788", "#03C133", "#00612E")
names(col13) <- c("D", "A", "R")

colD = c("#00B788", "#036C9B", "#003554")
names(colD) <- c("13", "40", "90")

colPan = c("#00B788", "#03C133", "#00612E", "#036C9B", "#003554")
names(colPan) <- c("D.13", "A.13", "R.13", "D.40", "D.90")
scaleColPan <- scale_colour_manual(name = "DirDeg", values=colPan)

scaleCol13 <- scale_colour_manual(name = "Dir", values=col13)
scaleColD <- scale_colour_manual(name = "Degree", values=colD)

scaleFill13 <- scale_fill_manual(name = "Dir", values=col13)
scaleFillD <- scale_fill_manual(name = "Degree", values=colD)

minm2.13 = sumMinm2 %>% filter(Degree == "13")
minm2.D = sumMinm2 %>% filter(Dir == "D")
hm2.13 = sumHourm2 %>% filter(Degree == "13")
hm2.D = sumHourm2 %>% filter(Dir == "D")
daym2.13 = sumDaym2 %>% filter(Degree == "13")
daym2.D = sumDaym2 %>% filter(Dir == "D")
minDif.13 = sumMin.dif %>% filter(Degree == "13")
minDif.D = sumMin.dif %>% filter(Dir == "D")

min.13 = sumMin.tidy %>% filter(Degree == "13")
min.D = sumMin.tidy %>% filter(Dir == "D")
h.13 = sumHour.tidy %>% filter(Degree == "13")
h.D = sumHour.tidy %>% filter(Dir == "D")
day.13 = sumDay.tidy %>% filter(Degree == "13")
day.D = sumDay.tidy %>% filter(Dir == "D")


pnt = geom_point(alpha = 0.8)
pnt2 = geom_point(alpha = 0.8, shape = 1)
a = theme_minimal()
awoLegend = theme_minimal() + theme(legend.position = "none")
# axTime = scale_x_datetime(date_labels = "%b %d", date_breaks = "2 weeks")
axTime = scale_x_datetime(date_labels = "%d", date_breaks = "5 days")
axTime2 = scale_x_datetime(date_labels = "%H", date_breaks = "2 hours")

labDeg = labs(x = "t, d", y = "E, kWh", shape = "Tips", col = "Lenkis")
labDir = labs(x = "t, d", y = "E, kWh", shape = "Tips", col = "Virziens")
labDeg2 = labs(x = "t, h", y = "E, Wh", shape = "Tips", col = "Lenkis")
labDir2 = labs(x = "t, h", y = "E, Wh", shape = "Tips", col = "Virziens")

labDeg3 = labs(x = "t, d", y = "delta E, Wh", shape = "Tips", col = "Lenkis")
labDir3 = labs(x = "t, d", y = "delta E, Wh", shape = "Tips", col = "Virziens")

labDegm2 = labs(x = "t, d", y = "E, kWh/m2", shape = "Tips", col = "Lenkis")
labDirm2 = labs(x = "t, d", y = "E, kWh/m2", shape = "Tips", col = "Virziens")
labDeg2m2 = labs(x = "t, h", y = "E, Wh/m2", shape = "Tips", col = "Lenkis")
labDir2m2 = labs(x = "t, h", y = "E, Wh/m2", shape = "Tips", col = "Virziens")

labDegType = labs(x = "Laiks", y = "E, kWh", col = "Leņķis")
labDirType = labs(x = "Laiks", y = "E, kWh", col = "Virziens")
labBase = labs(x = "Laiks", y = "E, kWh")

gridDir = facet_grid(. ~ Dir)
gridDeg = facet_grid(. ~ Degree)
shp = scale_shape_manual(values=c(1, 2))
shp3 = scale_shape_manual(values=c(".", "."))

intrval1 = c(min(sumHour$timestamp) + days(19) + hours(7), min(sumHour$timestamp) + days(20) - hours(7))
cord = coord_cartesian(xlim = intrval1)


deg = function(data, param){
    if (param == "Wh"){
    d = ggplot(data, aes(x = timestamp, y = Wh/1000, col = Degree, shape = Type)) +
        pnt + axTime + labDeg + shp + ggtitle("Direction: D") + scaleColD
    d2 = d + facet_grid(Type ~ Degree)  + awoLegend
    export_pdf(d2, "Deg")
    d = ggplot(data, aes(x = timestamp, y = Wh/1000, fill = Degree, shape = Type)) +
        geom_bar(stat = "identity", position=position_dodge()) +
        axTime + labDeg + shp + ggtitle("Direction: D") + scaleFillD
    d2 = d + facet_grid(Type ~ Degree)  + awoLegend
    export_pdf(d2, "Degbar")
    } else {
        d = ggplot(data, aes(x = timestamp, y = Whm2/1000, col = Degree, shape = Type)) +
            pnt + axTime + labDegm2 + shp + ggtitle("Direction: D") + scaleColD
        d2 = d + facet_grid(Type ~ Degree)  + awoLegend
        export_pdf(d2, "Degm2")
        d = ggplot(data, aes(x = timestamp, y = Whm2/1000, fill = Degree, shape = Type)) +
            geom_bar(stat = "identity", position=position_dodge()) +
            axTime + labDegm2 + shp + ggtitle("Direction: D") + scaleFillD
        d2 = d + facet_grid(Type ~ Degree)  + awoLegend
        export_pdf(d2, "Degbarm2")
        }
}

dir = function(data, param){
    if (param == "Wh"){
    p = ggplot(data, aes(x = timestamp, y = Wh/1000, col = Dir, shape = Type)) +
        pnt + axTime + labDir + shp + ggtitle("Degree: 13") + scaleCol13
    p2 = p + facet_grid(Type ~ Dir) + awoLegend
    export_pdf(p2, "Dir")
    p = ggplot(data, aes(x = timestamp, y = Wh/1000, fill = Dir, shape = Type)) +
        geom_bar(stat = "identity", position=position_dodge()) + 
        axTime + labDir + shp + ggtitle("Degree: 13") + scaleFill13
    p2 = p + facet_grid(Type ~ Dir) + awoLegend
    export_pdf(p2, "Dirbar")
    }
    else{
        p = ggplot(data, aes(x = timestamp, y = Whm2/1000, col = Dir, shape = Type)) +
            pnt + axTime + labDirm2 + shp + ggtitle("Degree: 13") + scaleCol13
        p2 = p + facet_grid(Type ~ Dir) + awoLegend
        export_pdf(p2, "Dirm2")
        p = ggplot(data, aes(x = timestamp, y = Whm2/1000, fill = Dir, shape = Type)) +
            geom_bar(stat = "identity", position=position_dodge()) + 
            axTime + labDirm2 + shp + ggtitle("Degree: 13") + scaleFill13
        p2 = p + facet_grid(Type ~ Dir) + awoLegend
        export_pdf(p2, "Dirbarm2")
    }
}


dayDeg  = function(data, param){
    if (param == "Wh"){
    d = ggplot(data, aes(x = timestamp, y = Wh, col = Degree, shape = Type)) +
        geom_line() + axTime2 + labDeg2 + shp + ggtitle("Direction: D") + scaleColD + cord
    d2 = d + facet_grid(Type ~ Degree)  + awoLegend
    export_pdf(d2, "Deg_l")
    }
    else{
        d = ggplot(data, aes(x = timestamp, y = Whm2, col = Degree, shape = Type)) +
            geom_line() + axTime2 + labDeg2m2 + shp + ggtitle("Direction: D") + scaleColD + cord
        d2 = d + facet_grid(Type ~ Degree)  + awoLegend
        export_pdf(d2, "Deg_lm2")
    }
}

dayDir  = function(data, param){
    if (param == "Wh"){
    p = ggplot(data, aes(x = timestamp, y = Wh, col = Dir, shape = Type)) +
        geom_line() + axTime2 + labDir2 + shp + ggtitle("Degree: 13") + scaleCol13 + cord
    p2 = p + facet_grid(Type ~ Dir) + awoLegend
    export_pdf(p2, "Dir_l")
    }
    else{
        p = ggplot(data, aes(x = timestamp, y = Whm2, col = Dir, shape = Type)) +
            geom_line() + axTime2 + labDir2m2 + shp + ggtitle("Degree: 13") + scaleCol13 + cord
        p2 = p + facet_grid(Type ~ Dir) + awoLegend
        export_pdf(p2, "Dir_lm2")
    }
}

difDir = function(data){
    p = ggplot(data, aes(x = timestamp, y = Wh, col = Dir, shape = Type)) +
        geom_point(alpha = 0.3) + axTime + labDir3 + shp3 + ggtitle("Degree: 13") + scaleCol13
    p2 = p + facet_grid(Type ~ Dir) + awoLegend
    export_pdf(p2, "difDir")
}

difDeg = function(data){
    p = ggplot(data, aes(x = timestamp, y = Wh, col = Degree, shape = Type)) +
        geom_point(alpha = 0.3) + axTime + labDeg3 + shp3 + ggtitle("Direction: D") + scaleColD
    p2 = p + facet_grid(Type ~ Degree) + awoLegend
    export_pdf(p2, "difDeg")
}

month  = function(data){
    # not working cuz x = timestamp is invalid (every panel occupys same spot and it looks like gray rectangle)
    p = ggplot(data, aes(x = timestamp, y = Wh/1000)) + geom_bar(stat = "identity", position=position_dodge()) + ggtitle("Marts")
    export_pdf(p, "mar_bar")
}


deg(day.D, "Wh")
dir(day.13, "Wh")
dayDeg(min.D, "Wh")
dayDir(min.13, "Wh")
difDeg(minDif.D)
difDir(minDif.13)

# deg(daym2.D, "Whm2")
# dir(daym2.13, "Whm2")
# dayDeg(minm2.D, "Whm2")
# dayDir(minm2.13, "Whm2")
