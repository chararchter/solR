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

sumMonth %>%
    gather(key, Wh, -timestamp, -type) %>%
    separate(key, c("DirDegree", "Type"), "\\.")

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

scaleCol13 <- scale_colour_manual(name = "Dir", values=col13)
scaleColD <- scale_colour_manual(name = "Degree", values=colD)

scaleFill13 <- scale_fill_manual(name = "Dir", values=col13)
scaleFillD <- scale_fill_manual(name = "Degree", values=colD)

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
labDegType = labs(x = "Laiks", y = "E, kWh", col = "Leņķis")
labDirType = labs(x = "Laiks", y = "E, kWh", col = "Virziens")
labBase = labs(x = "Laiks", y = "E, kWh")

gridDir = facet_grid(. ~ Dir)
gridDeg = facet_grid(. ~ Degree)
shp = scale_shape_manual(values=c(1, 2))

intrval1 = c(min(sumHour$timestamp) + days(19) + hours(7), min(sumHour$timestamp) + days(20) - hours(7))
cord = coord_cartesian(xlim = intrval1)


deg = function(data){
    d = ggplot(data, aes(x = timestamp, y = Wh/1000, col = Degree, shape = Type)) +
        pnt + axTime + labDeg + shp + ggtitle("Direction: D") + scaleColD
    d2 = d + facet_grid(Type ~ Degree)  + awoLegend
    export_pdf(d2, "Deg")
    d = ggplot(data, aes(x = timestamp, y = Wh/1000, fill = Degree, shape = Type)) +
        geom_bar(stat = "identity", position=position_dodge()) +
        axTime + labDeg + shp + ggtitle("Direction: D") + scaleFillD
    d2 = d + facet_grid(Type ~ Degree)  + awoLegend
    export_pdf(d2, "Degbar")
}

dir = function(data){
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


dayDeg  = function(data){
    d = ggplot(data, aes(x = timestamp, y = Wh, col = Degree, shape = Type)) +
        geom_line() + axTime2 + labDeg2 + shp + ggtitle("Direction: D") + scaleColD + cord
    d2 = d + facet_grid(Type ~ Degree)  + awoLegend
    export_pdf(d2, "Deg_l")
}

dayDir  = function(data){
    p = ggplot(data, aes(x = timestamp, y = Wh, col = Dir, shape = Type)) +
        geom_line() + axTime2 + labDir2 + shp + ggtitle("Degree: 13") + scaleCol13 + cord
    p2 = p + facet_grid(Type ~ Dir) + awoLegend
    export_pdf(p2, "Dir_l")
}

month  = function(data){
    p = ggplot(data, aes(x = timestamp, y = Wh, col = Dir, shape = Type)) +
        geom_line() + axTime2 + labDir2 + shp + ggtitle("Degree: 13") + scaleCol13 + cord
    p2 = p + facet_grid(Type ~ Dir) + awoLegend
    export_pdf(p2, "Dir_l")
}


deg(day.D)
dir(day.13)

dayDeg(min.D)
dayDir(min.13)