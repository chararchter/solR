library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)

default = "F:\\Users\\Janis\\VIKA\\solR\\data\\mar\\"
setwd(default)

lstData = list.files(pattern="*.csv")
data = read.csv("2019-03_whMins.csv", header = TRUE, sep = ",")
# data$timestamp = as.POSIXct(strptime(data$timestamp, format="%Y-%m-%d"))
data$timestamp = as.POSIXct(strptime(data$timestamp, format="%Y-%m-%d %H:%M:%S"))

export_pdf <- function(p, namePanel){
    month = "mar"
    ggsave(filename = paste0(month, "_", namePanel, ".pdf"), plot = p,
           height=9, width=16, units = "cm")
}

# colPan = c("#00B788", "#036C9B", "#003554", "#03C133", "#00612E")
# names(colPan) <- c("D13", "D40", "D90", "A13", "R13")
# colScale <- scale_colour_manual(values=colPan)
# fillScale = scale_fill_manual(values=colPan)


col13 = c("#00B788", "#03C133", "#00612E")
names(col13) <- c("D", "A", "R")

colD = c("#00B788", "#036C9B", "#003554")
names(colD) <- c("13", "40", "90")

scaleCol13 <- scale_colour_manual(name = "Dir", values=col13)
scaleColD <- scale_colour_manual(name = "Degree", values=colD)


data.tidy <- data %>%
    gather(key, Wh, -timestamp) %>%
    separate(key, c("Dir", "Degree", "Type"), "\\.")

data.tidy$Dir= as.factor(data.tidy$Dir)
data.tidy$Degree= as.factor(data.tidy$Degree)
data.tidy$Type= as.factor(data.tidy$Type)

data.JA = data.tidy %>% filter(Type == "JA")
data.LG = data.tidy %>% filter(Type == "LG")

data.13 = data.tidy %>% filter(Degree == "13")
data.D = data.tidy %>% filter(Dir == "D")

pnt = geom_point(alpha = 0.8)
pnt2 = geom_point(alpha = 0.8, shape = 1)
a = theme_minimal()
awoLegend = theme_minimal() + theme(legend.position = "none")
axTime = scale_x_datetime(date_labels = "%b %d", date_breaks = "2 weeks")

labDeg = labs(x = "Laiks", y = "kWh", shape = "Tips", col = "Lenkis")
labDir = labs(x = "Laiks", y = "kWh", shape = "Tips", col = "Virziens")
labDegType = labs(x = "Laiks", y = "kWh", col = "Leņķis")
labDirType = labs(x = "Laiks", y = "kWh", col = "Virziens")
labBase = labs(x = "Laiks", y = "kWh")

gridDir = facet_grid(. ~ Dir)
gridDeg = facet_grid(. ~ Degree)
shp = scale_shape_manual(values=c(1, 2))

viss = function(data){
    p = ggplot(data, aes(x = timestamp, y = Wh/1000, col = Dir, shape = Type)) +
        pnt + axTime + labDeg + shp
    p2 = p + gridDeg + a
    export_pdf(p2, "Deg_JA")
    
    d = ggplot(data, aes(x = timestamp, y = Wh/1000, col = Degree, shape = Type)) +
        pnt + axTime + labDir + shp
    d2 = d + gridDir  + a
    export_pdf(d2, "Dir_JA")
}

tips = function(data, type){
    ja = ggplot(data.JA, aes(x = timestamp, y = Wh/1000, col = Dir)) +
        pnt2 + axTime + labDirType
    ja2 = ja + gridDir + a
    export_pdf(ja2, paste0("Dir_", type, "_all"))
    
    ja = ggplot(data.JA, aes(x = timestamp, y = Wh/1000, col = Degree)) +
        pnt2 + axTime + labDegType
    ja2 = ja + gridDeg + a
    export_pdf(ja2, paste0("Deg_", type, "_all"))
}


deg = function(data){
    d = ggplot(data, aes(x = timestamp, y = Wh/1000, col = Degree, shape = Type)) +
        pnt + axTime + labDeg + shp + ggtitle("Direction: D") + scaleColD
    d2 = d + facet_grid(Type ~ Degree)  + awoLegend
    export_pdf(d2, "Deg")
    d = ggplot(data, aes(x = timestamp, y = Wh/1000, col = Degree, shape = Type)) +
        geom_line() + axTime + labDeg + shp + ggtitle("Direction: D") + scaleColD
    d2 = d + facet_grid(Type ~ Degree)  + awoLegend
    export_pdf(d2, "Deg_l")
}

dir = function(data){
    p = ggplot(data, aes(x = timestamp, y = Wh/1000, col = Dir, shape = Type)) +
        pnt + axTime + labDir + shp + ggtitle("Degree: 13") + scaleCol13
    p2 = p + facet_grid(Type ~ Dir) + awoLegend
    export_pdf(p2, "Dir")
    p = ggplot(data, aes(x = timestamp, y = Wh/1000, col = Dir, shape = Type)) +
        geom_line() + axTime + labDir + shp + ggtitle("Degree: 13") + scaleCol13
    p2 = p + facet_grid(Type ~ Dir) + awoLegend
    export_pdf(p2, "Dir_l")
}

viss(data.tidy)
deg(data.D)
dir(data.13)



# ja = ggplot(data.JA.13, aes(x = timestamp, y = Wh/1000, col = Dir)) +
#     pnt2 + axTime + labBase
# ja2 = ja + facet_grid(. ~ Dir) + awoLegend
# export_pdf(ja2, "Dir_JA")
# 
# ja = ggplot(data.JA.D, aes(x = timestamp, y = Wh/1000, col = Degree)) +
#     pnt2 + axTime + labBase
# ja2 = ja + facet_grid(. ~ Degree) + awoLegend
# export_pdf(ja2, "DegD_JA")
# 
# ja = ggplot(data.JA.13, aes(x = timestamp, y = Wh/1000, col = Dir)) +
#     geom_line() + axTime + labBase
# ja2 = ja + facet_grid(. ~ Dir) + awoLegend
# export_pdf(ja2, "Dir_JAl")
# 
# ja = ggplot(data.JA.D, aes(x = timestamp, y = Wh/1000, col = Degree)) +
#     geom_line() + axTime + labBase
# ja2 = ja + facet_grid(. ~ Degree) + awoLegend
# export_pdf(ja2, "DegD_JAl")