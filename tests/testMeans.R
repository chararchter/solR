library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)

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

datMin = read.csv("2019-03_whMins_atstarpes.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
# datMin = read.csv("sumHour_atstarpes.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
datMin.tidy = tidy(datMin)
datMin.tidy = datMin.tidy  %>% separate(timestamp, c("date", "time"), sep=" ")

# dummyDate = rep(datMin.tidy$date[1], each=nrow(datMin.tidy))
dummyDate = datMin.tidy$date[1]
datMin.tidy$date = ymd(datMin.tidy$date)
datMin.tidy$date = format(datMin.tidy$date, format="%y%m-%d")

datMin.tidier <- datMin.tidy %>%
    separate(date, into = c("YearMonth", "Date"), sep = "-")

datMin.tidier$fakeTime <- as.POSIXct(paste(dummyDate, datMin.tidy$time))

print(head(datMin.tidier))
datMin.tidier = datMin.tidier[-1]

col13 = c("#00B788", "#03C133", "#00612E")
names(col13) <- c("D", "A", "R")
colD = c("#00B788", "#036C9B", "#003554")
names(colD) <- c("13", "40", "90")
axTime = scale_x_datetime(date_labels = "%H:%M", date_breaks = "5 hours")
labDeg2 = labs(x = "t, h", y = "E, Wh", shape = "Tips", col = "Lenkis")
labDir2 = labs(x = "t, h", y = "E, Wh", shape = "Tips", col = "Virziens")
shp = scale_shape_manual(values=c(1, 2))
# scale_colour_gradient(colours = terrain.colors(10))


data.13 = datMin.tidier %>% filter(Degree == "13")
data.D = datMin.tidier %>% filter(Dir == "D")
# experiment$Grouping <- as.factor(experiment$Group) 

# d = ggplot(data.D, aes(x = fakeTime, y = Wh, col = Degree, linetype = Type)) +
#     geom_line(alpha = 0.5) + axTime + labDeg2 + shp + ggtitle("Direction: D") + scaleColD
# d2 = d + facet_grid(Type ~ Degree)
# export_pdf(d2, "test")

# testScale = scale_color_manual(values=["blue", "purple", "orange"])
#    scale_colour_gradient(low = "#132B43", high = "#56B1F7",
#                      space = "Lab", na.value = "grey50", guide = "colourbar",
#                      aesthetics = "colour")


datMin.tidier$Date = as.numeric(datMin.tidier$Date)
datMin.tidier$Degree = as.numeric(datMin.tidier$Degree)
write.csv(datMin.tidier, file = "killme.csv", row.names=FALSE)

# geom_line(data=data.frame(spline(data.test, n=500)))

data.test = datMin.tidier %>% filter(Dir == "D" & Degree == 40 & Type == "JA" & Date < 3)
data.test <- transform(data.test,Date=factor(Date,levels=unique(Date)))
write.csv(data.test, file = "killme666.csv", row.names=FALSE)
data.spline = data.frame(fakeTime = data.test$fakeTime, Wh = data.test$Wh)

d = ggplot(data.test, aes(x = fakeTime, y = Wh, col = Date)) +
    geom_line(alpha = 0.5) + scale_color_manual(values=c("blue", "purple"))
    # geom_line(data=data.frame(spline(data.spline, n=300)))
# d2 = d + facet_grid(Type ~ Degree)
export_pdf(d, "test1")