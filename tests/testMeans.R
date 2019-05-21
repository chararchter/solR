library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)

# change font to serif so it can display latvian language correctly
theme_map <- theme_get()
theme_map$text$family = "serif"
theme_map$text$size = 12

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
datMin$timestamp = as.POSIXct(datMin$timestamp)
datMin.tidy = tidy(datMin)
datMin.tidy = datMin.tidy  %>% separate(timestamp, c("date", "time"), sep=" ")

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

# d = ggplot(data.D, aes(x = fakeTime, y = Wh, col = Degree, linetype = Type)) +
#     geom_line(alpha = 0.5) + axTime + labDeg2 + shp + ggtitle("Direction: D") + scaleColD
# d2 = d + facet_grid(Type ~ Degree)
# export_pdf(d2, "test")

datMin.tidier$Date = as.numeric(datMin.tidier$Date)
datMin.tidier$Degree = as.numeric(datMin.tidier$Degree)
write.csv(datMin.tidier, file = "killme.csv", row.names=FALSE)

data.test = datMin.tidier %>% filter(Dir == "D" & Degree == 40 & Type == "JA" )
data.test = data.test %>% filter(Date %% 2 == 0)

d = ggplot(data.test, aes(x = fakeTime, y = Wh, group = as.numeric(Date))) + geom_line(aes(col = Date)) +
    scale_colour_gradient2() + labs(x = "t, h", col = "Šaursliežu dzelzceļš") +
    # theme_classic() +
    theme_map 
export_pdf(d, "test5")