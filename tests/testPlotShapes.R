library(ggplot2)
library(wesanderson)
library(RColorBrewer)

x = seq(1,10)
y = 2*x + 4
z = 2*x

df = data.frame(x,y,z,k)

p = ggplot(df, aes(x=x, y=value, color = variable, shape = variable)) +
    geom_point(aes(y = y, col = "y", shape = "y")) +
    geom_point(aes(y = z, col = "z", shape = "z")) +
    theme_minimal() + xlab("Day") + ylab("Wh") + 
    scale_color_manual(values = wes_palette("IsleofDogs2", 2))
    # scale_color_manual(values = wes_palette("GrandBudapest1", 3))
    # scale_color_manual(values = brewer.pal(n = 3, name = "Dark2"))
    # scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))

p

# The shape palette can deal with a maximum of 6 discrete values because more than 6
# becomes difficult to discriminate; you have 7. Consider specifying shapes manually if you
# must have them. 

# p = ggplot(df, aes(x=x, y=value, color = variable, shape = variable)) +
#     geom_point(aes(y = y)) + (aes(shape="y", color="y"))
#     geom_point(aes(y = z)) + (aes(shape="z", color="z"))
#     geom_point(aes(y = k)) + (aes(shape="k", color="k"))
#     theme_minimal() + xlab("Day") + ylab("Wh")
# p


x = seq(1,5)
y = 2*x 
z = 2*x +1
k = 2*x +2
j = 2*x +3
a = 2*x +4
b = 2*x +5
c = 2*x +6

df = data.frame(x,y,z, k, j, a, b, c)

p = ggplot(df, aes(x=x, y=value, color = variable)) +
    geom_point(aes(y = y, col = "y")) +
    geom_point(aes(y = z, col = "z")) +
    geom_point(aes(y = k, col = "k")) +
    geom_point(aes(y = j, col = "j")) +
    geom_point(aes(y = a, col = "a")) +
    geom_point(aes(y = b, col = "b")) +
    geom_point(aes(y = c, col = "c")) +
    theme_minimal() + xlab("Day") + ylab("Wh") + colr

p