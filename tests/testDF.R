n = 6
time = seq(1,n)
y0 = rnorm(n)
y1 = rnorm(n)
y2 = rnorm(n)
y3 = rnorm(n)
y4 = rnorm(n)

# dat = data.frame(time, y0, y1, y2, y3, y4)
# print(dat)
# # printē pirmās 3 rindas
# dat2 = dat[1:3,]
# # printē pirmās 3 kolonnas
# dat2 = dat[,1:3]
# # printē pirmo kolonnu un trešo ceturto kollonu
# dat2 = dat[,1] + dat[,3:4]
# dat3 = data.frame(time = dat[,1], dat[,3:4])
# 
# print(dat2)
# print(dat3)


dat1 = data.frame(time, "test"= y0)
dat2 = data.frame(time, "test"= y1)
datall = rbind(dat1, dat2)