# Create some data
df2 <- data.frame(supp=rep(c("VC", "OJ"), each=3),
                  dose=rep(c("0.5", "1", "2"),2),
                  len=c(6.8, 15, 33, 4.2, 10, 29.5))
head(df2)

# x axis treated as continuous variable
df2$dose <- as.numeric(as.vector(df2$dose))
ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
    geom_bar(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette="Paired")+
    theme_minimal()
# Axis treated as discrete variable
df2$dose<-as.factor(df2$dose)
ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
    geom_bar(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette="Paired")+
    theme_minimal()