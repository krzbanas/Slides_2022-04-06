library(ggplot2)


df1<-data.frame (c(1,3), c(2,6))
colnames(df1)<- c("x", "y")

ggplot(df1, aes(x=x, y=y)) + geom_point(size=3)

ggplot(df1, aes(x=x, y=y)) + geom_point(size=3)+geom_smooth(method=lm,se=FALSE)

df2<-rbind(df1, c(2,4.2))

ggplot(df2, aes(x=x, y=y)) + geom_point(size=3)

ggplot(df2, aes(x=x, y=y)) + geom_point(size=3)+geom_smooth(method=lm,se=FALSE)

ggplot(df2, aes(x=x, y=y)) + geom_point(size=5)+geom_smooth(method=lm,se=FALSE, size=6)
