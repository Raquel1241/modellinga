library(ggplot2)

traindata = read.csv("./Data/Battery_train.csv")
df <- traindata[,1:2] # take only first battery

# fit polynomial regression models up to degree 5
fit1 <- lm(df$NO.1~df$Cycle)
fit2 <- lm(df$NO.1~poly(df$Cycle,2,raw=TRUE))
fit3 <- lm(df$NO.1~poly(df$Cycle,3,raw=TRUE))
fit4 <- lm(df$NO.1~poly(df$Cycle,4,raw=TRUE))
fit5 <- lm(df$NO.1~poly(df$Cycle,5,raw=TRUE))

# create a scatterplot of cycle vs. capacity
plot(df$Cycle, df$NO.1, pch=19, xlab='Cycle', ylab='Capacity', type="l", lwd=2)

#add curve of each model to plot
lines(x_axis, predict(fit1, data.frame(x=df$Cycle)), col='green')
lines(x_axis, predict(fit2, data.frame(x=df$Cycle)), col='red')
lines(x_axis, predict(fit3, data.frame(x=df$Cycle)), col='purple')
lines(x_axis, predict(fit4, data.frame(x=df$Cycle)), col='blue')
lines(x_axis, predict(fit5, data.frame(x=df$Cycle)), col='orange')

legend(0, 1, legend=c("Linear", "First degree", "Second degree", "Third degree", "Fourth degree", "Fifth degree"),
       col=c("green", "red", "purple", "blue", "orange"), lty=1:2, cex=0.6)

#calculated adjusted R-squared of each model
summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared


