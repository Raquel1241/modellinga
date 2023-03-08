install.packages('ggfortify')

traindat = read.csv("/Users/tyrenkoning/Desktop/University/Modelling-2A/Traindatlong.csv")
traindat$sqrtRUL = sqrt(traindat$RUL)
traindat$one_thirdRUL = (traindat$RUL)^(1/3)

predict = data.frame(Capacity = seq(0.88, 1.1, 0.001))

#Linear Regression 1st order
{
lm1 = lm(RUL~Capacity, data = traindat)  #Create a linear regression with two variables
summary(lm1) #Review the results
acf(lm1$residuals, type = "correlation") #Test for autocorrelation, shows why lin. reg. not allowed
predict$lm1 = predict(lm1, new = predict)
predict$lm1[predict$lm1 < 0] = 0

ggplot() +
  xlab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  ylab(bquote("RUL" ~ C["t,i"] ~ "(Cycles)")) +
  #ggtitle("Figure 2: RUL (training)") +
  geom_path(data = traindat, aes(x = Capacity, y = RUL, group = Battnum), size=0.5) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank()) +
  geom_line(data = predict, aes(x = Capacity, y = lm1), color = 'Red')
}

#Linear Regression 2nd order
{lm2 = lm(RUL~Capacity + I(Capacity^2), data = traindat) 
summary(lm2)
acf(lm1$residuals, type = "correlation")

predict$lm2 = predict(lm2, new = predict)
predict$lm2[predict$lm2 < 0] = 0

ggplot() +
  xlab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  ylab(bquote("RUL" ~ C["t,i"] ~ "(Cycles)")) +
  #ggtitle("Figure 2: RUL (training)") +
  geom_path(data = traindat, aes(x = Capacity, y = RUL, group = Battnum), size=0.5) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank()) +
  geom_line(data = predict, aes(x = Capacity, y = lm2), color = 'Red')}

#Linear Regression 3rd order
{lm3 = lm(RUL~Capacity + I(Capacity^2) + I(Capacity^3), data = traindat) 
summary(lm3) 
acf(lm1$residuals, type = "correlation") 

predict$lm3 = predict(lm3, new = predict)
predict$lm3[predict$lm3 < 0] = 0

ggplot() +
  xlab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  ylab(bquote("RUL" ~ C["t,i"] ~ "(Cycles)")) +
  #ggtitle("Figure 2: RUL (training)") +
  geom_path(data = traindat, aes(x = Capacity, y = RUL, group = Battnum), size=0.5) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank()) +
  geom_line(data = predict, aes(x = Capacity, y = lm3), color = 'Red')}

#Linear Regression 4th order
{lm4 = lm(RUL~Capacity + I(Capacity^2) + I(Capacity^3) + I(Capacity^4), data = traindat) 
summary(lm4) 
acf(lm1$residuals, type = "correlation") 

predict$lm4 = predict(lm4, new = predict)
predict$lm4[predict$lm4 < 0] = 0

ggplot() +
  xlab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  ylab(bquote("RUL" ~ C["t,i"] ~ "(Cycles)")) +
  #ggtitle("Figure 2: RUL (training)") +
  geom_path(data = traindat, aes(x = Capacity, y = RUL, group = Battnum), size=0.5) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank()) +
  geom_line(data = predict, aes(x = Capacity, y = lm4), color = 'Red')}

#Linear Regression 5th order
{lm5 = lm(RUL~Capacity + I(Capacity^2) + I(Capacity^3) + I(Capacity^4) + I(Capacity^5), data = traindat) 
summary(lm5) 
acf(lm1$residuals, type = "correlation") 

library(ggfortify)
autoplot(lm5)

predict$lm5 = predict(lm5, new = predict)
predict$lm5[predict$lm5 < 0] = 0

ggplot() +
  xlab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  ylab(bquote("RUL" ~ C["t,i"] ~ "(Cycles)")) +
  #ggtitle("Figure 2: RUL (training)") +
  geom_path(data = traindat, aes(x = Capacity, y = RUL, group = Battnum), size=0.5) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank()) +
  geom_line(data = predict, aes(x = Capacity, y = lm5), color = 'Red')}

#Linear Regression 5th order sqrt data
{lm5 = lm(sqrtRUL~Capacity + I(Capacity^2) + I(Capacity^3) + I(Capacity^4) + I(Capacity^5), data = traindat) 
  summary(lm5) 
  acf(lm1$residuals, type = "correlation") 
  
  predict$lm5 = predict(lm5, new = predict)
  predict$lm5[predict$lm5 < 0] = 0
  
  ggplot() +
    xlab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
    ylab(bquote("RUL" ~ C["t,i"] ~ "(Cycles)")) +
    #ggtitle("Figure 2: RUL (training)") +
    geom_path(data = traindat, aes(x = Capacity, y = RUL, group = Battnum), size=0.5) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = 12), 
          panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_blank()) +
    geom_line(data = predict, aes(x = Capacity, y = lm5), color = 'Red')}

#Linear Regression 5th order 1/3 data
{lm5onethird = lm(one_thirdRUL~Capacity + I(Capacity^2) + I(Capacity^3) + I(Capacity^4) + I(Capacity^5), data = traindat) 
  summary(lm5onethird) 
  acf(lm1$residuals, type = "correlation") 
  
  autoplot(lm5onethird)
  
  predict$lm5 = predict(lm5, new = predict)
  predict$lm5[predict$lm5 < 0] = 0
  #ci90 <- predict(lm5, new = predict, interval = "confidence", level = 0.90)
  ci95 <- as.data.frame(predict(lm5, new = predict, interval = "confidence", level = 0.95))
  traindat$CI95low = ci95$lwr
  traindat$CI95up = ci95$upr
  #ci99 <- predict(lm5, new = predict, interval = "confidence", level = 0.99)
  
  ci_pred <- as.data.frame(ci90) %>% 
    mutate(ID = "90%") %>% 
    bind_rows(as.data.frame(ci95) %>% 
                mutate(ID = "95%"),
              as.data.frame(ci99) %>% 
                mutate(ID = "99%")
    )
  
  
  ggplot() +
    xlab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
    ylab(bquote("RUL" ~ C["t,i"] ~ "(Cycles)")) +
    #ggtitle("Figure 2: RUL (training)") +
    geom_path(data = traindat, aes(x = Capacity, y = one_thirdRUL, group = Battnum), size=0.5) +
    geom_line(data = predict, aes(x = Capacity, y = ci95.lwr), color = 'Blue', lty = 2) +
    geom_line(data = predict, aes(x = Capacity, y = ci95.upr), color = 'Blue', lty = 2) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = 12), 
          panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_blank()) +
    geom_line(data = predict, aes(x = Capacity, y = lm5), color = 'Red')}
