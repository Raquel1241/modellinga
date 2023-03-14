library(reshape2)
library(ggplot2)

traindat = read.csv("./Data/Traindatlong.csv")
traindat$sqrtRUL = sqrt(traindat$RUL)
traindat$one_thirdRUL = (traindat$RUL)^(1/3)

testdata = read.csv("./Data/Battery_test.csv")

# Get actual RUL of testdata
RULdata <- data.frame(testdata$Cycle) # initalize data
for (i in 1:(ncol(testdata)-1)){ # loop over each battery
  c = c(seq(sum(!is.na(testdata[i+1])), sum(!is.na(testdata[i+1]))-
              (nrow(testdata)-1), -1))
  c[c<0] = 0 # remove negative values
  RULdata[ , ncol(RULdata) + 1] <- c # Append new column
  colnames(RULdata)[ncol(RULdata)] <- paste0("NO.", i)
}

# Data formatting
RULdata_long <- melt(RULdata, id = "testdata.Cycle") # long format
testdata_long <- melt(testdata, id = "Cycle")
testdata_long$RUL <- RULdata_long$value # add RULdata to testdata
#testdata_long$sqrtRUL = sqrt(testdata_long$RUL)
#testdata_long$one_thirdRUL = (testdata_long$RUL)^(1/3)
colnames(testdata_long)[2] <- "Battery" # change column names
colnames(testdata_long)[3] <- "Capacity"

# Plot RUL of test data
ggplot(data = testdata_long, aes(x = Capacity, y = RUL, group = Battery)) +
  ggtitle("Capacity vs. RUL of test data ") + 
  xlab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  ylab(bquote("RUL (Cycles)")) +
  #ggtitle("Figure 2: RUL (training)") +
  geom_path(size=0.5) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank())

# Fit Linear models

# DF containing prediction for RUL
predict = data.frame(Capacity = testdata_long$Capacity) 
{
#1st ord. 
lm1 = lm(RUL~Capacity, data = traindat) 
predict$lm1 = predict(lm1, new = testdata_long)

#1st ord. sqrt
lm1sqrt = lm(sqrtRUL~Capacity, data = traindat)
predict$lm1sqrt = predict(lm1sqrt, new = testdata_long)

#1st ord. 1/3
lm1third = lm(one_thirdRUL~Capacity, data = traindat)
predict$lm1third = predict(lm1third, new = testdata_long)

#2nd ord.
lm2 = lm(RUL~Capacity + I(Capacity^2), data = traindat) 
predict$lm2 = predict(lm2, new = testdata_long)

#2nd ord. sqrt
lm2sqrt = lm(sqrtRUL~Capacity + I(Capacity^2), data = traindat)
predict$lm2sqrt = predict(lm2sqrt, new = testdata_long) 

#2nd ord. 1/3
lm2third = lm(one_thirdRUL~Capacity + I(Capacity^2), data = traindat) 
predict$lm2third = predict(lm2third, new = testdata_long)

#3rd ord.
lm3 = lm(RUL~Capacity + I(Capacity^2) + I(Capacity^3), data = traindat) 
predict$lm3 = predict(lm3, new = testdata_long)

#3rd ord. sqrt
lm3sqrt = lm(sqrtRUL~Capacity + I(Capacity^2) + I(Capacity^3), data = traindat) 
predict$lm3sqrt = predict(lm3sqrt, new = testdata_long)

#3rd ord. 1/3
lm3third = lm(one_thirdRUL~Capacity + I(Capacity^2) 
              + I(Capacity^3), data = traindat)
predict$lm3third = predict(lm3third, new = testdata_long)

#4th ord.
lm4 = lm(RUL~Capacity + I(Capacity^2) + I(Capacity^3) 
         + I(Capacity^4), data = traindat) 
predict$lm4 = predict(lm4, new = testdata_long)

#4th ord. sqrt
lm4sqrt = lm(sqrtRUL~Capacity + I(Capacity^2) + I(Capacity^3) 
             + I(Capacity^4), data = traindat) 
predict$lm4sqrt = predict(lm4sqrt, new = testdata_long)

#4th ord. 1/3
lm4third = lm(one_thirdRUL~Capacity + I(Capacity^2) + 
                I(Capacity^3) + I(Capacity^4), data = traindat) 
predict$lm4third = predict(lm4third, new = testdata_long)

#5th ord.
lm5 = lm(RUL~Capacity + I(Capacity^2) + I(Capacity^3) + 
           I(Capacity^4) + I(Capacity^5), data = traindat)
predict$lm5 = predict(lm5, new = testdata_long)

#5th ord. sqrt
lm5sqrt = lm(sqrtRUL~Capacity + I(Capacity^2) + I(Capacity^3) + 
               I(Capacity^4) + I(Capacity^5), data = traindat)
predict$lm5sqrt = predict(lm5sqrt, new = testdata_long)

#5th ord. 1/3
lm5third = lm(one_thirdRUL~Capacity + I(Capacity^2) + I(Capacity^3) + 
                I(Capacity^4) + I(Capacity^5), data = traindat)
predict$lm5third = predict(lm5third, new = testdata_long)
}
# set negative values and non-existing values to 0
predict[(predict < 0) | is.na(predict)] = 0 


# Calculate errors for each method
MSE = c() # mean squared error
MAPE = c() # mean absolute percentage error

for (i in seq(2,ncol(predict),3)){ # loop over each linear fit in steps of 3
    MSE = append(MSE,mean((testdata_long$RUL - predict[,i])^2)) # calculate MSE
    MSE = append(MSE,mean((sqrt(testdata_long$RUL) - predict[,i+1])^2)) # sqrt method
    MSE = append(MSE,mean(((testdata_long$RUL)^(1/3) - predict[,i+2])^2)) # inverse cube
    
    RUL  = testdata_long$RUL
    x1 = abs((RUL-predict[,i])/RUL) * 100 # calculate absolute percentage error
    x1[is.na(x1)] <- 0 # set NaN to 0
    MAPE = append(MAPE,mean(x1))
    
    # same for sqrt
    RUL = sqrt(testdata_long$RUL)
    x2 = abs((RUL-predict[,i+1])/RUL) * 100 # calculate absolute percentage error
    x2[is.na(x2)] <- 0 # set NaN to 0
    MAPE = append(MAPE,mean(x2))
    
    # Inverse cube
    RUL = testdata_long$RUL^(1/3)
    x3 = abs((RUL-predict[,i+2])/RUL) * 100 # calculate absolute percentage error
    x3[is.na(x3)] <- 0 # set NaN to 0
    MAPE = append(MAPE,mean(x3))

}

error <- data.frame(MSE = MSE, MAPE = MAPE)
# fix rownames
rownames(error)[seq(1,15,3)] <- paste0("lm", seq(1,5))
rownames(error)[seq(2,15,3)] <- paste0("lm", seq(1,5),"sqrt")
rownames(error)[seq(3,15,3)] <- paste0("lm", seq(1,5),"^(1/3)")

# Plot linear fits
ggplot() +
  xlab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  ylab(bquote("RUL (Cycles)")) +
  ggtitle("Predicted RUL vs. actual RUL") +
  geom_path(data = testdata_long, 
            aes(x = Capacity, y = RUL, group = Battery), size=0.5,lty=2) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank()) +
  geom_line(data = predict, 
            aes(x = testdata_long$Capacity, y = lm1, color = 'red'),linewidth=1) +
  geom_line(data = predict, 
            aes(x = testdata_long$Capacity, y = lm2, color = 'blue'),linewidth=1) +
  geom_line(data = predict, 
            aes(x = testdata_long$Capacity, y = lm3, color = 'green'),linewidth=1) +
  geom_line(data = predict, 
            aes(x = testdata_long$Capacity, y = lm4, color = 'purple'),linewidth=1) +
  geom_line(data = predict, 
            aes(x = testdata_long$Capacity, y = lm5, color = 'orange'),linewidth=1) +
  labs(colour="Linear model") +
  scale_color_manual(
    labels = c("Fifth order", "Fourth order","Third order", "Second order","First order"), 
    values = c("red","blue", "green","purple","orange")) 

# Plot fits on square root
ggplot() +
  xlab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  ylab(bquote("RUL" ~ C["t,i"] ~ "(Cycles)")) +
  ggtitle("Predicted RUL vs. actual RUL on square root of data ") +
  geom_path(data = testdata_long, 
            aes(x = Capacity, y = sqrt(RUL), group = Battery), size=0.5, lty=2) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank()) +
  geom_line(data = predict, aes(x = testdata_long$Capacity, y = lm1sqrt, color = 'red') ) +
  geom_line(data = predict, aes(x = testdata_long$Capacity, y = lm2sqrt, color = 'blue') ) +
  geom_line(data = predict, aes(x = testdata_long$Capacity, y = lm3sqrt, color = 'green') ) +
  geom_line(data = predict, aes(x = testdata_long$Capacity, y = lm4sqrt, color = 'yellow') ) +
  geom_line(data = predict, aes(x = testdata_long$Capacity, y = lm5sqrt, color = 'orange') ) +
  labs(colour="Linear model") +
  scale_color_manual(
    labels = c("Fifth order", "Fourth order","Third order", "Second order","First order"), 
    values = c("red","blue", "green","purple","orange")) 



# Plot fits on cube root
ggplot() +
  xlab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  ylab(bquote("RUL (Cycles)")) +
  ggtitle("Predicted RUL vs. actual RUL of data^(1/3)") +
  geom_path(data = testdata_long, aes(x = Capacity, y = RUL^(1/3), group = Battery), size=0.5, lty=2) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank()) +
  geom_line(data = predict, aes(x = testdata_long$Capacity, y = lm1third, color = 'red') ) +
  geom_line(data = predict, aes(x = testdata_long$Capacity, y = lm2third, color = 'blue') ) +
  geom_line(data = predict, aes(x = testdata_long$Capacity, y = lm3third, color = 'green') ) +
  geom_line(data = predict, aes(x = testdata_long$Capacity, y = lm4third, color = 'yellow') ) +
  geom_line(data = predict, aes(x = testdata_long$Capacity, y = lm5third, color = 'orange') ) +
  labs(colour="Linear model") +
  scale_color_manual(
    labels = c("Fifth order", "Fourth order","Third order", "Second order","First order"), 
    values = c("red","blue", "green","purple","orange")) 
  
