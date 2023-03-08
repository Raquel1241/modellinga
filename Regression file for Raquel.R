library(reshape2)
library(ggplot2)


traindat = read.csv("./Data/Traindatlong.csv")
traindat$sqrtRUL = sqrt(traindat$RUL)
traindat$one_thirdRUL = (traindat$RUL)^(1/3)

testdata = read.csv("./Data/Battery_test.csv")

RULdata <- data.frame(testdata$Cycle) # initalize data
for (i in 1:(ncol(testdata)-1)){ # loop over each battery
  c = c(seq(sum(!is.na(testdata[i+1])), sum(!is.na(testdata[i+1]))-(nrow(testdata)-1), -1))
  c[c<0] = 0 # remove negative values
  RULdata[ , ncol(RULdata) + 1] <- c # Append new column
  colnames(RULdata)[ncol(RULdata)] <- paste0("NO.", i)
}

# Data formatting
RULdata_long <- melt(RULdata, id = "testdata.Cycle") # long format
testdata_long <- melt(testdata, id = "Cycle")
testdata_long$RUL <- RULdata_long$value # add RULdata to testdata
colnames(testdata_long)[2] <- "Battery" # change column names
colnames(testdata_long)[3] <- "Capacity"

# Plot RUL of test data
ggplot(data = testdata_long, aes(x = Capacity, y = RUL, group = Battery)) +
  ggtitle("Capacity vs. RUL of test data ") + 
  xlab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  ylab(bquote("RUL" ~ C["t,i"] ~ "(Cycles)")) +
  #ggtitle("Figure 2: RUL (training)") +
  geom_path(size=0.5) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank())

# Capacity sequence
predict = data.frame(Capacity = seq(0.88, 1.1, by = (1.1-0.88)/(nrow(testdata)-1)))

# Fit linear models
#1st ord. 
lm1 = lm(RUL~Capacity, data = traindat)  #Create a linear regression with two variables
predict$lm1 = predict(lm1, new = predict[1])
predict$lm1[predict$lm1 < 0] = 0

# Calculate MSE for each method and for each battery
MSE = data.frame(matrix(ncol = ncol(testdata)-1, nrow = 0)) # initialize MSE
colnames(MSE) <- paste0("NO.", seq(1,ncol(testdata)-1)) # change column names

for (i in 1:(ncol(RULdata)-1)){ # loop over batteries
  val = mean((RULdata[,i+1] - rev(predict$lm1))^2) # calculate MSE
  MSE[1,i] <- val # Append new value
}
MSE$total = rowMeans(MSE) # append total MSE of method

# Make scatterplot of MSE
x <- seq(1,ncol(MSE)-1)
plot(x, MSE[-ncol(MSE)], main="Scatterplot of MSE using a first-order linear approximation",
     xlab="Battery No. ", ylab="MSE ", pch=19)


ggplot() +
  xlab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  ylab(bquote("RUL" ~ C["t,i"] ~ "(Cycles)")) +
  ggtitle("Predicted RUL vs. actual RUL") +
  geom_path(data = testdata_long, aes(x = Capacity, y = RUL, group = Battery), size=0.5) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank()) +
  geom_line(data = predict, aes(x = Capacity, y = lm1), color = 'Red') + 
  annotate("text", x=0.91, y=500, label= "Predicted RUL", colour = "red", size = 3.1)
  
  
#1st ord. sqrt
lm1sqrt = lm(sqrtRUL~Capacity, data = traindat)

#1st ord. 1/3
lm1third = lm(one_thirdRUL~Capacity, data = traindat)

#2nd ord.
lm2 = lm(RUL~Capacity + I(Capacity^2), data = traindat) 

#2nd ord. sqrt
lm2sqrt = lm(sqrtRUL~Capacity + I(Capacity^2), data = traindat) 

#2nd ord. 1/3
lm2third = lm(one_thirdRUL~Capacity + I(Capacity^2), data = traindat) 

#3rd ord.
lm3 = lm(RUL~Capacity + I(Capacity^2) + I(Capacity^3), data = traindat) 

#3rd ord. sqrt
lm3sqrt = lm(sqrtRUL~Capacity + I(Capacity^2) + I(Capacity^3), data = traindat) 

#3rd ord. 1/3
lm3third = lm(one_thirdRUL~Capacity + I(Capacity^2) + I(Capacity^3), data = traindat) 

#4th ord.
lm4 = lm(RUL~Capacity + I(Capacity^2) + I(Capacity^3) + I(Capacity^4), data = traindat) 

#4th ord. sqrt
lm4sqrt = lm(sqrtRUL~Capacity + I(Capacity^2) + I(Capacity^3) + I(Capacity^4), data = traindat) 

#4th ord. 1/3
lm4third = lm(one_thirdRUL~Capacity + I(Capacity^2) + I(Capacity^3) + I(Capacity^4), data = traindat) 

#5th ord.
lm5 = lm(RUL~Capacity + I(Capacity^2) + I(Capacity^3) + I(Capacity^4) + I(Capacity^5), data = traindat)

#5th ord. sqrt
lm5sqrt = lm(sqrtRUL~Capacity + I(Capacity^2) + I(Capacity^3) + I(Capacity^4) + I(Capacity^5), data = traindat)

#5th ord. 1/3
lm5third = lm(one_thirdRUL~Capacity + I(Capacity^2) + I(Capacity^3) + I(Capacity^4) + I(Capacity^5), data = traindat)




