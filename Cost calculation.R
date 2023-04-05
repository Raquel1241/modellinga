#setwd("~/GitHub/modellinga")
library(dplyr)

# Load train and test data and linear models
load("./Data/traindata_long.Rda")
load("./Data/testdata_long.Rda")
load("./Data/linearmodels.RData")

traindata <- read.csv("./Data/Battery_train.csv", header=TRUE)
testdata <-  read.csv("./Data/Battery_test.csv", header=TRUE)


# Calculate total cost of inspection policy
calculateCost <- function(Cr,Cm,Cp,t, tau, RULhat, RULtrue) {
  # INPUT
  #   Cr:       replacement cost, INT
  #   Cm:       maintenance cost, INT
  #   Cp:       penalty cost, INT
  #   t:        time of inspection, INT
  #   tau:      inspection period
  #   RULhat:   vector containing predicted RUL, based on linear model
  #   RULtrue:  vector containing true RUL
  
  # OUTPUT
  #   cost:     cost of inspection at t cycles, integer
  #   replace:  BOOLEAN, TRUE means replace battery, FALSE means keep batter
  cost =  case_when( # define new cost
    RULhat[t] < tau ~ Cr + Cm,
    RULhat[t] > tau & RULtrue[t] < tau  ~ Cr + Cp + Cm,
    RULhat[t] > tau & RULtrue[t] > tau  ~ Cm,
    TRUE ~ 0)
  return(cost)
}

# Function that determines if a battery should be replaced
replaceBattery <- function(RULhat,t,tau){
  # INPUT
  #   RULhat:   vector containing predicted RUL, based on linear model
  #   t:        time of inspection, INT
  #   tau:      inspection period
  
  # OUTPUT
  #   replace:  BOOLEAN, TRUE means replace battery, FALSE means keep battery
  
  replace =  case_when( # replace battery?
    RULhat[t] < tau ~ TRUE,
    RULhat[t] >= tau ~ FALSE,
    TRUE ~ FALSE)
  return(replace)
}

# Add predictions
{
RULhatdata <- data.frame(Cycle=traindata_long$Cycle, Battnum=traindata_long$Battnum)
RULhatdata$lm1 <- predict(lm1, newdata = traindata_long)
RULhatdata$lm1sqrt = predict(lm1sqrt, new = traindata_long)
RULhatdata$lm1third = predict(lm1third, new = traindata_long)
RULhatdata$lm2 = predict(lm2, new = traindata_long)
RULhatdata$lm2sqrt = predict(lm2sqrt, new = traindata_long)
RULhatdata$lm2third = predict(lm2third, new = traindata_long)
RULhatdata$lm3 = predict(lm3, new = traindata_long)
RULhatdata$lm3sqrt = predict(lm3sqrt, new = traindata_long)
RULhatdata$lm3third = predict(lm3third, new = traindata_long)
RULhatdata$lm4 = predict(lm4, new = traindata_long)
RULhatdata$lm4sqrt = predict(lm4sqrt, new = traindata_long)
RULhatdata$lm4third = predict(lm4third, new = traindata_long)
RULhatdata$lm5 = predict(lm5, new = traindata_long)
RULhatdata$lm5sqrt = predict(lm5sqrt, new = traindata_long)
RULhatdata$lm5third = predict(lm5third, new = traindata_long)

RULhatdata[(RULhatdata < 0)|is.na(RULhatdata)] = 0 # set negative values to 0
}

# Run simulation
Cr = 3        # replacement cost
Cm = 0.5      # maintenance cost
Cp = 1        # penalty cost
tau = 200     # inspection period
t_end = 2000  # end of inspection
N = 1         # No. of batteries to test

randomBattery <- function(data){
  #  select N batteries without replacement
  battIndex = sample.int(max(data$Battnum),1, replace=TRUE)
  batteries = subset(data, Battnum %in% battIndex) # subset of traindata
  
  RULtrue <- batteries$RUL
  RULhat <- subset(RULhatdata, Battnum %in% battIndex)$lm1
  return
}

time <- seq(0, t_end, tau)[-1] # {200, 400, ...,2000}
offset <- numeric(0)
cost <- vector(mode="numeric", length(time))

for (t in time) { # Calculate cost of each method at each inspection time
  if (!replaceBattery(RULhat,t-offset,tau)){ # check if replaceBattery is TRUE
    cost = append(cost, calculateCost(Cr,Cm,Cp,t-offset,tau,RULhat,RULtrue))
    print(t)
  }
  else{ # replace battery
    cost = append(cost, calculateCost(Cr,Cm,Cp,t-offset,tau,RULhat,RULtrue)) 
    offset <- t # implement time offset
    
    battIndex = sample.int(max(traindata_long$Battnum),1, replace=TRUE)
    # add random new battery
    batteries = subset(traindata_long, Battnum %in% battIndex) # subset of traindata
    
    RULtrue <- batteries$RUL
    RULhat <- subset(RULhatdata, Battnum %in% battIndex)$lm1
    print("break")
    break
  }
}
costdata <- data.frame(time = time, cost = cost)
time = c(tau)
while (!replaceBattery(RULhat,t,tau) & tail(time,1) < t_end){
  
  
}


