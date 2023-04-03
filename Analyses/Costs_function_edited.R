#setwd("~/GitHub/modellinga")
library(dplyr)

# Load train and test data and linear models
load("/Users/tyrenkoning/Documents/GitHub/modellinga/Data/traindata_long.Rda")
load("/Users/tyrenkoning/Documents/GitHub/modellinga/Data/testdata_long.Rda")
load("/Users/tyrenkoning/Documents/GitHub/modellinga/Data/linearmodels.RData")
load("/Users/tyrenkoning/Documents/GitHub/modellinga/Data/plm_precition.rda")

# Calculate total cost of inspection policy (Couldn't get it to work)
calculateCost <- function(tau,t_end, RULhat, RULtrue, batt) {
  # INPUT
  #   Cr:     replacement cost, integer
  #   Cm:     maintenance cost, integer
  #   Cp:     penalty cost, integer
  #   tau:    inspection period, integer
  #   t_end:  final inspection time, integer
  #   RULhat: vector containing predicted RUL and cycle, based on linear model
  #   RULtrue:vector containing true RUL and cycle
  #   batt:   battery being tested
  
  # OUTPUT
  #   cost:   total cost of inspection policy after t_end cycles, integer
  
  cost = c(0,0,0)
  cycle = tau
  batt2 = batt
  for (i in seq(tau,t_end,tau)){ # calculate cost at each tau
    r = sample.int(79,1)
    Cr_new =  case_when( # define new cost
      subset(RULhat, Battnum == batt)[cycle,2] < tau ~ 1,
      subset(RULhat, Battnum == batt)[cycle,2] > tau & subset(RULtrue, Battnum == batt)[cycle,2] < tau  ~ 1,
      subset(RULhat, Battnum == batt)[cycle,2] > tau & subset(RULtrue, Battnum == batt)[cycle,2] > tau  ~ 0,
      TRUE ~ 0)
    Cp_new =  case_when( # define new cost
      subset(RULhat, Battnum == batt)[cycle,2] < tau ~ 0,
      subset(RULhat, Battnum == batt)[cycle,2] > tau & subset(RULtrue, Battnum == batt)[cycle,2] < tau  ~ 1,
      subset(RULhat, Battnum == batt)[cycle,2] > tau & subset(RULtrue, Battnum == batt)[cycle,2] > tau  ~ 0,
      TRUE ~ 0)
    Cm_new =  case_when( # define new cost
      subset(RULhat, Battnum == batt)[cycle,2] < tau ~ 1,
      subset(RULhat, Battnum == batt)[cycle,2] > tau & subset(RULtrue, Battnum == batt)[cycle,2] < tau  ~ 1,
      subset(RULhat, Battnum == batt)[cycle,2] > tau & subset(RULtrue, Battnum == batt)[cycle,2] > tau  ~ 1,
      TRUE ~ 0)
    cost[1] = cost[1] + Cr_new # add new cost
    cost[2] = cost[2] + Cp_new
    cost[3] = cost[3] + Cm_new
    batt2 = case_when( #Check if battery is replaced
      subset(RULhat, Battnum == batt)[cycle,2] < tau ~ r,
      subset(RULhat, Battnum == batt)[cycle,2] > tau & subset(RULtrue, Battnum == batt)[cycle,2] < tau  ~ r,
      TRUE ~ 0)
    cycle = case_when( #Determine cycle based on whether battery is replaced or not
      subset(RULhat, Battnum == batt)[cycle,2] < tau ~ tau,
      subset(RULhat, Battnum == batt)[cycle,2] > tau & subset(RULtrue, Battnum == batt)[cycle,2] < tau  ~ tau,
      subset(RULhat, Battnum == batt)[cycle,2] > tau & subset(RULtrue, Battnum == batt)[cycle,2] > tau  ~ cycle + tau,
      TRUE ~ 0)
    batt = batt2
  }
  return(cost)
}

#Calculate total cost of inspection policy (works)
#I changed the cr, cp and cm to 1 since the total cost is linear in these
#values. As such, we can generalize it for all different values by setting
#them to 1 in the cost function.
calculateCost <- function(tau,t_end, RULhat, RULtrue, batt) {
  cost = c(0,0,0)
  cycle = tau
  for (i in seq(tau,t_end,tau)){ # calculate cost at each tau
    r = sample.int(79,1)
    if (subset(RULhat, Battnum == batt)[cycle,2] < tau){
      Cr_new = 1
      Cp_new = 0
      Cm_new = 1
      batt = r
      cycle = tau
    }
    else if (subset(RULhat, Battnum == batt)[cycle,2] > tau & subset(RULtrue, Battnum == batt)[cycle,2] < tau){
      Cr_new = 1
      Cp_new = 1
      Cm_new = 1
      batt = r
      cycle = tau
    }
    else if (subset(RULhat, Battnum == batt)[cycle,2] > tau & subset(RULtrue, Battnum == batt)[cycle,2] > tau){
      Cr_new = 0
      Cp_new = 0
      Cm_new = 1
      cycle = cycle + tau
    }
    cost[1] = cost[1] + Cr_new # add new cost
    cost[2] = cost[2] + Cp_new
    cost[3] = cost[3] + Cm_new
  }
  return(cost)
}


# Cost variables
tau = 200     # inspection period
t_end = 2000  # end of inspection

# Add predictions
{
  traindat = read.csv("/Users/tyrenkoning/Documents/GitHub/modellinga/Data/Traindatlong.csv")
  RUL = traindat[,c(1,2,4)]
  RUL$lm1 = predict(lm1, newdata = traindat)
  RUL$lm1sqrt = (predict(lm1sqrt, new = traindat))^2
  RUL$lm1third = (predict(lm1third, new = traindat))^3
  RUL$lm2 = predict(lm2, new = traindat)
  RUL$lm2sqrt = (predict(lm2sqrt, new = traindat))^2
  RUL$lm2third = (predict(lm2third, new = traindat))^3
  RUL$lm3 = predict(lm3, new = traindat)
  RUL$lm3sqrt = (predict(lm3sqrt, new = traindat))^2
  RUL$lm3third = (predict(lm3third, new = traindat))^3
  RUL$lm4 = predict(lm4, new = traindat)
  RUL$lm4sqrt = (predict(lm4sqrt, new = traindat))^2
  RUL$lm4third = (predict(lm4third, new = traindat))^3
  RUL$lm5 = predict(lm5, new = traindat)
  RUL$lm5sqrt = (predict(lm5sqrt, new = traindat))^2
  RUL$lm5third = (predict(lm5third, new = traindat))^3
  RUL$plm = plm_prediction$prediction
}
RUL[(RUL < 0)|is.na(RUL)] = 0 # set negative values to 0

#Creating a dataframe and repeating the 2000 cycles 1000 times for each
#regression to get a robust idea of how good each model is
cost = data.frame(matrix(nrow = 1000,
                  ncol = 48))
colnames(cost) = c("Cr_lm1", "Cr_lm1sqrt", "Cr_lm1third", "Cr_lm2", 
                   "Cr_lm2sqrt", "Cr_lm2third", "Cr_lm3", "Cr_lm3sqrt", 
                   "Cr_lm3third", "Cr_lm4", "Cr_lm4sqrt", "Cr_lm4third", 
                   "Cr_lm5", "Cr_lm5sqrt", "Cr_lm5third", "Cr_plm", 
                   "Cp_lm1", "Cp_lm1sqrt", "Cp_lm1third", "Cp_lm2", 
                   "Cp_lm2sqrt", "Cp_lm2third", "Cp_lm3", "Cp_lm3sqrt", 
                   "Cp_lm3third", "Cp_lm4", "Cp_lm4sqrt", "Cp_lm4third", 
                   "Cp_lm5", "Cp_lm5sqrt", "Cp_lm5third", "Cp_plm", 
                   "Cm_lm1", "Cm_lm1sqrt", "Cm_lm1third", "Cm_lm2", 
                   "Cm_lm2sqrt", "Cm_lm2third", "Cm_lm3", "Cm_lm3sqrt", 
                   "Cm_lm3third", "Cm_lm4", "Cm_lm4sqrt", "Cm_lm4third", 
                   "Cm_lm5", "Cm_lm5sqrt", "Cm_lm5third", "Cm_plm")

for (j in seq(1, 16)){
  for (i in seq(1, 1000)) { # Calculate for lm1 1000 times
    s = sample.int(79,1)
    res = calculateCost(tau,t_end, RUL[,c(2,j+3)],RUL[,c(2,3)],s)
    cost[i,j] = res[1]
    cost[i,j+16] = res[2]
    cost[i,j+32] = res[3]
  }
}

costs_mean = colMeans(cost)


































