# Load train and test data and linear models
load("./Data/testdata_long.Rda")
load("./Data/traindata_long.Rda")
load("./Data/linearmodels.RData")

# Add prediction
traindata_long$RULhat = predict(lm4, newdata = traindata_long)
traindata_long$RULhat[(traindata_long$RULhat < 0)|is.na(traindata_long$RULhat)] = 
  0 # set negative values to 0

# Cost variables
Cr = 3        # replacement cost
Cm = 0.5      # maintenance cost
Cp = 1        # penalty cost
tau = 200     # inspection period
t_end = 2000  # end of inspection

RULhat <- traindata_long$RULhat
RULtrue <- traindata_long$RUL

# Calculate total cost of inspection policy
calculateCost <- function(Cr,Cm,Cp,tau,t_end, RULhat, RULtrue) {
# INPUT
#   Cr:     replacement cost, integer
#   Cm:     maintenance cost, integer
#   Cp:     penalty cost, integer
#   tau:    inspection period, integer
#   t_end:  final inspection time, integer
#   RULhat: vector containing predicted RUL, based on linear model
#   RULtrue:vector containing true RUL
  
# OUTPUT
#   cost:   total cost of inspection policy after t_end cycles, integer
  
  cost = 0
  for (i in seq(tau,t_end,tau)){
    newcost =  case_when(
      RULhat[i] < tau ~ Cr + Cm,
      RULhat[i] > tau & RULtrue[i] < tau  ~ Cr + Cp + Cm,
      RULhat[i] > tau & RULtrue[i] > tau  ~ Cm,
      TRUE ~ 0)
    cost = cost + newcost
  }
  return(cost)
}

calculateCost(Cr,Cm,Cp,tau,t_end, RULhat, RULtrue)
