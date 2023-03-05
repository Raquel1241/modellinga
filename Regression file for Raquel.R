traindat = read.csv("/Users/tyrenkoning/Documents/GitHub/modellinga/Data/Traindatlong.csv")
traindat$sqrtRUL = sqrt(traindat$RUL)
traindat$one_thirdRUL = (traindat$RUL)^(1/3)

predict = data.frame(Capacity = seq(0.88, 1.1, 0.001))

#1st ord. 
lm1 = lm(RUL~Capacity, data = traindat)

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
