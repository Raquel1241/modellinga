install.packages("lmtest")

library(car)
library(lmtest)

traindat = read.csv("/Users/tyrenkoning/Documents/GitHub/modellinga/Data/Traindatlong.csv")
traindat$sqrtRUL = sqrt(traindat$RUL)
traindat$one_thirdRUL = (traindat$RUL)^(1/3)

predict = data.frame(Capacity = seq(0.88, 1.1, 0.001))

durbinWatsonTest()

#1st ord. 
lm1 = lm(RUL~Capacity, data = traindat)
durbinWatsonTest(lm1)
gqtest(lm1, order.by = ~Capacity, data = traindat, fraction = 32516)

#1st ord. sqrt
lm1sqrt = lm(sqrtRUL~Capacity, data = traindat)
durbinWatsonTest(lm1sqrt)
gqtest(lm1sqrt, order.by = ~Capacity, data = traindat, fraction = 32516)

#1st ord. 1/3
lm1third = lm(one_thirdRUL~Capacity, data = traindat)
durbinWatsonTest(lm1third)
gqtest(lm1third, order.by = ~Capacity, data = traindat, fraction = 32516)

#2nd ord.
lm2 = lm(RUL~Capacity + I(Capacity^2), data = traindat) 
durbinWatsonTest(lm2)
gqtest(lm2, order.by = ~Capacity, data = traindat, fraction = 32516)

#2nd ord. sqrt
lm2sqrt = lm(sqrtRUL~Capacity + I(Capacity^2), data = traindat) 
durbinWatsonTest(lm2sqrt)
gqtest(lm2sqrt, order.by = ~Capacity, data = traindat, fraction = 32516)

#2nd ord. 1/3
lm2third = lm(one_thirdRUL~Capacity + I(Capacity^2), data = traindat) 
durbinWatsonTest(lm2third)
gqtest(lm2third, order.by = ~Capacity, data = traindat, fraction = 32516)

#3rd ord.
lm3 = lm(RUL~Capacity + I(Capacity^2) + I(Capacity^3), data = traindat) 
durbinWatsonTest(lm3)
gqtest(lm3, order.by = ~Capacity, data = traindat, fraction = 32516)

#3rd ord. sqrt
lm3sqrt = lm(sqrtRUL~Capacity + I(Capacity^2) + I(Capacity^3), data = traindat) 
durbinWatsonTest(lm3sqrt)
gqtest(lm3sqrt, order.by = ~Capacity, data = traindat, fraction = 32516)

#3rd ord. 1/3
lm3third = lm(one_thirdRUL~Capacity + I(Capacity^2) + I(Capacity^3), data = traindat) 
durbinWatsonTest(lm3third)
gqtest(lm3third, order.by = ~Capacity, data = traindat, fraction = 32516)

#4th ord.
lm4 = lm(RUL~Capacity + I(Capacity^2) + I(Capacity^3) + I(Capacity^4), data = traindat) 
durbinWatsonTest(lm4)
gqtest(lm4, order.by = ~Capacity, data = traindat, fraction = 32516)

#4th ord. sqrt
lm4sqrt = lm(sqrtRUL~Capacity + I(Capacity^2) + I(Capacity^3) + I(Capacity^4), data = traindat) 
durbinWatsonTest(lm4sqrt)
gqtest(lm4sqrt, order.by = ~Capacity, data = traindat, fraction = 32516)

#4th ord. 1/3
lm4third = lm(one_thirdRUL~Capacity + I(Capacity^2) + I(Capacity^3) + I(Capacity^4), data = traindat) 
durbinWatsonTest(lm4third)
gqtest(lm4third, order.by = ~Capacity, data = traindat, fraction = 32516)

#5th ord.
lm5 = lm(RUL~Capacity + I(Capacity^2) + I(Capacity^3) + I(Capacity^4) + I(Capacity^5), data = traindat)
durbinWatsonTest(lm5)
gqtest(lm5, order.by = ~Capacity, data = traindat, fraction = 32516)

#5th ord. sqrt
lm5sqrt = lm(sqrtRUL~Capacity + I(Capacity^2) + I(Capacity^3) + I(Capacity^4) + I(Capacity^5), data = traindat)
durbinWatsonTest(lm5sqrt)
gqtest(lm5sqrt, order.by = ~Capacity, data = traindat, fraction = 32516)

#5th ord. 1/3
lm5third = lm(one_thirdRUL~Capacity + I(Capacity^2) + I(Capacity^3) + I(Capacity^4) + I(Capacity^5), data = traindat)
durbinWatsonTest(lm5third)
gqtest(lm5third, order.by = ~Capacity, data = traindat, fraction = 32516)





