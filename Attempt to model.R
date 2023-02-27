#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("ggplot2")
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)

<<<<<<< Updated upstream
traindat=read.csv("C:/Users/sofia/OneDrive/Random/GitHub/modellinga/Data/Battery_train.csv")
#traindat = read.csv("./Data/Battery_train.csv")
#traindat = read.csv("/Users/tyrenkoning/Documents/GitHub/modellinga/Battery_train.csv")
traindat_new = gather(traindat, variable, value, -Cycle)

ggplot(data = traindat_new, aes(x = Cycle, y = value, group = variable)) +
  xlab("t (cycles)") +
  ylab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  ggtitle("Figure 1: Battery capacity degradation data (training") +
  geom_line() +
  geom_hline(yintercept=0.88, 
             linetype="dashed", 
             color = "red", 
             linewidth=0.5) +
  annotate("text", x=1900, y=0.87, label= "Threshold: 0.88", colour = "red", size = 3) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "Black"))




=======
>>>>>>> Stashed changes

#Modelling Battery

secord=(traindat$Cycle)**2
thirord=(traindat$Cycle)**3
forord=(traindat$Cycle)**4

lmres=lm(traindat$NO.1~traindat$Cycle+secord+thirord+forord, data=traindat)
y=coefficients(lmres)[2]*traindat$Cycle+coef(lmres)[1]+coef(lmres)[3]*secord+coef(lmres)[4]*thirord+coef(lmres)[5]*forord
plotters=data_frame(y, traindat$NO.1, traindat$Cycle)

ggplot(data = plotters, aes(x = traindat$Cycle, y = traindat$NO.1)) +
  xlab("t (cycles)") +
  ylab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  ggtitle("Figure 1: Battery capacity degradation data (training") +
  geom_line() +
  geom_hline(yintercept=0.88, 
             linetype="dashed", 
             color = "red", 
             linewidth=0.5) +
  annotate("text", x=1900, y=0.87, label= "Threshold: 0.88", colour = "red", size = 3) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "Black"))

