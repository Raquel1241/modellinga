#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("ggplot2")
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)


traindat=read.csv("C:/Users/sofia/OneDrive/Random/GitHub/modellinga/Data/Battery_train.csv")
#traindat = read.csv("./Data/Battery_train.csv")
#traindat = read.csv("/Users/tyrenkoning/Documents/GitHub/modellinga/Battery_train.csv")
traindat_new = gather(traindat, variable, value, -Cycle)


#Modelling Battery 1

secord=(traindat$Cycle)**2
thirord=(traindat$Cycle)**3
forord=(traindat$Cycle)**4

lmres=lm(traindat$NO.1~traindat$Cycle+secord+thirord+forord, data=traindat)
y=coefficients(lmres)[2]*traindat$Cycle+coef(lmres)[1]+coef(lmres)[3]*secord+coef(lmres)[4]*thirord+coef(lmres)[5]*forord
plotters=data_frame(y, traindat$NO.1, traindat$Cycle)

ggplot(data = plotters, aes(x = traindat$Cycle, y = traindat$NO.1)) +
  xlab("t (cycles)") +
  ylab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  ggtitle("Figure 1: Battery 1 capacity degradation data (training)") +
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


ggplot(data = plotters, aes(x = traindat$Cycle, y = y)) +
  xlab("t (cycles)") +
  ylab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  ggtitle("Figure 2: Model of Battery 1 capacity degradation data (training)") +
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



#Modelling Battery 2

N2=traindat$NO.2
N2_new=na.omit(N2)
C2=traindat$Cycle
C2_new=C2[1:length(N2_new)]
traindat_N2=data_frame(C2_new,N2_new)

secord=(C2_new)**2
thirord=(C2_new)**3
forord=(C2_new)**4

lmres=lm(N2_new~C2_new+secord+thirord+forord, data=traindat_N2)
y=coef(lmres)[1]+coef(lmres)[2]*C2_new+coef(lmres)[3]*secord+coef(lmres)[4]*thirord+coef(lmres)[5]*forord
plotters2=data_frame(y, N2_new, C2_new)

ggplot(data = plotters2, aes(x = C2_new, y = y)) +
  xlab("t (cycles)") +
  ylab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  ggtitle("Figure 2: Model of Battery 2 capacity degradation data (training)") +
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

ggplot(data = plotters, aes(x = traindat$Cycle, y = traindat$NO.2)) +
  xlab("t (cycles)") +
  ylab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  ggtitle("Figure 1: Battery 2 capacity degradation data (training)") +
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


#Modelling All Batteries (first try)

secord=(traindat_new$Cycle)**2
thirord=(traindat_new$Cycle)**3
forord=(traindat_new$Cycle)**4

lmres=lm(traindat_new$value~traindat_new$Cycle+secord+thirord+forord, data=traindat_new)
y=coefficients(lmres)[2]*traindat_new$Cycle+coef(lmres)[1]+coef(lmres)[3]*secord+coef(lmres)[4]*thirord+coef(lmres)[5]*forord
plotters=data_frame(y, traindat_new$value, traindat_new$Cycle)

ggplot(data = plotters, aes(x = traindat_new$Cycle, y = y)) +
  xlab("t (cycles)") +
  ylab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  ggtitle("Figure 3: Model of battery capacity degradation data (training)") +
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


#Modelling All Batteries (second try)

bigvector= c()
bigveccyc= c()
for (i in 2:length(traindat)){
  toadd=na.omit(traindat[1:length(traindat$Cycle),i])
  bigvector=append(bigvector, toadd)
  bigveccyc=append(bigveccyc, traindat$Cycle[1:length(toadd)])
}

bigtraindata=data_frame(bigveccyc,bigvector)

firord=(bigtraindata$bigveccyc)
secord=(bigtraindata$bigveccyc)**2
thirord=(bigtraindata$bigveccyc)**3
forord=(bigtraindata$bigveccyc)**4

lmres=lm(bigtraindata$bigvector~firord+secord+thirord+forord, data=bigtraindata)
y=coef(lmres)[1]+coef(lmres)[2]*firord+coef(lmres)[3]*secord+coef(lmres)[4]*thirord+coef(lmres)[5]*forord
plotters=data_frame(y, bigtraindata$bigvector, bigtraindata$bigveccyc)

ggplot(data = plotters, aes(x = bigtraindata$bigveccyc, y = y)) +
  xlab("t (cycles)") +
  ylab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  ggtitle("Figure 4: Second Model of battery capacity degradation data (training)") +
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









