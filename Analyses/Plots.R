install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)

#setwd("/Users/tyrenkoning/Desktop/University/Modelling-2A")

traindat = read.csv("./Data/Battery_train.csv")
#traindat = read.csv("/Users/tyrenkoning/Documents/GitHub/modellinga/Battery_train.csv")
traindat_new = gather(traindat, variable, value, -Cycle)

#Figure 1
#pdf("Plot1.pdf")
ggplot(data = traindat_new, aes(x = Cycle, y = value, group = variable)) +
  xlab("t (cycles)") +
  ylab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  #ggtitle("Figure 1: Battery capacity degradation data (training") +
  geom_line() +
  geom_hline(yintercept=0.88, 
             linetype="dashed", 
             color = "red", 
             size=0.5) +
  annotate("text", x=1900, y=0.87, label= "Threshold: 0.88", colour = "red", size = 3) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank())
#dev.off()

#Figure 2
for (i in 1:(ncol(traindat)-1)){ # loop over each battery
  c = c(seq(sum(!is.na(traindat[i+1])), sum(!is.na(traindat[i+1]))-(nrow(traindat)-1), -1))
  c[c<0] = 0
  traindat[ , ncol(traindat) + 1] <- c              # Append new column
  colnames(traindat)[ncol(traindat)] <- paste0("RUL", i)
}

write.csv(traindat, "/Users/tyrenkoning/Desktop/University/Modelling-2A/Traindat2.csv", row.names=FALSE)
write.csv(traindat, "./Data/Traindat2.csv", row.names=FALSE)
#In between here is where Python data reformatting takes place
traindatlong = read.csv("/Users/tyrenkoning/Desktop/University/Modelling-2A/Traindatlong.csv")

#pdf("Plot2.pdf")
ggplot(data = traindatlong, aes(x = Capacity, y = RUL, group = Battnum)) +
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
#dev.off()

#Figure 3
testdat = read.csv("/Users/tyrenkoning/Desktop/University/Modelling-2A/Battery_test.csv")
testdat_new = gather(testdat, Battery, Capacity, -Cycle)

#pdf("Plot3.pdf")
ggplot(data = testdat_new, aes(x = Cycle, y = Capacity, group = Battery)) +
  xlab("t (cycles)") +
  ylab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  #ggtitle("Figure 1: Battery capacity degradation data (training") +
  geom_line() +
  geom_hline(yintercept=0.88, 
             linetype="dashed", 
             color = "red", 
             size=0.5) +
  annotate("text", x=1600, y=0.87, label= "Threshold: 0.88", colour = "red", size = 3) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank())
#dev.off()