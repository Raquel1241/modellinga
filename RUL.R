library(ggplot2)
library(reshape2)

traindata <- read.csv("./Data/Battery_train.csv")

# create capacity array with same size as no. of cycles
capacity <- seq(from = 0.88, to = 1.1, by =1.1/(nrow(traindata)-1))

RULdata <- data.frame(capacity)

for(b in 2:ncol(traindata)){ # loop over batteries
  RUL = c() # initialize value
  for(c in capacity){ # for_loop for each capacity
    lifetime <- which(traindata[,b] < 0.88) # find maximum amount of cycles before breakdown
    time <- which.min(abs(traindata[,b] - c)) # find time index closest to capacity
    RULval <- lifetime - time # RUL = total lifetime - t
    if (RULval <= 0){RULval = NA} # ensure RUL is greater than zero
    RUL <- append(RUL, RULval) # add to RUL vector
  }
  RULdata[ , ncol(RULdata) + 1] <- RUL   # Append new column
  colnames(RULdata)[ncol(RULdata)] <- paste0("NO.", b-1) # change column name
}
  

data.long <- melt(RULdata, id = "capacity")
ggplot(data.long, aes(capacity, value, group = variable)) +
  xlab("t (cycles)") +
  ylab(bquote("Capacity" ~ C["t,i"] ~ "(Ah)")) +
  ggtitle("Battery capacity degradation data (training)") +
  geom_line() + 
  geom_hline(yintercept=0.88, 
             linetype="dashed", 
             color = "red", 
             linewidth=0.5) +
  annotate("text", x=1900, y=0.87, label= "Threshold: 0.88", colour = "red", size = 3)



