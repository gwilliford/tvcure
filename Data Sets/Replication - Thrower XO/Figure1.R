#Load necessary libraries to load csv and make bargrah
library(foreign)
library(ggplot2)
library(gplots)

#set working directory
setwd()

#load dataset and attach variables
data<-read.csv("Figure1.csv", header = T)
attach(data)

#Make bar graph for Figure 1
par(mar=c(4.8, 4.8, 2.1, 2.1))
barplot(as.matrix(data), beside = T, col=c("gray", "dimgray","black"), ylab="Number of Executive Orders", xlab="Years to Change", names.arg=c("Not Changed", "1 to 10", "11 to 20", "21 to 30", "31+"))
legend("topright",c("Superseded","Amended","Revoked"), fill=c("gray","dimgray","black"))
box(bty = "l")