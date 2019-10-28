#load packages and data
setwd("C:/Users/gwill_000/Dropbox/Research/Discrete Time Cure Models/Potential Replication Papers/BennettStam1996Data")

library(survival)
library(flexsurv)

bens<-read.csv("./Stata_Version/one_per_year.csv")

#rescale data
bens$summper<-(bens$summper/1000)
bens$sumpop<-(bens$sumpop/1000000)
bens$reprsum<-(bens$reprsum*-1)

#create new censoring indicator
bens$end<-rep(0)
bens$end<-replace(bens$end, bens$lastyear==bens$year, 1)

#expand
bens$month<-ifelse(bens$time<=12, 12-bens$time, bens$time-12)



Surv(time= , time2=bens$time, event=bens$end)

flexsurvreg()