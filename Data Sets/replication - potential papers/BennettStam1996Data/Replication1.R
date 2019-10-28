###set up
setwd("C:/Users/gwill_000/Dropbox/Research/Discrete Time Cure Models/Potential Replication Papers/BennettStam1996Data/Stata_Version/one_per_year.csv)

library(survival)
library(eha)

###Load original replication data
benstam<-read.csv("C:/Users/gwill_000/Dropbox/Research/2 - Discrete Time Cure Models/Potential Replication Papers/BennettStam1996Data/Stata_Version/one_per_year.csv")

###rescale data for correct replication results according to Bennett and Stam's replication do file
benstam$summper<-(benstam$summper/1000)
benstam$sumpop<-(benstam$sumpop/1000000)
benstam$reprsum<-(benstam$reprsum*-1)


###modify censoring indicator according to Bennett and Stam's replication do file
benstam$end<-rep(0)
benstam$end<-replace(benstam$end, benstam$lastyear==benstam$year, 1)

###format data as survival data
benstam.surv<-Surv(time=benstam$time, event=benstam$end)

###replicate Model 1 - this model won't work - survreg won't accept TVCs
benstam.mod1.aft<-survreg(benstam.surv~popratio+year+nactors, data=benstam, dist="weibull")
benstam.mod1.ph<-(-benstam.mod1.aft$coefficients)/(benstam.mod1.aft$scale)
benstam.mod1.hr<-exp(benstam.mod1.ph)

benstam.mod1.ph
benstam.mod1.hr

bensam.mod1a<-phreg(benstam.surv~popratio+year+nactors+cluster(warnum), data=benstam, dist='weibull')
summary(bensam.mod1a)


benstam.Cox<-coxph(benstam.surv~benstam$nactors, method="breslow")
summary(benstam.Cox)

###format as discrete data
benstam$enter<-rep(0)
benstam.int<-toBinary(benstam, surv=c("enter", "time", "end"))
benstam.aft<-aftreg(benstam.new~popratio+year+nactors, data=benstam, dist='weibull')
benstam.ph.new<-phreg(benstam.new~popratio+year+nactors, data=benstam, dist='weibull')
summary(benstam.ph.new)

#
#create end indicator
#benstam$stop<-benstam$year+1
#benstam$stop<-replace(benstam$stop, benstam$lastyear==benstam$year, 0)
#benstam$stop<-replace(benstam$stop, benstam$stop==0, benstam$year)
#benstam$stop[benstam$year==benstam$lastyear]<-benstam$lastyear[benstam$year==benstam$lastyear]
#benstam2<-benstam[-benstam$stop==0,]
#benstam.surv2<-Surv(time=benstam2$year, time2=benstam2$stop, event=benstam2$end)
#benstam.surv<-Surv(time=benstam$time, event=benstam)
#benstam.surv<-Surv(time=benstam$time, event=benstam$censor)
#benstam.surv<-Surv(time=benstam$time, time2=benstam$end2, event=benstam$censor)


#weibull AFT
benstam.mod1.weib<-survreg(benstam.surv~popratio+year+nactors, data=benstam, dist="weibull")
summary(benstam.mod1.weib)
#weibull HRs
benstam.mod1.weib.PH<-(-benstam.mod1.weib$coefficients)/(benstam.mod1.weib$scale)
benstam.mod1.weib.PH

#above don't work, need TVCs
install.packages("flexsurv")

benstamflex<-flexsurvreg(benstam.new~popratio+year+nactors, data=benstam, dist='weibull)
summary(benstamflex$coefficients)

benstam.new<-Surv(benstam$enter, benstam$time, benstam$end)
summary(benstam.new)
benstam$time-benstam$enter
benstam.aft.new<-aftreg(benstam.new~popratio+year+nactors, data=benstam, dist='weibull')
(benstam.aft.ph<-(-benstam.aft.new$coefficients/benstam.aft.new$scale))
join.spells(benstam, strict = FALSE, eps = 1.e-8)
benstam.new.ph<-phreg(benstam.new~popratio+year+nactors, data=benstam, dist='weibull')
summary(benstam.new.ph)



check<-as.matrix(cbind(benstam$warnum,benstam$enter, benstam$time))
View(check)
