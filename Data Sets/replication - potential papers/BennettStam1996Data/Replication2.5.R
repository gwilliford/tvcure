###set up
setwd("C:/Users/gwill_000/Dropbox/Research/Discrete Time Cure Models/Potential Replication Papers/BennettStam1996Data)




###rescale data for correct replication results according to Bennett and Stam's replication do file

head(benstam$summper)
benstam$summper<-(benstam$summper/1000)
head(benstam$summper)

head(benstam$sumpop)
benstam$sumpop<-(benstam$sumpop/1000000)
head(benstam$sumpop)

head(benstam$reprsum)
benstam$reprsum<-(benstam$reprsum*-1)
head(benstam$reprsum)

###modify censoring indicator according to Bennett and Stam's replication do file
benstam$end<-rep(0)
benstam$end<-replace(benstam$end, benstam$lastyear==benstam$year, 1)

###format data as survival data
benstam.surv<-Surv(time=benstam$time, event=benstam$end)

###replicate Model 1
#Cox
benstam.Cox<-coxph(benstam.surv~benstam$nactors, method="breslow")
summary(benstam.Cox)

###format as discrete data
benstam$enter<-rep(0)
benstam.int<-toBinary(benstam, surv=c("enter", "time", "end"))

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

benstam$end2<-ifelse(benstam$year==benstam$lastyear, benstam$lastyear, benstam$year+1)
benstam$end2<-benstam$year
