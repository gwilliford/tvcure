setwd("C:/Users/gwill_000/Dropbox/Research/2 - Discrete Time Cure Models/Potential Replication Papers/BennettStam1996Data")
library(survival)
library(eha)
library(plm)
library(plyr)
library(reshape2)


###load data and cleanup
benstam<-read.csv("C:/Users/gwill_000/Dropbox/Research/2 - Discrete Time Cure Models/Potential Replication Papers/BennettStam1996Data/Stata_Version/one_per_year.csv")

###rescale data for correct replication results according to Bennett and Stam's replication do file
benstam$summper<-(benstam$summper/1000)
benstam$sumpop<-(benstam$sumpop/1000000)
benstam$reprsum<-(benstam$reprsum*-1)

###modify censoring indicator according to Bennett and Stam's replication do file
benstam$censor<-ifelse(benstam$lastyear==benstam$year, 1, 0)

###gen obs indicator
benstam$obs<-1:169
#benstam$obslag<-benstam$obs-1

#get start value for row n-
#benstam$lagtime<-NULL
#benstam$lagtime<-benstam[,benstam$time]
x<-function(x){pmax(benstam$time)}
e<-by(benstam$warnum, x)
benstam$timemaxlist<-(ddply(benstam, .(warnum),x))
timemaxlist2<-as.list(timemax[1,])
warnumlist<-unique(benstam$warnum)
timemaxmat<-cbind(warnumlist, timemaxlist)

?me

benstam.melt<-melt(benstam,id="warnum")
#format as panel data
benstam.p<-plm.data(benstam, c("warnum", "year"))
aggregate(benstam, by="warnum", max(benstam$time))

x<-function(x){lag(benstam.p$censor,-1)}
benstam.p$lead_censor<-by(benstam.p, benstam$warnum, x)
if previous censor (stop) = 1, then zero
if previous censor (stop) = 0, then previous stop value+1

l.censor==1, 0
l.censor==0, l.stop+1
ifelse()

benstam$start<-ifelse(benstam$censor==1, benstam$, )

{
while benstam$censor==1, <-
  benstam$time[benstam$obs==(benstam$obs-1)]
}

benstam$start<-NULL
ifelse(benstam$censor==1, benstam$start<-)

take the value of the previous observation
benstam$obs.lag<-benstam$obs-1


benstam.ts<-(ts(benstam, benstam$obs))
benstam.ts$start<-lag(benstam$time, 1)
benstam.ts2<-as.data.frame(benstam.ts)

###create start indicator
#benstam$obs<-1:169 #obs indicator
#benstam2<-ts(benstam, time)
#benstam$sameobs<-as.numeric(benstam$year<benstam$lastyear)
#benstam$samebenstam$warnum[,benstam$obs-0] 
#0 if previous is not the same
#previous+1 if same
#create indicator for not last

library(plm)
benstam.p<-plm.data(benstam, c("warnum", "year"))

benstam.p$start<-ifelse(lag(benstam$censor==1), 0, lag(benstam$time)+1)

