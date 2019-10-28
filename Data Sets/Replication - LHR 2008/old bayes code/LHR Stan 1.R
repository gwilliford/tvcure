###Setup
rm(list=ls())
setwd("C:/Users/gwill_000/Dropbox/Methods Notes/Survival Analysis/Cure Models Paper/Replication - LHR")
library(readstata13)
library(plyr)
library(eha)
library(rstan)

###Load data, rename surv variables, create survival object, create binary data
lhr<-read.dta13("lhrIOOct08replication.dta")
lhr<-rename(lhr,c("_t0"="start"))
lhr<-rename(lhr,c("_t"="stop"))
lhr<-rename(lhr,c("_d"="event"))
lhr.surv<-Surv(lhr$start, lhr$stop, lhr$event)
lhr2<-lhr[,c("archigosFIRC", "archigosFIRClnt", "capchange", "battletide", "thirdpartycfire", "index", "onedem5", "twodem5", "twodem5lnt", "tie", "lndeaths", "cfhist", "stakes", "contiguity", "contiguitylnt", "LHRcluster", "start", "stop", "event", "id")]
lhr.bin<-na.omit(toBinary(lhr2, surv=c("start", "stop", "event")))

###Run frequentist cox model
lhr.cox<-coxph(lhr.surv~archigosFIRC + archigosFIRClnt + capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + twodem5lnt + tie + lndeaths + cfhist + stakes + contiguity + contiguitylnt+cluster(LHRcluster), data=lhr, ties=c("breslow"))
summary(lhr.cox)

###Format data for use in stan
nrow<-nrow(lhr.bin)
nsub<-unique(lhr$id)
idx <- sort(unique(lhr.bin$riskset))
dummy <- matrix(NA, nrow = nrow, ncol = length(idx))
for (j in 1:length(idx)) {dummy[,j] <- as.integer(datreal$riskset == idx[j])}
X<-with(lhr.bin,as.matrix(cbind(rep(1,nrow),archigosFIRC, archigosFIRClnt,capchange, battletide, thirdpartycfire, index, onedem5, twodem5, twodem5lnt, tie, lndeaths, cfhist, stakes, contiguity, contiguitylnt,dummy)))
Z<-with(lhr.bin,as.matrix(cbind(rep(1,nrow),archigosFIRC, archigosFIRClnt,capchange, battletide, thirdpartycfire, index, onedem5, twodem5, twodem5lnt, tie, lndeaths, cfhist, stakes, contiguity, contiguitylnt)))
ncovx<-ncol(X)
ncovz<-ncol(Z)
datfin<-list(y=lhr.bin$event, X=X, Z=Z, nrow=nrow, nsub=nsub, ncovx=ncovx, ncovz=ncovz, id=lhr.bin$id)

date();lhr.zip<-stan(file='m6mod.stan', data = datfin, iter = 1000, chains = 2,control=list(adapt_delta=.99));date()

###Split-population model - old
#cox.sp.simsurv<-smcure(lhr.surv~archigosFIRC,cureform=~archigosFIRC, data=lhr3)
#+capchange,~stakes, data=lhr3, model="ph", na.action=na.omit, link="logit")
#
#                      + archigosFIRClnt + capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + twodem5lnt + tie + lndeaths + cfhist + stakes + contiguity + contiguitylnt, data=lhr3, model="ph")
#with(lhr,length(archigosFIRC))
#with(lhr,length(capchange))
#with(lhr,length(tie))
#
#lhr.surv<-Surv(lhr3$start, lhr3$stop, lhr3$event,type="counting"
#cureform=~archigosFIRC + archigosFIRClnt + capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + twodem5lnt + tie + lndeaths + cfhist + stakes + contiguity + contiguitylnt, data=lhr, model="ph")
