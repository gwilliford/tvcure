###Setup
setwd("C:/Users/gwill/Dropbox/Methods Notes/Survival Analysis/Cure Models Paper/Replication - LHR 2008")
library(readstata13)
library(plyr)
#library(smcure)

###Load data, rename surv variables, create survival object
lhr<-read.dta13("lhrIOOct08replication.dta")
lhr<-rename(lhr,c("_t0"="start"))
lhr<-rename(lhr,c("_t"="stop"))
lhr<-rename(lhr,c("_d"="event"))
  lhr2<-lhr[,c("archigosFIRC", "archigosFIRClnt", "capchange", "battletide", "thirdpartycfire", "index", "onedem5", "twodem5", "twodem5lnt", "tie", "lndeaths", "cfhist", "stakes", "contiguity", "contiguitylnt", "LHRcluster", "start", "stop", "event", "newwar", "date1")]
  lhr3<-na.exclude(lhr2)
lhr.surv<-Surv(lhr3$start, lhr3$stop, lhr3$event)
lhr.surv<-Surv(lhr3$date1, lhr3$newwar)

###Regular cox model
lhr.cox<-coxph(lhr.surv~archigosFIRC + archigosFIRClnt + capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + twodem5lnt + tie + lndeaths + cfhist + stakes + contiguity + contiguitylnt+cluster(LHRcluster), data=lhr3, ties=c("breslow"))
summary(lhr.cox)

###Split-population model
lhr.tvcure<-tvcure(lhr.surv~capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, cureform=~capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, data=lhr3, model="ph", Var=T, firthlogit=T)
system.time(lhr.tvcure<-tvcure(lhr.surv~thirdpartycfire + index + onedem5 + tie + lndeaths + stakes, cureform=~capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, data=lhr3, model="ph", Var=T, firthlogit=T, nboot=10,emmax=1000))





cox.sp.simsurv<-smcure(lhr.surv~archigosFIRC,cureform=~archigosFIRC, data=lhr3)

+capchange,~stakes, data=lhr3, model="ph", na.action=na.omit, link="logit")

                       + archigosFIRClnt + capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + twodem5lnt + tie + lndeaths + cfhist + stakes + contiguity + contiguitylnt, data=lhr3, model="ph")

with(lhr,length(archigosFIRC))
with(lhr,length(capchange))
with(lhr,length(tie))

lhr.surv<-Surv(lhr3$start, lhr3$stop, lhr3$event,type="counting")

                       cureform=~archigosFIRC + archigosFIRClnt + capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + twodem5lnt + tie + lndeaths + cfhist + stakes + contiguity + contiguitylnt, data=lhr, model="ph")
