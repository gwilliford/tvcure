###Setup
setwd("C:/Users/gwill_000/Dropbox/Methods Notes/Survival Analysis/Cure Models Paper/Replication - LHR")
library(readstata13)
library(plyr)
library(pscl)
library(eha)
library(smcure)
library(coefplot)
library(xtable)
library(stargazer)

###Load data, rename surv variables, create survival object
lhr<-read.dta13("lhrIOOct08replication.dta")
lhr<-rename(lhr,c("_t0"="start"))
lhr<-rename(lhr,c("_t"="stop"))
lhr<-rename(lhr,c("_d"="event"))
lhr2<-lhr[,c("wernerFIRC", "wernerFIRClnt", "capchange","battletide", "thirdpartycfire", "index", "onedem5", "twodem5", "tie", "lndeaths", "cfhist","cfhistlnt", "stakes", "contiguity", "LHRcluster", "start", "stop", "event", "date1")]
lhr3<-na.exclude(lhr2)
lhr4<-toBinary(lhr3,c("start","stop","event"))
=#lhr.surv<-Surv(lhr3$date1, lhr3$newwar)

###Regular cox model
lhr.cox<-coxreg(Surv(lhr3$start, lhr3$stop, lhr3$event)~wernerFIRC + capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, data=lhr3, method=c("breslow"))
lhr.cox
lhrph<-cox.zph(lhr.cox, "identity")
lhrph
plot(lhrph[1])

###Split-population model
zip<-zeroinfl(event~wernerFIRC+wernerFIRClnt+capchange+battletide+thirdpartycfire+index+onedem5+twodem5+tie+lndeaths+cfhist+cfhistlnt+stakes+contiguity+riskset| wernerFIRC+battletide+capchange+index+thirdpartycfire+twodem5+tie+lndeaths+contiguity, data=lhr4)
coefs<-as.vector(zip$coefficients$zero)
names(coefs)[2] = "se" 
coefs$vars = rownames(coefs)
zip$coefficients
coefplot(zip[,zero])
ggplot(coefs)
aes(vars, Estimate)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se), 
                lwd=1, colour="red", width=0) +
  geom_errorbar(aes(ymin=Estimate - se, ymax=Estimate + se), 
                lwd=2.5, colour="blue", width=0) +
  geom_point(size=4, pch=21, fill="yellow") +
  theme_bw()






haz<-zip$coefficients$count
hrs<-exp(zip$coefficients$count)


exp(haz["wernerFIRC"])
exp(log(3650)*haz["wernerFIRClnt"]+haz["wernerFIRC"])
