setwd("C:/Users/gwill_000/Dropbox/Methods Notes/Models - Duration/Cure Models Paper/replication - lhr")
library(foreign)
library(plyr)
library(eha)
library(lmtest)
library(pscl)

###Load data, rename surv variables
LHR<-read.dta("LHRIOOct08replication.dta")
LHR<-rename(LHR,c("_t0"="start"))
LHR<-rename(LHR,c("_t"="stop"))
LHR<-rename(LHR,c("_d"="event"))

###Create Survival Object
LHRSurv<-Surv(LHR$start, LHR$stop, LHR$event)

###Create binary dataset
LHR.bin<-toBinary(LHR, surv=c("start", "stop", "event"))
#LHR.bin<-na.omit(LHR.bin)
###Create binary dataset with risksets - superfluous
#LHR.risk<-risksets(LHRSurv)
#LHR.bin$riskset<-LHR.risk$riskset

###Cox model on original data
LHR.cox<-coxreg(LHRSurv~archigosFIRC+archigosFIRClnt+ capchange+ battletide +thirdpartycfire +index +onedem5+ twodem5+ twodem5lnt+ tie +lndeaths +cfhist +stakes+ contiguity+ contiguitylnt, data=LHR, method=c("breslow"))
summary(LHR.cox)
x<-basehaz(LHR.cox, survsim2)
survfit

###Poisson model on binary data
LHR.pois<-glm(event~archigosFIRC+archigosFIRClnt+ capchange+ battletide +thirdpartycfire +index +onedem5+ twodem5+ twodem5lnt+ tie +lndeaths +cfhist +stakes+ contiguity+ contiguitylnt+as.factor(riskset), data=LHR.bin,family="poisson")
summary(LHR.pois)

###Poisson with glmbsmboot
LHR.pois.eha<-glmmboot(event~archigosFIRC+archigosFIRClnt+ capchange+ battletide +thirdpartycfire +index +onedem5+ twodem5+ twodem5lnt+ tie +lndeaths +cfhist +stakes+ contiguity+ contiguitylnt, cluster=riskset, family=poisson, data=LHR.bin)
summary(LHR.pois.eha)

predict(LHR.pois.eha)

###Cox model with mean values calculated at zero
LHR.cox<-coxph(LHRSurv~archigosFIRC+archigosFIRClnt+ capchange+ battletide +thirdpartycfire +index +onedem5+ twodem5+ twodem5lnt+ tie +lndeaths +cfhist +stakes+ contiguity+ contiguitylnt, data=LHR, method=c("breslow"))
sim1<-coxsimLinear(LHR.cox, b="capchange", qi = "Hazard Ratio", ci=.95, Xj=xj)

attach(LHR.bin)
xj=c(mean(archigosFIRC), mean(archigosFIRClnt), mean(capchange), mean(battletide), mean(thirdpartycfire),mean(index), mean(onedem5), mean(twodem5), mean(twodem5lnt), mean(tie), mean(lndeaths), mean(cfhist), mean(stakes), mean(contiguity), mean(contiguitylnt))

LHR.cox.nocent<-coxreg(LHRSurv~archigosFIRC+archigosFIRClnt+ capchange+ battletide +thirdpartycfire +index +onedem5+ twodem5+ twodem5lnt+ tie +lndeaths +cfhist +stakes+ contiguity+ contiguitylnt, data=LHR, method=c("breslow"), center=F)
summary(LHR.cox.nocent)
LHR.cox.nocent$hazards

pois.base<-exp(LHR.pois.eha$frail)
plot(1:46, pois.base, type="s")#baseline hazard
plot(1:46, 1-pois.base, type="s")#baseline surv

###fit statistics
#AIC(LHR.cox.nocent)
#AIC(LHR.pois.eha)
LHR.pois.eha$aic
AIC(LHR.cox.nocent)

###Compare hazard estimates - didn't work
#LHR.cox$hazards #fitted values in Cox
#demean variables used in poisson model
#rerun poisson model
#exp(pois$frailties)
#LHR.cox$means #means used to fit values in Cox
#exp(LHR.pois.eha$frail) #frailties from pois
#LHR.bin2<-LHR.bin-LHR.cox$means#doesn't work

###Compare lrtest - none worked
#-2*log(LHR.cox.nocent$loglik[1]/LHR.cox.nocent$loglik[2])
#-2*log(LHR.pois.eha$logLik/-267.2105)

#LHR.cox.null = coxreg(LHRSurv~1)
#LHR.pois.null<-glmmboot(event~1, family=poisson, data=LHR.bin)

#drop1(LHR.cox.nocent)
#drop1(LHR.pois.eha)

#drop1(LHR.cox, test="Chisq", "na.omit")

a<-as.data.frame(LHR.pois.eha$coefficients)
a$names<-row.names(a)
for each i in 1:n{
  n<-nrow(a.names)
  mean[i]<-mean(LHR.bin$[i]
}


######
attach(LHR.bin)
capchange<-seq(0,5,.1)
survsim2<-as.data.frame(capchange)
survsim2$archigosFIRC<-mean(archigosFIRC)
survsim2$archigosFIRClnt<-mean(archigosFIRClnt)
survsim2$capchange<-mean(capchange)
survsim2$battletide<-mean(battletide)
survsim2$thirdpartycfire<-mean(thirdpartycfire)
survsim2$index<-mean(index)
survsim2$onedem5<-mean(onedem5)
survsim2$twodem5<-mean(twodem5)
survsim2$twodem5lnt<-mean(twodem5lnt)
survsim2$tie<-mean(tie)
survsim2$lndeaths<-mean(lndeaths, na.rm=T)
survsim2$cfhist<-mean(cfhist)
survsim2$stakes<-mean(stakes)
survsim2$contiguity<-mean(contiguity)
survsim2$contiguitylnt<-mean(contiguitylnt)
View(survsim2)
survpred<-survfit(LHR.cox,survsim2)
View(survpred$surv)
library(lattice)
plot(survpred$surv~survsim2$capchange)
x<-survfit(LHR.cox)

survsim22<-cbind(seq(0,5,.1), mean(archigosFIRC),mean(archigosFIRClnt))

survsim2<-LHR.bin
survsim2$archigosFIRC<-mean(archigosFIRC)
survsim2$archigosFIRClnt<-mean(archigosFIRClnt)
survsim2$capchange<-mean(capchange)
survsim2$battletide<-mean(battletide)
survsim2$thirdpartycfire<-mean(thirdpartycfire)
survsim2$index<-mean(index)
survsim2$onedem5<-mean(onedem5)
survsim2$twodem5<-mean(twodem5)
survsim2$twodem5lnt<-mean(twodem5lnt)
survsim2$tie<-mean(tie)
survsim2$lndeaths<-mean(lndeaths, na.rm=T)
survsim2$cfhist<-mean(cfhist)
survsim2$stakes<-mean(stakes)
survsim2$contiguity<-mean(contiguity)
survsim2$contiguitylnt<-mean(contiguitylnt)

survsim3<-survsim2[0:51,]
survsim3$capchange<-seq(0,5,.1)

survsim3$pred<-predict(LHR.pois, newdata=survsim3, type="response", se.fit=T)
survsim3$up<-survsim3$pred$fit+survsim3$pred$se.fit*1.96
survsim3$lo<-survsim3$pred$se.fit*-1.96
plot(exp(survsim3$pred$fit)~survsim3$capchange, type='l')
abline(survsim3$up~capchange)
abline(survsim3$lo~capchange)


sfnew<-survfit(LHR.cox, newdata=survsim3, type="l")
sfnew$surv


###ZIP
zip<-zeroinfl(event~archigosFIRC+archigosFIRClnt+ capchange+ battletide +thirdpartycfire +index +onedem5+ twodem5+ twodem5lnt+ tie +lndeaths +cfhist +stakes+ contiguity+ contiguitylnt+as.factor(riskset)|archigosFIRC+ capchange+ battletide +thirdpartycfire +index +onedem5+ twodem5+ tie +lndeaths +cfhist +stakes+ contiguity, data=LHR.bin, method=c("CG"))
summary(zip)

zip2<-zipoisson(event~archigosFIRC+archigosFIRClnt+ capchange+ battletide +thirdpartycfire +index +onedem5+ twodem5+ twodem5lnt+ tie +lndeaths +cfhist +stakes+ contiguity+ contiguitylnt+as.factor(riskset)|archigosFIRC+ capchange+ battletide +thirdpartycfire +index +onedem5+ twodem5+ tie +lndeaths +cfhist +stakes+ contiguity)
