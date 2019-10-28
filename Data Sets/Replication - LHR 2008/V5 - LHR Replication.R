setwd("C:/Users/gwill_000/Dropbox/Methods Notes/Models - Duration/Cure Models Paper")
library(foreign)
library(plyr)
library(eha)
library(lmtest)
library(pscl)
library(coefplot)
library(VGAM)#zipoisson function

###Load data, rename surv variables
lhr<-read.dta("./replication - lhr/lhrIOOct08replication.dta")
lhr<-rename(lhr,c("_t0"="start"))
lhr<-rename(lhr,c("_t"="stop"))
lhr<-rename(lhr,c("_d"="event"))

###Create Survival Object
lhr.surv<-Surv(lhr$start, lhr$stop, lhr$event)

###Create binary dataset
lhr.bin<-toBinary(lhr, surv=c("start", "stop", "event"))

###subset to relevant variables
lhr.keep<-c("event","capchange","battletide","thirdpartycfire","index","onedem5","tie","lndeaths","cfhist","stakes","contiguity","LHRcluster", "riskset")
lhr.fin<-lhr.bin[lhr.keep]


#lhr.bin<-na.omit(lhr.bin)
###Create binary dataset with risksets - superfluous
#lhr.risk<-risksets(lhr.surv)
#lhr.bin$riskset<-lhr.risk$riskset

###Cox model on original data
lhr.cox<-coxreg(lhr.surv~capchange+ battletide + thirdpartycfire + index + onedem5 + tie +lndeaths +cfhist +stakes+ contiguity, data=lhr, method=c("breslow"))
summary(lhr.cox)
#x<-basehaz(lhr.cox, survsim2)
#survfit

###Poisson model on binary data
lhr.pois<-glm(event~capchange+ battletide + thirdpartycfire + index + onedem5 + tie +lndeaths +cfhist +stakes+ contiguity+as.factor(riskset), data=lhr.bin,family="poisson")
summary(lhr.pois)

###Zip model on binary data
lhr.zip<-zeroinfl(event~capchange+ battletide + thirdpartycfire + index + onedem5 + tie +lndeaths +cfhist +stakes+ contiguity+as.factor(riskset)|capchange+ battletide + thirdpartycfire + index + onedem5 + tie +lndeaths +cfhist +stakes+ contiguity, data=lhr.bin)

###create coefficient plot
coefplot(lhr.pois)
coefplot(lhr.zip, col.pts="red",  intercept=TRUE)
#, add=TRUE,

##########################################
###Poisson with glmbsmboot
lhr.pois.eha<-glmmboot(event~capchange+ battletide + thirdpartycfire + index + onedem5 + tie +lndeaths +cfhist +stakes+ contiguity+as.factor(riskset), cluster=riskset, data=lhr.fin,family="poisson")
summary(lhr.pois.eha)
predict(lhr.pois.eha)

###Cox model with mean values calculated at zero
lhr.cox<-coxph(lhr.surv~archigosFIRC+archigosFIRClnt+ capchange+ battletide +thirdpartycfire +index +onedem5+ twodem5+ twodem5lnt+ tie +lndeaths +cfhist +stakes+ contiguity+ contiguitylnt, data=lhr, method=c("breslow"))
sim1<-coxsimLinear(lhr.cox, b="capchange", qi = "Hazard Ratio", ci=.95, Xj=xj)

attach(lhr.bin)
xj=c(mean(archigosFIRC), mean(archigosFIRClnt), mean(capchange), mean(battletide), mean(thirdpartycfire),mean(index), mean(onedem5), mean(twodem5), mean(twodem5lnt), mean(tie), mean(lndeaths), mean(cfhist), mean(stakes), mean(contiguity), mean(contiguitylnt))

lhr.cox.nocent<-coxreg(lhr.surv~archigosFIRC+archigosFIRClnt+ capchange+ battletide +thirdpartycfire +index +onedem5+ twodem5+ twodem5lnt+ tie +lndeaths +cfhist +stakes+ contiguity+ contiguitylnt, data=lhr, method=c("breslow"), center=F)
summary(lhr.cox.nocent)
lhr.cox.nocent$hazards

pois.base<-exp(lhr.pois.eha$frail)
plot(1:46, pois.base, type="s")#baseline hazard
plot(1:46, 1-pois.base, type="s")#baseline surv

###fit statistics
#AIC(lhr.cox.nocent)
#AIC(lhr.pois.eha)
lhr.pois.eha$aic
AIC(lhr.cox.nocent)

###Compare hazard estimates - didn't work
#lhr.cox$hazards #fitted values in Cox
#demean variables used in poisson model
#rerun poisson model
#exp(pois$frailties)
#lhr.cox$means #means used to fit values in Cox
#exp(lhr.pois.eha$frail) #frailties from pois
#lhr.bin2<-lhr.bin-lhr.cox$means#doesn't work

###Compare lrtest - none worked
#-2*log(lhr.cox.nocent$loglik[1]/lhr.cox.nocent$loglik[2])
#-2*log(lhr.pois.eha$logLik/-267.2105)

#lhr.cox.null = coxreg(lhr.surv~1)
#lhr.pois.null<-glmmboot(event~1, family=poisson, data=lhr.bin)

#drop1(lhr.cox.nocent)
#drop1(lhr.pois.eha)

#drop1(lhr.cox, test="Chisq", "na.omit")

a<-as.data.frame(lhr.pois.eha$coefficients)
a$names<-row.names(a)
for each i in 1:n{
  n<-nrow(a.names)
  mean[i]<-mean(lhr.bin$[i]
}


######
attach(lhr.bin)
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
survpred<-survfit(lhr.cox,survsim2)
View(survpred$surv)
library(lattice)
plot(survpred$surv~survsim2$capchange)
x<-survfit(lhr.cox)

survsim22<-cbind(seq(0,5,.1), mean(archigosFIRC),mean(archigosFIRClnt))

survsim2<-lhr.bin
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

survsim3$pred<-predict(lhr.pois, newdata=survsim3, type="response", se.fit=T)
survsim3$up<-survsim3$pred$fit+survsim3$pred$se.fit*1.96
survsim3$lo<-survsim3$pred$se.fit*-1.96
plot(exp(survsim3$pred$fit)~survsim3$capchange, type='l')
abline(survsim3$up~capchange)
abline(survsim3$lo~capchange)


sfnew<-survfit(lhr.cox, newdata=survsim3, type="l")
sfnew$surv


###ZIP
zip<-zeroinfl(event~archigosFIRC+archigosFIRClnt+ capchange+ battletide +thirdpartycfire +index +onedem5+ twodem5+ twodem5lnt+ tie +lndeaths +cfhist +stakes+ contiguity+ contiguitylnt+as.factor(riskset)|archigosFIRC+ capchange+ battletide +thirdpartycfire +index +onedem5+ twodem5+ tie +lndeaths +cfhist +stakes+ contiguity, data=lhr.bin, method=c("CG"))
summary(zip)

zip2<-zipoisson(event~archigosFIRC+archigosFIRClnt+ capchange+ battletide +thirdpartycfire +index +onedem5+ twodem5+ twodem5lnt+ tie +lndeaths +cfhist +stakes+ contiguity+ contiguitylnt+as.factor(riskset)|archigosFIRC+ capchange+ battletide +thirdpartycfire +index +onedem5+ twodem5+ tie +lndeaths +cfhist +stakes+ contiguity)
