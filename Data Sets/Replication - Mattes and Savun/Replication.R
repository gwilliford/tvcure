setwd("C:/Users/gwill_000/Dropbox/Methods Notes/Survival Analysis/Cure Models Paper/Replication - Mattes and Savun")
library(readstata13)
library(survival)

#Load original data
msload<-read.dta13("ISQrepdata.dta")
write.csv(msload,"ISQrepdata.csv")

colnames(msload)[30]<-"censor"
summary(msload$censor) #mean is .3913 - 60% of obs are censored
#Note on data - Peacedur = duration of peace, duration = duration or previous conflict (used as a control)
colnames(msload)[18]<-"prevdur"
tabulate()
colnames(msload)[32]<-"start"
colnames(msload)[31]<-"stop"

#format as surv data
mssurv<-Surv(msload$peacedur, msload$peacefail) 

#Kaplan-Meier function
km<-survfit(mssurv~1)
plot(km,mark.time=T,lwd=c(2,1,1))

#Basic cox model - replication
mscoxrep<-coxph(mssurv~polpssum+milpssum+econpssum+terrpssum+costinc+guarantee+issue+costs+prevdur, method="efron",data=msload, robust=T)
summary(mscoxrep)
#Successfully replicated (exp(coef) = hazard rates) - but, SEs are off
mscoxrep2<-coxph(mssurv~polpssum+milpssum+econpssum+terrpssum+costinc+guarantee+issue+costs+prevdur+cluster(ccode), method="efron",data=msload, robust=T)
summary(mscoxrep2)

mscoxrep3<-coxreg(mssurv~polpssum+milpssum+econpssum+terrpssum+costinc+guarantee+issue+costs+prevdur, method="efron",data=msload, hazards = T )
summary(mscoxrep3)

mscoxrep4<-coxreg(mssurv~polpssum+milpssum+econpssum+terrpssum+costinc+guarantee+issue+costs+prevdur, method="breslow",data=msload, hazards = T )

coeftest(mscoxrep3, vcov=vcovHC(mscoxrep3, "HC1"))
deltaMethod(mscoxrep3, "polpssum", vcov="HC1")
library(rms)
robcov(mscoxrep3, cluster="ccode")
mssurv2<-Surv(msload$start, msload$stop, msload$censor)
msbin<-toBinary(msload, c("start", "stop", "censor"))
hist(msload$event)

library(pscl)
zip1<-zeroinfl(event~polpssum+milpssum+econpssum+terrpssum+costinc+guarantee+issue+costs+prevdur|polpssum+milpssum+econpssum+terrpssum, data=msbin)
summary(zip1)

zip2<-zeroinfl(event~polpssum+milpssum+econpssum+terrpssum+costinc+guarantee+issue+costs+prevdur, data=msbin)

pois1<-glm(event~polpssum+milpssum+econpssum+terrpssum+costinc+guarantee+issue+costs+prevdur, data=msbin, family=poisson))
summary(pois1)
exp(pois1$coefficients)

msfix<-glmmboot(
  event~polpssum+milpssum+econpssum+terrpssum+costinc+guarantee+issue+costs+prevdur,
  data=msbin,
  family=poisson,
  cluster=peacedur
)
mslmer<-glmer(event~polpssum+milpssum+econpssum+terrpssum+costinc+guarantee+issue+costs+prevdur+(1|peacedur),
  data=msbin,
  family=poisson,
)




exp(mscoxrep4$coefficients)
