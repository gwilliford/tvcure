install.packages("smcure")
install.packages("semicure")
install.packages("em")

library(smcure)
library(survival)
  #estimates nonparametric cure models, cannot incorporate TVCs
library(semicure)
library(em)

)


test.data <- as.data.frame(cbind(start=c(1, 2, 5, 2, 1, 7, 3, 4, 8, 8),
              stop =c(2, 3, 6, 7, 8, 9, 9, 9,14,17),
              event=c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
              x    =c(1, 0, 0, 1, 0, 1, 1, 1, 0, 0) ))
test.data$diff<-test.data$stop-test.data$start
test.surv<-with(test.data,Surv(start, stop, event))
test.cox1<-coxph(test.surv~x,test.data)
plot(test.cox1.surv<-survfit(test.cox1)$surv)

###one for each unique failure time

test.cox1<-coxph(test.surv~x,test.data)
test.cox2<-coxph(Surv(diff,event)~x,test.data)


Time, Status, X, Z, offsetvar, b, beta, link, emmax, eps



library(smcure)
smcure.orig<-smcure(Surv(stop,event)~x,cureform=~x,data=test.data,model="ph",Var=F)
detach("package:smcure", unload=TRUE)

tvcure.orig<-tvcure(Surv(stop,event)~x,cureform=~x,data=test.data,model="ph")
tvcure.count<-tvcure(test.surv~x,cureform=~x,data=test.data, model="ph",Var=F)

data(e1684)
pd <- test.cox1<-coxph(test.surv~x,test.data)
(Surv(FAILTIME,FAILCENS)~TRT+SEX+AGE,cureform=~TRT+SEX+AGE,
             data=e1684,model="ph",Var = FALSE)
printsmcure(pd,Var = FALSE)


tvcure.new<-tvcure(Surv(enter,exit,event)~ses,~ses,data=mort,model="ph",Var=T)
#unfortnuatnely, can't firgure out survival sestiamtes.
#Next step is to resuccsitate teh variance function

summary(smcure(a~ x, cureform=~X, data=test2))

test1 <- as.data.frame(list(time=c(4,3,1,1,2,2,3,4),
                            status=c(1,1,1,0,1,1,0,1),
                            x=c(0,2,1,1,1,0,0,2),
                            sex=c(0,0,0,0,1,1,1,1)))
coxtest1<-coxph(Surv(time,status)~x+sex,data=test1,ties="breslow")
#smsurv(test1$time,test1$status,cbind(test1$x,test1$sex),coxtest1$coefficients,test1$status,"ph")
plot(survfit(coxtest1)$surv)


smtest1<-smcure(Surv(time,status)~x,cureform=~x,data=test1,model="ph",Var=F)
plot(smtest1$s)

data(e1684)
pd <- smcure(Surv(FAILTIME,FAILCENS)~TRT+SEX+AGE,cureform=~TRT+SEX+AGE,data=e1684,model="ph")
pd <- tvcure(Surv(FAILTIME,FAILCENS)~TRT+SEX+AGE,cureform=~TRT+SEX+AGE,data=e1684,model="ph")
