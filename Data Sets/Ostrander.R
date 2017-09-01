library(readstata13)
library(plyr)

ostrander <- read.dta13("C:/Users/gwill/Dropbox/Methods Notes/Survival Analysis/Cure Models Paper/replication - potential papers/Ostrander 2015 Executive Nominations/exec noms 100-112_2.dta")
ostrander <- rename(ostrander, replace = c("_st" = "st", "_d"="event","_t"="stop","_t0"="start","_origin"="origin"))

ostrander.tvcure<-tvcure(Surv(start,stop,event)~sendivide+polarization+pres_app_m+first90+preselection+lameduck+as.factor(idprez)+as.factor(tier)+female+priorconfirm+workload+defense+Infrastructure+Social,cureform=~sendivide+polarization+pres_app_m+first90+preselection+lameduck+female+priorconfirm+workload+defense+Infrastructure+Social,data=ostrander,na.action=na.omit,Var=T, model="ph", nboot=10, emmax=1000)
test6<-tvcure(Surv(FAILTIME,FAILCENS)~TRT+SEX+AGE,cureform=~TRT+SEX+AGE, data=e1684,model="ph",Var = T,firthlogit = TRUE)
