setwd("C:/Users/gww17580/Dropbox/Methods Notes/Survival Analysis/Cure Models Paper/Replication - Thrower XO")
library(readstata13)
library(survival)

dat<-read.dta13("thrower_ajps_replication_main.dta")
dat2<-subset(dat,dat$ceremonial==0)
s<-with(dat2,Surv(current_year-1,current_year,time_revoked))
c1<-coxph(s~divdgovt+fds+nyt+public_mention+war+inflation+electyr+admchg+endterm+trend37+Truman+IKE+JFK+LBJ+Nixon+Ford+Carter+Reagan+Bush41+Clinton+Bush43+Obama+current_divdgovt+current_inflation+current_electyr+opposing_pres+log_eo_total+current_admchg+current_endterm+current_war+current_trend37+current_divdgovt+current_inflation+current_electyr+opposing_pres+log_eo_total+current_admchg+current_endterm+current_war+current_trend37+Truman2+IKE2+JFK2+LBJ2+Nixon2+Ford2+Carter2+Reagan2+Bush412+Clinton2+Bush432+Obama2,data=dat2)
c2<-coxph(s~current_divdgovt,data=dat2)

c1
c2
current_divdgovt+current_inflation+current_electyr
,+nohr+robust
divdgovt+fds+nyt+public_mention+war+inflation+electyr+admchg+endterm+trend37+Truman+IKE+JFK+LBJ+Nixon+Ford+Carter+Reagan+Bush41+Clinton+Bush43+Obama+current_divdgovt+current_inflation++current_electyr+opposing_pres+log_eo_total+current_admchg+current_endterm+current_war+current_trend37+Truman2+IKE2+JFK2+LBJ2+Nixon2+Ford2+Carter2+Reagan2+Bush412+Clinton2+Bush432+Obama2+if+ceremonial==0,+nohr+robust
