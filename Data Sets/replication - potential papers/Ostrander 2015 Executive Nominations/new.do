***Set up
clear
cd "C:\Users\gwill_000\Dropbox\Methods Notes\Survival Analysis\Cure Models Paper\Replication - Potential Papers\Executive Nominations"
use "exec noms 100-112_2.dta", clear
gen id = _n
stset datelast, failure(censored==0) enter(time datenom) exit(time datelast) origin(time datenom) id(id)
stsplit, at(failures) riskset(interval)
drop if interval==.

***Replicate Original Weibull Model
streg sendivide polarization pres_app_m first90 preselection lameduck i.idprez i.tier female priorconfirm workload defense Infrastructure Social, dist(weibull) nohr
***Replicate Cox Model
stcox sendivide polarization pres_app_m first90 preselection lameduck i.idprez i.tier female priorconfirm workload defense Infrastructure Social
***
poisson _d sendivide polarization pres_app_m first90 preselection lameduck i.idprez i.tier female priorconfirm workload defense Infrastructure Social, offset(interval)
