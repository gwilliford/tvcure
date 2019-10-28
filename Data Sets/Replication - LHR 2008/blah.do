cd "C:\Users\gwill_000\Dropbox\Methods Notes\Survival Analysis\Cure Models Paper\Replication - Mattes and Savun"
import delimited ISQrepdata.csv, clear
drop _d _t _t0
stset peacedur, id(idnumber) failure(peacefail)
stsplit, at(failures) riskset(interval)
drop if interval==.
stcox polpssum milpssum econpssum terrpssum costinc guarantee issue costs duration, efron cluster(ccode) 

zip _d 	polpssum milpssum econpssum terrpssum costinc guarantee issue costs duration, ///
inflate(_cons) cluster(ccode)
