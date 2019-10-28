cd "C:/Users/gwill_000/Dropbox/Methods Notes/Survival Analysis/Cure Models Paper/Replication - LHR" 
use "./LHRIOOct08Replication.dta", clear

***Kaplan Meier Plot
sts graph, ci ylabel(, nogrid angle(verticle)) ymtick(, nogrid) xtitle(Cease-fire Duration (in days, 10 year intervals)) xlabel(0(3650)32850, nogrid) title("") legend(off) scheme(s1mono)
graph export "lhrkm.eps", replace

***Format as Interval Data
stsplit, at(failures) riskset(interval)
drop if interval==.
la var interval "Risk Interval"

***Models
stcox 		 	wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist cfhistlnt stakes contiguity, 			  cluster(LHRcluster) nohr
poisson _d 	 	wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist cfhistlnt stakes contiguity i.interval, cluster(LHRcluster)
zip		_d 		wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist cfhistlnt stakes contiguity i.interval, ///
