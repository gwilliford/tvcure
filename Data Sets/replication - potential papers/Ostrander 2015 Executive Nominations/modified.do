cd "C:\Users\gwill_000\Dropbox\Methods Notes\Survival Analysis\Cure Models Paper\Replication - Potential Papers\Executive Nominations"
use "exec noms 100-112_2.dta"

* Figure 1: Kaplan-Meier Survivor Function for Executive Nominations
  sts graph, ylabel(0(.2)1) xlabel(0(100)700) title("") ytitle("Proportion of Nominees Remaining") xtitle("Days Since Nomination") graphregion(color(white))


* Table 1: Time to Decision by Presidential Term & Congress
mean delay, over(congress)
tab congress


* Table 2: Delay Time in Days by Level of Position
mean delay, over(tier)


* Table 3: Duration of Nomination Decisions in the 100 to 112 Congresses

stset datelast, failure(censored==0) enter(time datenom) exit(time datelast) origin(time datenom) id(nominee)
stsplit, at(failures) riskset(interval)

stcox sendivide polarization pres_app_m first90 preselection lameduck i.idprez i.tier female priorconfirm workload defense Infrastructure Social, nohr
set matsize 1000
poisson _d sendivide polarization pres_app_m first90 preselection lameduck i.idprez i.tier female priorconfirm workload defense Infrastructure Social i.interval
zip _d sendivide polarization pres_app_m first90 preselection lameduck i.idprez i.tier female priorconfirm workload defense Infrastructure Social i.interval, ///
inflate(sendivide polarization pres_app_m first90 preselection lameduck female priorconfirm)



streg sendivide polarization pres_app_m first90 preselection lameduck i.idprez i.tier female priorconfirm workload defense Infrastructure Social, dist(weibull) nohr
stcox sendivide polarization pres_app_m first90 preselection lameduck i.idprez i.tier female priorconfirm workload defense Infrastructure Social
gen id = _n
stset datelast, failure(censored==0) enter(time datenom) exit(time datelast) origin(time datenom) id(id)
stsplit, at(failures) riskset(interval)
drop if interval==.
set matsize 1000
poisson _d sendivide polarization pres_app_m first90 preselection lameduck i.idprez i.tier female priorconfirm workload defense Infrastructure Social ibn.interval
xtset id interval
xtpoisson _d sendivide polarization pres_app_m first90 preselection lameduck i.idprez i.tier female priorconfirm workload defense Infrastructure Social

* Figure 2: Predicted Hazard Ratios
*    This Figure uses results from the output of the model described in 
*    Table 3 above.  R was used to create the figure and the code is
*    found in the associated R file.


* Duration Model (Table 3) without Agency Ideology Scores (Footnote 4)

 xi: streg sendivide polarization pres_app_m first90 preselection lameduck i.tier female priorconfirm workload defense Infrastructure Social, dist(weibull)
