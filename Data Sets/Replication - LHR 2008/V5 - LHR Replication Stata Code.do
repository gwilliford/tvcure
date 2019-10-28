************Set UP****************************************************************
use "C:/Users/gwill_000/Dropbox/Methods Notes/Survival Analysis/Cure Models Paper/replication - lhr/LHRIOOct08Replication.dta" 
use "./replication - LHR/LHRIOOct08Replication.dta"
*stset date1, id(id) failure(newwar) time0(date0) origin(time date0)
stsplit, at(failures) riskset(interval)
set more off
drop if interval==.
***********************************************************************************

*original LHR model - cox
stcox archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, cluster(LHRcluster) nohr
predict coxxb, xb
predict coxhr, hr
predict cox_basehc, basehc
predict cox_basesurv, basesurv
*original LHR model - poisson 
poisson _d archigosFIRC  archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt ibn.interval, cluster(LHRcluster) nocons
predict pois_xb, xb
	*the following three produce identical results
	predict pois_ir, ir
	predict pois_n, n
	gen pois_exb = exp(pois_xb)
	predict pois_1, pr(1)
	
margins, at(capchange=(0(.25)5))
marginsplot

margins, at(capchange=(0(.25)5))
marginsplot

*equivalent: predict coxxb, xb predict poisxb, xb
zip _d archigosFIRC  archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt ibn.interval, inflate(_cons) cluster(LHRcluster)
predict zipn
est sto pois
estat ic 
* N		*LL Null	**LL Model			  *AIC			*BIC
* 5524           .   -273.9705     32     611.9409    823.6804

zip _d capchange battletide thirdpartycfire index onedem5 tie lndeaths cfhist stakes contiguity ibn.interval, inflate(capchange battletide thirdpartycfire index onedem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)
est sto zip
* 5524   -283.5143   -256.4101     32     576.8203    788.5597

****Create Coefficient Plot
coefplot (pois, label(Poisson)) (zip, label(Zero-Inflated Poisson) msymbol(S)), keep(capchange battletide thirdpartycfire index onedem5 tie lndeaths cfhist stakes contiguity) xline(0)coeflabels(_cons = ”Constant” capchange = "Capability Change" battletide = "Battle Tide" thirdpartycfire = "Third Party Intervention" index="Agreement Strength" onedem5 = "One Democracy" tie = "Tie" lndeaths = "Battle Deaths" cfhist = "Conflict History" stakes = "Stakes" contiguity = "Contiguous", notick labgap(3)) graphregion(color(white)) plotregion(lcolor(black))
graph save Graph "C:\Users\gwill_000\Dropbox\Methods Notes\Models - Duration\Cure Models Paper\figures\lhr_coef_plot.gph"
graph export "C:\Users\gwill_000\Dropbox\Methods Notes\Models - Duration\Cure Models Paper\figures\lhr_coef_plot.eps", as(eps) preview(off) replace

zip _d capchange battletide thirdpartycfire index onedem5 tie lndeaths cfhist stakes contiguity ibn.interval, inflate(capchange battletide thirdpartycfire index onedem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)

***all variables minus lnt
archigosFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity

***zip with all variables
zipcv _d archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt ibn.interval, inflate(archigosFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)
***zip with all variables minus lnt
zipcv _d archigosFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity ibn.interval, inflate(archigosFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)

***zip with all variables minus lnt, no regime type
zipcv _d archigosFIRC capchange battletide thirdpartycfire index tie lndeaths cfhist stakes contiguity ibn.interval, inflate(archigosFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)

***
zipcv _d capchange battletide thirdpartycfire index tie lndeaths cfhist stakes contiguity ibn.interval, inflate(capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)
zipcv _d capchange battletide thirdpartycfire index tie lndeaths cfhist stakes contiguity ibn.interval, inflate(capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) vuong n

***Inflate: democracy only in the inflation equation
zipcv _d archigosFIRC capchange battletide thirdpartycfire index tie lndeaths ///
cfhist stakes contiguity ibn.interval, ///
inflate(archigosFIRC capchange battletide thirdpartycfire index  ///
onedem5 twodem5 tie lndeaths ///
cfhist stakes contiguity) 

***predicted hazards, survivor curves
gen survpred = exp(-exb)

***schoenfeld residuals
	*predict relative risk for each observation
	predict xb, xb
	gen exb = exp(xb)
	
	*for each obs, take the x value and multiply by exb
	gen xexb = xvar*exb
	*sum above value across all individuals at risk at each failure time
	if _d==1
	egen snum = sum(xexb)
	*sum exb for all individuals
	egen sden sum(exb)
	*divide 1 by 2
	gen xbar snum/sden
	*take x value for individual and subtract above for all censored observations
	gen s_x - x_bar
	*rescale residuals
		*variance of X at the kth event time
		sum (y_i(t_k)r_i(t_k)s_x*s_x) / sum(Y_i(t_k)r_i(t_k))
		*scaled residuals = inverse variance times s_k
	*within time period k
	
***variables
	Only whether: onedem5 twodem5
	Only when: 
	Either: archigosFIRC capchange battletide thirdpartycfire index* tie lndeaths cfhist stakes* contiguity

	
**************************WERNER CODING
zipcv _d wernerFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity ibn.interval, inflate(wernerFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)
set more off


zipcv _d wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist cfhistlnt stakes contiguity contiguitylnt ibn.interval, inflate(wernerFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)
logit newwar wernerFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity, cluster(LHRcluster)

stcox wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist cfhistlnt stakes contiguity,cluster(LHRcluster)
est sto cox
poisson _d wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist cfhistlnt stakes contiguity ibn.interval,cluster(LHRcluster)
est sto pois
zipcv _d wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index tie lndeaths cfhist cfhistlnt stakes contiguity ibn.interval, inflate(wernerFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)
est sto zip
coefplot (pois, label(Poisson) level(90)) (zip, label(Zero-Inflated Poisson) level(90) msymbol(S)), keep(wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist cfhistlnt stakes contiguity) xline(0) graphregion(color(white)) plotregion(lcolor(black)) coeflabels(,notick labgap(3))
coeflabels(_cons = "Constant", capchange = "Capability Change" battletide = "Battle Tide" thirdpartycfire = "Third Party Intervention" index="Agreement Strength" onedem5 = "One Democracy" tie = "Tie" lndeaths = "Battle Deaths" cfhist = "Conflict History" stakes = "Stakes" contiguity = "Contiguous") 
graph save Graph "C:\Users\gwill_000\Dropbox\Methods Notes\Models - Duration\Cure Models Paper\figures\lhr_coef_plot.gph"
graph export "C:\Users\gwill_000\Dropbox\Methods Notes\Models - Duration\Cure Models Paper\figures\lhr_coef_plot.eps", as(eps) preview(off) replace
help zip margins
margins, at(index=(0(1)10)) pr(p) level(90)
marginsplotv
margins, at(index=(0(1)10)) pr(pr(1)) level(90)
marginsplot

test [_d]wernerFIRC [_d]wernerFIRClnt

*
FIRC increases the probability of breakdown, but decreases the hazard, but the effect decays(?) over time
lndeaths decreases the probability that conflict recurs, but increases the hazard 
ties increase the hazard
thirdparty ceasefires are less likely to break down, but have a higher hazard
capchange increases probability of recurrence, and increase the hazard
index more likely to recur, decreases the hazard



*****Appendix
*Variable labels
la var newwar "Cease-Fire Failure" 
la var _d "Cease-Fire Failure"
la var archigosFIRC "Foreign-Imposed Regime Change"
la var wernerFIRClnt "Foreign-Imposed Regime Change $\times \ln(t)$"
la var wernerFIRC "Foreign-Imposed Regime Change" 
la var capchange "Capability Change"
la var battletide "Descisive Victory"
la var thirdpartycfire "Third Party Intervention"
la var index "Agreement Strength"
la var tie "Tied Battle"
la var onedem5 "One Democracy"
la var twodem5 "Two Democracies"
la var contiguity "Contiguous Dyad"
la var lndeaths "Battle Deaths"
la var cfhist "Conflict History"
la var cfhistlnt "Conflict History $\times \ln(t)$"
la var stakes "Existential Stakes"
la var interval "Risk Interval"
*Descriptive Stats
sutex2 _d archigosFIRC wernerFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie ///
lndeaths cfhist stakes contiguity interval, minmax percentiles(50) ///
varlab tablab(tab:descLHR) caption(Descriptive Statistics for Cease-Fire Data) saving("./tables/descLHR.tex") replace

******Codes
zipcv
coefplot
sutex2

*****Archigos
***All variables in both components
zipcv _d archigosFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity ibn.interval, inflate(archigosFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)
***All varaibles in inflation, duration does not include regime type
zipcv _d archigosFIRC capchange battletide thirdpartycfire index tie lndeaths cfhist stakes contiguity ibn.interval, inflate(archigosFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)
***Above, with archigoslnt
zipcv _d archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire index tie lndeaths cfhist stakes contiguity ibn.interval, inflate(archigosFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)
***Drop all Archigos
zipcv _d capchange battletide thirdpartycfire index tie lndeaths cfhist stakes contiguity ibn.interval, inflate(capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)
***Above, add back democracy variables
zipcv _d capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity ibn.interval, inflate(capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)
***FULL MODEL (all vars in both with lnts)
zipcv _d archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist cfhistlnt stakes contiguity contiguitylnt ibn.interval, inflate(archigosFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)
***Above, drop regime type from duration
zipcv _d archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire index tie lndeaths cfhist cfhistlnt stakes contiguity contiguitylnt ibn.interval, inflate(archigosFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)


*****Werner
***All with lnts --- doesn't converge
zipcv _d wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist cfhistlnt stakes contiguity ibn.interval, inflate(wernerFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster) tech(nr)
***Above, strip wernerFIRClnt (keep regime type) --- converges - mostly well behaved, regime type doesn't change much
*(A)
zipcv _d wernerFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist cfhistlnt stakes contiguity ibn.interval, inflate(wernerFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)
***All, drop regime type --- very well behaved
*(B)
zipcv _d wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index tie lndeaths cfhist cfhistlnt stakes contiguity ibn.interval, inflate(wernerFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)
***Above, add back regime type --- does not converge
zipcv _d wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist cfhistlnt stakes contiguity ibn.interval, inflate(wernerFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)

****
Coefplot
Estout

*****Models for Workshop
***cox
stcox wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist cfhistlnt stakes contiguity, cluster(LHRcluster) nohr
eststo cox
***poisson 
poisson _d wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist cfhistlnt stakes contiguity ibn.interval, cluster(LHRcluster)
eststo poisson
***zip
zipcv _d wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index tie lndeaths cfhist cfhistlnt stakes contiguity ibn.interval, inflate(wernerFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)
eststo zip

***Significance test for interaction term
test [_d]wernerFIRC [_d]wernerFIRClnt

***Kaplan Meier Plot
sts graph, ci ylabel(, nogrid) ymtick(, nogrid) xtitle(Cease-fire Duration (in days)) xlabel(, nogrid) title("") legend(off) scheme(s1mono)


***Create Regression Table
esttab cox poisson zip using ./tables/zipLHR.tex, replace ///
cells(b(star fmt(3)) se(par fmt(3))) ///
label title(Duration of International Cease-Fire Agreements \label{tab:LHR}) varlabel(_cons Constant)  wrap ///
starlevels(* 0.20 ** 0.10 *** 0.02)  ///
stats(N aic bic, fmt(0 2 2) label ("N" "AIC" "BIC")) ///
keep(wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist cfhistlnt stakes contiguity) ///
numbers ///
style(tex) booktabs unstack ///
mlabels("Cox" "Poisson" "Zero-Inflated Poisson", nodep) ///
eqlabels(Duration Duration Inflation) collabels(,none) ///
postfoot("\end{tabular}\begin{tablenotes}\midrule\\ \textit{Note:} Standard errors in parentheses. Standard errors are clustered by the conflict that caused each cease-fire.  ***$ p <.01$, **$ p <.05$, *$ p <.10$, one-tailed tests. Dummy variables for failure times have been omitted to save space. \end{tablenotes}\end{table}")

***Create Coefficient Plot
coefplot (poisson, label(Poisson)) (zip, label(Zero-Inflated Poisson) msymbol(S)), keep(wernerFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) xline(0) coeflabels(,notick labgap(3)) graphregion(color(white)) plotregion(lcolor(black)) levels(90)
graph save Graph "./figures/lhr_coef_plot.gph", replace
graph export "./figures/lhr_coef_plot.eps", as(eps) replace
