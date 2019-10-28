***Packages needed: crossfold, zipcv, coefplot, esttab/estout, sutex2
***Set up
cd "C:/Users/gwill_000/Dropbox/Methods Notes/Survival Analysis/Cure Models Paper/Replication - LHR" 
use "./LHRIOOct08Replication.dta", clear
cd "C:/Users/gwill_000/Dropbox/Methods Notes/Survival Analysis/Cure Models Paper" 
set seed 3211991
stset date1, id(id) failure(newwar) time0(date0) origin(time date0) 
la var _d "Failure (in interval)"
la var newwar "Failure (at all)" 
la var archigosFIRC "Foreign-Imposed Regime Change"
la var archigosFIRClnt "Foreign-Imposed Regime Change $\times \ln{t}$"
la var wernerFIRC "Foreign-Imposed Regime Change" 
la var wernerFIRClnt "Foreign-Imposed Regime Change $\times \ln(t)$"
la var capchange "Capability Change"
la var battletide "Decisive Victory"
la var thirdpartycfire "Third Party Intervention"
la var index "Agreement Strength"
la var tie "Tied War"
la var onedem5 "One Democracy"
la var twodem5 "Joint Democracy"
la var contiguity "Contiguous Dyad"
la var lndeaths "Battle Deaths"
la var cfhist "Conflict History"
la var cfhistlnt "Conflict History $\times \ln(t)$"
la var stakes "Existential Stakes"

***Kaplan Meier Plot
sts graph, ci ylabel(, nogrid angle(verticle)) ymtick(, nogrid) xtitle(Cease-fire Duration (in days, 10 year intervals)) xlabel(0(3650)32850, nogrid) title("") legend(off) scheme(s1mono)
graph export "lhrkm.eps", replace

***Format as Interval Data
stsplit, at(failures) riskset(interval)
drop if interval==.
la var interval "Risk Interval"

***Cox model
*crossfold stcox archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, cluster(LHRcluster) nohr k(10)

***Poisson model
eststo coxmod: 	stcox 		 	wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist cfhistlnt stakes contiguity, 			  cluster(LHRcluster) nohr
eststo poismod:	poisson _d 	 	wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist cfhistlnt stakes contiguity i.interval, cluster(LHRcluster)
eststo zipmod: 	zip		_d 		wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist cfhistlnt stakes contiguity i.interval, ///
						inflate(wernerFIRC  			 capchange battletide thirdpartycfire index 	    twodem5	tie lndeaths 	 		  	 		 contiguity) cluster(LHRcluster)
						
***Table with one-tailed significance tests
la var wernerFIRClnt "Foreign-Imposed Regime Change $\times \ln(t)$"
la var cfhistlnt "Conflict History $\times \ln(t)$"
esttab coxmod poismod zipmod using tab_lhr_primary.tex, replace cells(b(star fmt(3)) se(par fmt(3))) wrap booktabs ///
starlevels(* 0.10 ** 0.02 *** 0.002) stats(N aic bic, fmt(0 2 2) label ("N" "AIC" "BIC")) numbers label ///
keep(wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist cfhistlnt stakes contiguity _cons) ///
style(tex) unstack mlabels("\underline{Cox}" "\underline{Poisson}" "\underline{Zero-Inflated Poisson}", nodep) varlabel(_cons Constant)  ///
eqlabels(Duration Duration Inflation) collabels(,none) varwidth(60)

****Coefficient Plots
la var wernerFIRClnt "Foreign-Imposed Regime Change `=char(215)' ln(t)"
la var cfhistlnt "Conflict History `=char(215)' ln(t)"
*Hazards
coefplot (poismod, label(Cox) level(90)) (zipmod, label(Zero-Inflated Poisson) level(90) symbol(S)), ///
keep(battletide thirdpartycfire index onedem5 twodem5 tie lndeaths stakes contiguity) xline(0,lcol(gray20)) ///
graphregion(color(white)) plotregion(lcolor(black)) xsize(6) nokey coeflabels(,wrap(26))
graph save lhr1.gph, replace
coefplot (poismod, label(Cox) level(90)) (zipmod, label(Zero-Inflated Poisson) level(90) symbol(S)), ///
keep(wernerFIRC wernerFIRClnt cfhist cfhistlnt capchange) xline(0,lcol(gray20)) ///
graphregion(color(white)) plotregion(lcolor(black)) xtitle("Estimated Hazard Coefficients") xsize(6) coeflabels(,wrap(26))
graph save lhr2.gph, replace
graph combine lhr1.gph lhr2.gph, col(1) graphregion(color(white))
graph export "fig_lhr_coef.eps", replace 
*Inflation
coefplot zipmod, keep(inflate:) level(90) xline(0,lcol(gray20)) graphregion(color(white)) plotregion(lcolor(black)) drop(_cons) xtitle("Estimated Inflation Coefficients")
graph export "fig_lhr_coefl.eps", replace

***Descriptive Statistics
sutex2 _d wernerFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie ///
lndeaths cfhist stakes contiguity interval, minmax tabular varlab saving("descLHR.tex") replace

corr wernerFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity interval

***Proportional hazards tests
estat phtest, log detail
estat phtest, detail km	
estat phtest, detail log

*Schoenfeld Residual Plots
estat phtest, log plot(wernerFIRC) bwidth(.8) graphregion(color(white)) saving(phwerner,replace) title("FIRC")
estat phtest, plot(capchange) graphregion(color(white)) saving(capchange, replace) title("Capability Change")
estat phtest, plot(cfhist) graphregion(color(white)) saving(cfhist, replace) title("Conflict History")
estat phtest, plot(twodem5) graphregion(color(white)) saving(twodem5,replace) title("Two Democracies")
estat phtest, plot(contiguity) graphregion(color(white)) saving(contiguity,replace) title("Contiguity")
graph combine phwerner.gph capchange.gph cfhist.gph twodem5.gph contiguity.gph, graphregion(color(white)) saving(phschores, replace)				
graph export phschores.eps, replace
*Schoenfeld 
stcoxkm, by(wernerFIRC)
stcoxkm, by(capchange)
stcoxkm, by(cfhist)
*
stphplot, by(wernerFIRC)
stphplot, by(capchange)
stphplot, by(cfhist)



***Full ZIP Model
eststo zipmod2: 	zip		_d 		wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist cfhistlnt stakes contiguity i.interval, ///
							inflate(wernerFIRC 			 	 capchange battletide thirdpartycfire index onedem5 twodem5 tie	lndeaths cfhist 		  stakes contiguity) 			cluster(LHRcluster)
							
la var wernerFIRClnt "Foreign-Imposed Regime Change $\times \ln(t)$"
la var cfhistlnt "Conflict History $\times \ln(t)$"
esttab zipmod zipmod2 using tab_lhr_full.tex, replace cells(b(star fmt(3)) se(par fmt(3))) wrap booktabs ///
starlevels(* 0.20 ** 0.10 *** 0.02) stats(N aic bic, fmt(0 2 2) label ("N" "AIC" "BIC")) nonumbers label ///
keep(wernerFIRC wernerFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist cfhistlnt stakes contiguity) ///
style(tex) unstack mlabels(,none nodep) varlabel(_cons Constant)  ///
eqlabels(Hazard Inflation Hazard Inflation) collabels(,none) varwidth(60)


*equivalent: predict coxxb, xb predict poisxb, xb
*predicted hazards, survivor curves
gen survpred = exp(-exb)
margins, at(index=(0(1)10)) pr(p) level(90)
marginsplot
margins, at(index=(0(1)10)) pr(pr(1)) level(90)
marginsplot

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
