clear
use "C:\Users\gwill_000\Dropbox\Methods Notes\Models - Duration\Cure Models Paper\replication - lhr\LHRIOOct08Replication.dta" 

****generate riskset variable
stsplit, at(failures) riskset(interval)
gen interval2 = log(interval)
bys interval: gen nfailint = sum(_d)

****replication code for LHR Mod 1
stcox archigosFIRC  archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, nohr efron cluster(LHRcluster)

****replication with Breslow:
stcox archigosFIRC  archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, breslow nohr
predict hrcox
predict bases
egen m_archigosFIRC= mean(archigosFIRC) 
egen m_archigosFIRClnt= mean(archigosFIRClnt) 
egen m_battletide= mean(battletide)
egen m_thirdpartycfire= mean(thirdpartycfire)
egen m_index =mean(index)
egen m_onedem5 =mean(onedem)
egen m_twodem5 =mean(twodem5)
egen m_twodem5lnt =mean(twodem5lnt)
egen m_tie =mean(tie) 
egen m_lndeaths =mean(lndeaths) 
egen m_cfhist =mean(cfhist) 
egen m_stakes =mean(stakes) 
egen m_contiguity =mean(contiguity)
egen m_contiguitylnt =mean(contiguitylnt)
gen obs=_n-1
keep if obs<=50
gen m_capchange = obs/10
keep m_*
renpfix m_
predict hrcox, hr
twoway (line hrcox capchange)
gen hrcox2 = hrcox/100
twoway (line hrcox2 capchange)
summarize(hrcox2)

gen expcox = exp(xb)
predict hr
predict basehazcox, basechazard
predict csnellcox, csnell
predict s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15, schoenfeld
twoway (scatter capchange s3)
twoway (scatter  basehazcox csnellcox)
*gen xb_cap_pois = exp(capchange*.2703646)
*bys interval: gen sdenpois = sum(xb_cap_pois)) if _d==1
*bys interval: gen snumpois = sum(capchange*sdenpois) if _d==1
*gen srespois = capchange-(snum/sden)
twoway (scatter srespois interval)

****Xb
predict xbpois, xb
gen expxbpois = exp(xbpois)
gen cap_expxbpois = capchange*expxbpois
bys interval: gen jnum = sum(cap_expxbpois) if _d==1
bys interval: gen jden = sum(expxbpois) if _d==1
gen srespoiscap = capchange - (jnum/jden) if _d==1
twoway (scatter srespoiscap interval)

gen one = 1 if e(sample)
stcox one if e(sample), breslow 
lrtest cox, dir
est sto cox
lr = -2 ln(L(m1)/L(m2)) = 2(ll(m2)-ll(m1))
display -2*log(-207.48497/-257.08512)
*42869667
*.50696825

poisson _d archigosFIRC  archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt ibn.interval
est sto pois 
poisson _d ibn.interval if e(sample), nocons
lrtest pois
display -2*log(-247.89261/-297.49275 )
*40744017
*.46927436

****replication with Poisson
poisson _d archigosFIRC  archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, nocons cluster(LHRcluster)
poisson _d archigosFIRC  archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt ibn.interval, nocons cluster(LHRcluster) irr
twoway (line interval 
outreg2 using results, replace noaster
margins, at(capchange=(0(.1)5)) pr(1)
marginsplot
help poisson postestimation
predict irpois, ir
predict hazpois, xb
gen survpois = 1-hazpois
gen coxsnellpois = -log(survpois)
predict xbpois, xb
gen exbpois = exp(xbpois)
bys interval: gen sumexbpois = sum(exbpois)
gen inthazpois = nfailint/sumexbpois
twoway(line survpois capchange, sort)
twoway(scatter  inthazpois coxsnellpois)
*out of sample pp
egen m_archigosFIRC= mean(archigosFIRC) 
egen m_archigosFIRClnt= mean(archigosFIRClnt) 
egen m_battletide= mean(battletide)
egen m_thirdpartycfire= mean(thirdpartycfire)
egen m_index =mean(index)
egen m_onedem5 =mean(onedem)
egen m_twodem5 =mean(twodem5)
egen m_twodem5lnt =mean(twodem5lnt)
egen m_tie =mean(tie) 
egen m_lndeaths =mean(lndeaths) 
egen m_cfhist =mean(cfhist) 
egen m_stakes =mean(stakes) 
egen m_contiguity =mean(contiguity)
egen m_contiguitylnt =mean(contiguitylnt)
gen m_interval = 23
gen obs=_n-1
keep if obs<=50
gen m_capchange = obs/10
keep m_*
renpfix m_
*xi: poisson _d archigosFIRC  archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt ibn.interval, nocons cluster(LHRcluster)
predict hazpoisout, ir
drop hazpoisout
predict hazpoisout2, pr(1)
predict hazpoisout3 *option n
predict hazpoisout4, xb
gen exphazpoisout = exp(hazpoisout4)
predict hazpoisse, stdp
gen hazup = hazpoisse*1.96
gen hazdw = hazpoisse*-1.96
gen survpoisout = 1-hazpoisout
twoway(line hazpoisout capchange)
twoway(line hazpoisout2 capchange)
twoway(line hazpoisout3 capchange)
twoway(line hazpoisout4 capchange)
twoway(line exphazpoisout capchange)
summarize(hazpoisout)
 (line hazup capchange) (line hazdw capchange)
twoway(line survpoisout capchange)
****ZIP model
clear
use "C:\Users\gwill_000\Dropbox\Methods Notes\Models - Duration\Cure Models Paper\replication - lhr\LHRIOOct08Replication.dta" 
stsplit, at(failures) riskset(interval)
zip _d archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt ibn.interval, inflate(archigosFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) nocons cluster(LHRcluster)
outreg2 using results, noaster
margins, at(capchange=(0(.1)5))
marginsplot
egen m_archigosFIRC= mean(archigosFIRC) 
egen m_archigosFIRClnt= mean(archigosFIRClnt) 
egen m_battletide= mean(battletide)
egen m_thirdpartycfire= mean(thirdpartycfire)
egen m_capchange=mean(capchange)
*egen m_index =mean(index)
egen m_onedem5 =mean(onedem)
egen m_twodem5 =mean(twodem5)
egen m_twodem5lnt =mean(twodem5lnt)
egen m_tie =mean(tie) 
egen m_lndeaths =mean(lndeaths) 
egen m_cfhist =mean(cfhist) 
egen m_stakes =mean(stakes) 
egen m_contiguity =mean(contiguity)
egen m_contiguitylnt =mean(contiguitylnt)
egen m_interval = mean(interval)
gen obs=_n-1
keep if obs<=20
gen m_index = obs/2
keep m_*
renpfix m_
predict hazzip, pr(1)
predict hazzip2, xb
gen hazzip3 = exp(hazzip2)
predict hazzip4
predict hazzip5, xb
predict hazzipse, stdp
gen hazzipup = hazzipse*1.96
gen hazzipdw = hazzipse*-1.96
gen survzip = 1-hazzip
twoway(line hazzip capchange)
twoway(line hazzip4 capchange)(line hazzipup capchange) (line hazzipdw capchange)
twoway(line survzip capchange)

predict indexpois
twoway(line indexpois index)

predict ct zero

predict p_zero, pr
predict st, stdp
gen p_zero_up = p_zero+(1.96*st)
gen p_zero_lo = p_zero-(1.96*st)
twoway(line p_zero index, sort) (line p_zero_up index, sort) (line p_zero_lo index, sort), xtitle("Agreement Strength Index") ytitle("Predicted Probability Cease-Fire Does Not Fail")

predict p_one, pr()
twoway(line p_one index), ytitle("Agreement Strength Index") xtitle("Hazard Ratio")

margins, at(index=(0(.5)10)) predict(pr(1))
marginsplot

*add interactions
stcox capchange index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, efron cluster(LHRcluster) nohr
streg capchange index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, cluster(LHRcluster) nohr distribution(weibull)

streg archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire formal withdraw dmz ac pk ext_inv internal detail info disp_res onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, cluster(LHRcluster) distribution(weibull) hr
zip _d archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire formal withdraw dmz ac pk ext_inv internal detail info disp_res onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, inflate(archigosFIRC capchange index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster) nocons 
zip _d archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire withdraw dmz onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, inflate(archigosFIRC capchange withdraw dmz onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster) nocons 
