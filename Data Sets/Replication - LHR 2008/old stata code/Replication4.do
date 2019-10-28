clear
use "C:\Users\gwill_000\Dropbox\Methods Notes\Models - Duration\Cure Models Paper\replication - lhr\LHRIOOct08Replication.dta" 

***Cox
stcox archigosFIRC  archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, cluster(LHRcluster) nohr
outreg2 using results_posterV3_cox
estat ic
*11831   -257.0851    -207.485     15     444.9699    555.6471

***Poisson
stsplit, at(failures) riskset(interval)
poisson _d archigosFIRC  archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt ibn.interval, cluster(LHRcluster)
outreg2 using results_posterV3_cox
estat ic
*5524 .   -247.8926     31     557.7852    762.9078
margins if touse, at(capchange=(0(.1)5)) pr(pr(0)) 
marginsplot, recast(line) recastci(rarea) yline(1) plotr(m(zero)) xtitle("Capability Change") xtitle("Probability of Survival")

***Zero-Inflated Poisson
zip _d archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt ibn.interval, inflate(archigosFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)
outreg2 using results_posterV3_zip
estat ic
*5524   -278.5088   -233.1439     32     530.2878    742.0273
*Vuong Test
zip _d archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt ibn.interval, inflate(archigosFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) vuong
*Vuong test of zip vs. standard Poisson: z = 1.61  Pr>z = 0.0534

*capchange, probability of being a zero
margins, at(capchange=(0(.1)5)) pr(pr)
marginsplot, recast(line) recastci(rarea) plotr(m(zero)) title("Incidence Equation") ytitle("Probability of Being an Excess Zero")  xtitle("Capability Change") yline(0)

margins, at(capchange=(0(.1)5)) pr(pr(0))
marginsplot, recast(line) recastci(rarea) plotr(m(zero)) title("Hazard Equation") ytitle("Probability of Survival for Non-Longterm Survivors") xtitle("Capability Change")

margins, at(capchange=(0(.1)5)) pr(pr(1))

*capchange, probability of one
*predict prtest, pr(1)
*gen touse = e(sample)
*replace touse = 0 if prtest==.
*margins if touse, at(capchange=(0(.1)5)) pr(pr(1))
*marginsplot

*******
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
gen m_interval = 23.5
gen obs=_n-1
keep if obs<=50
gen m_capchange = obs/10
keep m_*
renpfix m_
predict prpois, pr
predict prpois1, pr(1)
predict prxb, xb


gen eprxb = exp(prxb)
twoway (line prpois capchange)
twoway (line eprxb capchange)
twoway (line prn capchange)
summarize(hrcox2)


predict blank
