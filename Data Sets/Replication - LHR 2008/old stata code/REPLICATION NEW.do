clear
use "C:\Users\gwill_000\Dropbox\Methods Notes\Models - Duration\Cure Models Paper\replication - lhr\LHRIOOct08Replication.dta" 

****generate riskset variable
stsplit, at(failures) riskset(interval)
gen interval2 = log(interval)
bys interval: gen nfailint = sum(_d)

****replication code for LHR Mod 1
stcox archigosFIRC  archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, nohr efron cluster(LHRcluster)

**********Cox Models
****replication with Breslow:
stcox archigosFIRC  archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, nohr breslow cluster(LHRcluster)
****replication with Efron:
stcox archigosFIRC  archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, nohr efron cluster(LHRcluster)


****replication with Poisson
poisson _d archigosFIRC  archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt ibn.interval, nocons cluster(LHRcluster)
outreg2 using results, replace noaster
predict hazpois, xb
gen survpois = 1-hazpois
gen coxsnellpois = -log(survpois)
predict xbpois, xb
bys interval: gen sumxbpois = sum(xbpois)
gen inthazpois = nfailint/sumxbpois
twoway(line survpois capchange, sort)
twoway(scatter  inthazpois coxsnellpois)
*out of sample pp
capchange
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
egen m_interval = mean(interval)
gen obs=_n
keep if obs<=50
gen m_capchange = obs/10
keep m_*
renpfix m_
*xi: poisson _d archigosFIRC  archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt ibn.interval, nocons cluster(LHRcluster)
predict hazpoisout
gen survpoisout = 1-hazpoisout
twoway(line hazpoisout capchange)
twoway(line survpoisout capchange)
****ZIP model
zip _d archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt ibn.interval, inflate(archigosFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) nocons cluster(LHRcluster)
outreg2 using results, noaster
*add interactions
stcox capchange index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, efron cluster(LHRcluster) nohr
streg capchange index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, cluster(LHRcluster) nohr distribution(weibull)

streg archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire formal withdraw dmz ac pk ext_inv internal detail info disp_res onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, cluster(LHRcluster) distribution(weibull) hr
zip _d archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire formal withdraw dmz ac pk ext_inv internal detail info disp_res onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, inflate(archigosFIRC capchange index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster) nocons 
zip _d archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire withdraw dmz onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, inflate(archigosFIRC capchange withdraw dmz onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster) nocons 
