use "C:\Users\gwill_000\Dropbox\Research\1 - Data\3 - Replication - War Termination\LHRIOOct08replication.dta" 

****generate riskset variable
stsplit, at(failures) riskset(interval)
eststo clear
****replication code for LHR Mod 1
eststo: quietly stcox archigosFIRC  archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, nohr efron cluster(LHRcluster)
*replication with Breslow:
eststo: quietly stcox archigosFIRC  archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, nohr breslow cluster(LHRcluster)
*replication with Poisson
eststo: quietly poisson _d archigosFIRC  archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt ibn.interval, nocons cluster(LHRcluster)

****ZIP model
zip _d archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt ibn.interval, inflate(archigosFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) nocons cluster(LHRcluster) irr
*add interactions
zip _d archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt ibn.interval, inflate(archigosFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) nocons cluster(LHRcluster)
*drop cfhist and thirdpartyceasefire
zip _d archigosFIRC archigosFIRClnt capchange battletide index onedem5 twodem5 twodem5lnt tie lndeaths stakes contiguity contiguitylnt ibn.interval, inflate(archigosFIRC  capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity contiguitylnt) nocons cluster(LHRcluster)

****Regression Table
esttab using LHR.tex, compress replace booktabs title(LHR Replication\label{lhr})

****Kaplan-Meier curve
sts graph
