clear
use "C:\Users\gwill_000\Dropbox\Methods Notes\Models - Duration\Cure Models Paper\replication - lhr\LHRIOOct08Replication.dta" 
stsplit, at(failures) riskset(interval)

stcox archigosFIRC  archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt, cluster(LHRcluster)

predict pr1, pr(1)
keep if e(sample)
replace touse = 0 if missing(pr1)
margins, at(index=(0(.2)10)) pr(pr(0))
marginsplot, l(90)
margins, pr(n) at(index=(0(.2)10))

poisson _d archigosFIRC  archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt ibn.interval, nocons cluster(LHRcluster) irr
predict prtest, pr(1)
gen touse = e(sample)
replace touse = 0 if prtest==.
margins if touse, at(index=(0(.2)10)) pr(pr(0))
marginsplot, recast(line) recastci(rarea) plotr(m(zero)) xtitle("Agreement Strength Index") ytitle("Survival Probability (in one interval)") title("Single Population Model") yline(1)
estat ic

twoway(line prtest interval)
drop prtest2 touse2
predict prtest2, pr(1)
gen touse2 = e(sample)
replace touse2 = 0 if prtest2==.
margins, at(index=(0(.2)10))

****Plotting hazard at mean of all covariates
	keep if interval==1
	collapse archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt
	expand 46
	gen obs = _n
	rename obs interval
	predict xbmean, xb
	gen exbmean = exp(xbmean)
	twoway (connected exbmean interval, sort connect(stairstep))

*Vuong Test
zip _d archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt ibn.interval, inflate(archigosFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) cluster(LHRcluster)
outreg2 using outregreal, replace
zip _d archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt ibn.interval, inflate(archigosFIRC capchange battletide thirdpartycfire index onedem5 twodem5 tie lndeaths cfhist stakes contiguity) nocons cluster(LHRcluster) 
outreg2 using outregreal
margins, at(index=(0(.2)10)) pr(pr(0))
marginsplot, recast(line) recastci(rarea) plotr(m(zero)) xtitle("Agreement Strength Index") ytitle("Probability of Survival") yline(0) title("Hazard Equation")
estat ic

margins, at(index=(0(.2)10)) pr(pr)
marginsplot, recast(line) recastci(rarea) plotr(m(zero)) xtitle("Agreement Strength Index") ytitle("Probability of Being an Excess Zero") yline(0) title("Incidence Equation")

estat ic
margins, at(archigosFIRC=(0 1))
marginsplot

predict zipxb, xb
gen zipexb = exp(zipxb)

*e(b) matrix
matrix list e(b)
*0-15 - poisson coefficients
*16-61 - interval coefficients
*62-74 - inflation coefficients

*enter mata environment
mata
*create a matrix of coefficients
b = st_matrix("e(b)")
*create a matrix of just interval coefficients
logh = b[16..61]
st_addvar("float","logh")
st_store(.,"pmg",pmg)
*get baseline hazard at each interval
h = exp(logh)
*create sum of baseline hazard
H=runningsum(h)
*create survivor function
S=exp(-H)
*exit mata
end
matrix list e(b)
matrix A = get(_b)
matrix list A
matrix B = get(A[16..61])

***using predict
keep if interval==1
collapse archigosFIRC archigosFIRClnt capchange battletide thirdpartycfire index onedem5 twodem5 twodem5lnt tie lndeaths cfhist stakes contiguity contiguitylnt
expand 46
gen obs = _n
rename obs interval
predict xbmean, xb
gen exbmean = exp(xbmean)
twoway (connected exbmean interval, sort connect(stairstep))

replace capchange = 4.56
predict xbmean2, xb
gen exbmean2 = exp(xbmean2)
twoway (connected exbmean interval, sort connect(stairstep))(connected exbmean2 interval, sort connect(stairstep))


gen H = exbmean[_n]+exbmean[_n-1]
