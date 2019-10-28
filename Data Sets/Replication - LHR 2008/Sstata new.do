*****Use cured fraction data
cd "C:\Users\gwill_000\Dropbox\Methods Notes\Survival Analysis\Cure Models Paper\Monte Carlo Simulations"
import delimited "datinit.csv", clear
stset realtime, failure(realevent) id(v1)
stsplit, at(failures) riskset(interval)
drop if interval==.
gen bhaz = 0

*Std Cox
stcox x1 x2, nohr

*Std Weib
streg x1 x2, nohr dist(weibull)
streg x1 x2, time dist(weibull)

*SP Cox
zip _d x1 x2 i.interval, inflate(_cons)
zip _d x1 x2 i.interval, inflate(z1)

*SP Weib
strsmix x1 x2, dist(weibull) link(logistic) bhaz(bhaz)
