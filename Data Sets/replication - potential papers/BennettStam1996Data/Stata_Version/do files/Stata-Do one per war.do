* The Duration of Interstate Wars, 1816-1985 - (Bennett and Stam 1996) - (APSR 90-2)
* Stata commands for one case per war model: Table 1 - Model 5 (non-TVC)
* This version generates predictions and computes error.


****************************************************************
** Commands to read data and estimate one case per war model  **
****************************************************************

clear

insheet using one_per_war.csv

* Note that a couple of variables were rescaled from this original data to give the coefficients in Table 1.
replace summper = summper/1000
replace sumpopb = sumpopb/1000000
replace reprsum=-1*reprsum

stset length, id(warnum) fail(censor)

* Model 5
streg oadm oada oadp opda rterrain rterrstr bofadjbg summperb sumpopbg popratbg qualratb surpdiff salscale reprsumb demosumb adis3010 nactors, dist(weibull) nohr time

* note: adding the "robust" subcommand to the above will change the s.e. slightly, and in fact strengthen some results we found without robust s.e. estimates.



*************************************************
** Commands to do predicted values for Model 5 **
*************************************************

* naive prediction.
streg, dist(weibull) nohr time  robust
outreg using one-per-war-predictions1.doc, nolabel replace se
predict preddurnaiive


*  Full model.  Here, do a robust estimation for new tables.
streg oadm oada oadp opda rterrain  rterrstr  bofadjbg summperb sumpopbg popratbg qualratb surpdiff salscale  reprsumb demosumb adis3010  nactors, dist(weibull) nohr time  robust
outreg using one-per-war-predictions2.doc, nolabel replace se
predict preddurfull

egen meanlength=mean(length)
egen medlength=median(length)
sum length medlength
* mean length is 15.56
* median length is 5
* pred length from naiive (constant only) model is 5.8576

gen fullerror=preddurfull-length
gen naierror=preddurnaiive-length
gen absfullerr=abs(fullerror)
gen absnaierr=abs(naierror)
egen totabsfullerr = sum(absfullerr)
egen totabsnaierr = sum(absnaierr)

egen meanabsfullerr = mean(absfullerr)
egen medabsfullerr = median(absfullerr)
egen meanabsnaierr = mean(absnaierr)
egen medabsnaierr = median(absnaierr)

label var meanabsfullerr "Mean absolute errors, full model"
label var medabsfullerr "Median absolute errors, full model"
label var meanabsnaierr "Mean of absolute errors, naiive model"
label var medabsnaierr "Median of absolute errors, naiive model"

* summarize errors to get mean of errors, absolute errors, and sd.
sum fullerror naierror absfullerr absnaierr
sum meanabsfullerr medabsfullerr meanabsnaierr medabsnaierr



* Now compute error as % of length.  It turns out this is not a great measure, because 
* error can be greater than length.  Then, 2.5 (e.g.) means the error is 2.5x the length.
* This may not be a good measure then, because you are automatically very wrong for the shortest wars.
* e.g. in a 1 month war, a 6 month error leads to a 600% (bad) error, while in a 12 month war, that 
* same error is a 50% error.
* BUT, our model still does much better than the naiive model here.
gen pctactabsfullerr = absfullerr/length
gen pctactabsnaierr = absnaierr/length
egen meanpctactabsfullerr = mean(pctactabsfullerr )
egen meanpctactabsnaierr = mean(pctactabsnaierr )
label var meanpctactabsfullerr "Mean of absolute errors, full model, as pct of length"
label var meanpctactabsnaierr "Mean absolute errors, naiive model, as pct of length"

* Look at error as pct of length.
sum pctactabsfullerr pctactabsnaierr meanpctactabsfullerr meanpctactabsnaierr 



* Now generate total errors from naiive model, total errors from our model, and can do PRE.
* PRE:  (error1 - error2) / error1
egen totalnaiiveerrors=sum(absnaierr)
egen totalfullerrors=sum(absfullerr)
gen PRE=(totalnaiiveerrors - totalfullerrors)/totalnaiiveerrors
label var totalnaiiveerrors "Total of absolute errors, naiive model"
label var totalfullerrors "Total of absolute errors, full model"
label var PRE "Prop. reduction in total errors"

sum totalnaiiveerrors totalfullerrors PRE


* To see what cases are right/wrong, can list cases and errors
* sort fullerror
* list warnum frstyear  lastyear ccode1 ccode2 length preddurfull fullerror naierror
