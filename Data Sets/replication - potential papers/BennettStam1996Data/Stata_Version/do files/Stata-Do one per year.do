* The Duration of Interstate Wars, 1816-1985 - (Bennett and Stam 1996) - (APSR 90-2)
* Stata commands for one case per war model: Table 1 - Model 1~4 (TVC)
* This version generates predictions and computes error.


****************************************************************
** Commands to read data and estimate one case per war model  **
****************************************************************

clear

insheet using one_per_year.csv

* Note that a couple of variables were rescaled from this original data to give the coefficients in Table 1.
replace summper = summper/1000
replace sumpop = sumpop/1000000
replace reprsum=-1*reprsum

* In LIMDEP, war-years with censor==1 are those where the end of war is observed, so some are not censored by theory. Need to recode censoring in Stata
gen end=0
replace end=1 if year==lastyear
stset time, id(warnum) fail(end)

* Model 1
streg popratio year nactors, dist(weibull) nohr time 

* Model 2
streg oadm oada oadp opda rterrain rterrstr bofadj summper sumpop popratio qualrat surpdiff salscale, dist(weibull) nohr time 

* Model 3
streg reprsum demosum, dist(weibull) nohr time 

* Model 4
streg oadm oada oadp opda rterrain rterrstr bofadj summper sumpop popratio qualrat surpdiff salscale reprsum demosum adis3010 nactors, dist(weibull) nohr time 

* note: Adding the "robust" subcommand to the following equations will change the s.e. slightly, and in fact strengthen some results we found without robust s.e. estimates.



**********************************************************************
** Commands to do predicted values for Model 4 (Complete TVC model) **
**********************************************************************

* Naive model
streg , dist(weibull) nohr time  robust
outreg using one-per-year-predictions1.doc, nolabel replace se
predict preddurnaiive


*  Full model.  Here, do a robust estimation for new tables.
streg oadm oada oadp opda rterrain  rterrstr  bofadj  summper sumpop  popratio qualrat  surpdiff  salscale  reprsum  demosum salscale  adis3010  nactors, dist(weibull) nohr time robust
outreg using one-per-year-predictions2.doc, nolabel replace se
predict preddurfull


* Before doing any summaries, need to keep only the end year predictions so that error comparison can be 
* only for the final end of war.
keep if end==1
* Final length is now the time of each of these observations
gen length=time


egen meanlength=mean(length)
egen medlength=median(length)
sum length medlength
* mean length is 15.724
* median length is 5
* pred length from naiive (constant only) model on the TVC model is 6.071

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
* list warnum year  lastyear ccode1 ccode2 length preddurfull fullerror naierror



*********************************************************************
** Commands to do predicted values for Model 1 (Limited TVC model) **
*********************************************************************

clear

insheet using one_per_year.csv

gen end=0
replace end=1 if year==lastyear

stset time, id(warnum) fail(end)

*  Here, do a robust estimation for new tables.
streg popratio nactors year, dist(weibull) nohr time robust
outreg using one-per-year-predictions3.doc, nolabel replace se
predict preddurfull

streg, dist(weibull) nohr time  robust
predict preddurnaiive


* Before doing any summaries, need to keep only the end year predictions so that error comparison can be 
* only for the final end of war.
keep if end==1
* Final length is now the time of each of these observations
gen length=time


egen meanlength=mean(length)
egen medlength=median(length)
sum length medlength
* mean length is 15.724
* median length is 5
* pred length from naiive (constant only) model on the TVC model is 6.071

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




*************************************************************************
** Commands to do predicted values for Model 2 (Realpolitik TVC model) **
*************************************************************************

clear

insheet using one_per_year.csv

gen end=0
replace end=1 if year==lastyear

stset time, id(warnum) fail(end)

*  Here, do a robust estimation for new tables.
streg oadm oada oadp opda rterrain  rterrstr  bofadj  summper sumpop  popratio qualrat  surpdiff  salscale, dist(weibull) nohr time robust
outreg using one-per-year-predictions4.doc, nolabel replace se
predict preddurfull

streg, dist(weibull) nohr time  robust
predict preddurnaiive

* Before doing any summaries, need to keep only the end year predictions so that error comparison can be 
* only for the final end of war.
keep if end==1
* Final length is now the time of each of these observations
gen length=time


egen meanlength=mean(length)
egen medlength=median(length)
sum length medlength
* mean length is 15.724
* median length is 5
* pred length from naiive (constant only) model on the TVC model is 6.071

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




********************************************************************
** Commands to do predicted values for Model 3 (Regime TVC model) **
********************************************************************

clear

insheet using one_per_year.csv

gen end=0
replace end=1 if year==lastyear

stset time, id(warnum) fail(end)

*  Here, do a robust estimation for new tables.
streg reprsum  demosum, dist(weibull) nohr time robust
outreg using one-per-year-predictions5.doc, nolabel replace se
predict preddurfull

streg, dist(weibull) nohr time  robust
predict preddurnaiive

* Before doing any summaries, need to keep only the end year predictions so that error comparison can be 
* only for the final end of war.
keep if end==1
* Final length is now the time of each of these observations
gen length=time


egen meanlength=mean(length)
egen medlength=median(length)
sum length medlength
* mean length is 15.724
* median length is 5
* pred length from naiive (constant only) model on the TVC model is 6.071

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
