/****************************************************************************/
/* Regime Duration do-file                                                  */
/* Last edited: HH & HS 10.06.05                                            */
/*                                                                          */
/****************************************************************************/


/* Introduction */


/* Variables Explained:                                                     */
/*   --ourtype, "Regime Category":                                          */
/*      0: Inconsistent                                                     */
/*      2: Autocracies                                                      */
/*      3: Democracies                                                      */
/*                                                                          */
/*   --app:                                                                 */
/*      0: Actual observations                                              */
/*      1: Simulated observations                                           */
/*                                                                          */
/*   --xrec: "Executive Recruitment"                                        */
/*      1: Cæsaristic                                                       */
/*      2: Ascription/Designation                                           */
/*      3: Dual: Ascription-Designation; Election                           */
/*      4: Election                                                         */
/*                                                                          */

clear all
set mem 100m
set matsize 100
set more off

use "H:\papers\inst\AJPS\Institutions_AJPS_Rewrite_GHJS_final_RC1.dta", clear
capture log close
log using "H:\papers\inst\AJPS\RC2\Institutions_AJPS_Rewrite_nov.log", replace


/* Table 1. Distribution of Polity Types 
   for the 1800-2000 and 1900-2000 Periods                                  */

tab ourtype if orig==1 & app == 0
tab ourtype if orig==1 & app==0 & year>=1900

stset endnd, id(stsetpolid) failure(status==1) origin(time entrydate) scale(365.25)


/* Table 2. Log-logistic Regression Estimates of 
   Polity Survival Time Ratios, 1800-2000                                   */
   
/*   Model 1:*/

xi: streg i.ourtype i.period if app == 0, dist(llogistic) robust tr

/* Table 3. Log-logistic Regression Estimates of Different Polity Types’ 
   Survival Time-Ratios, 1900-2000                                          */
/*   Model 2: Based on our definition of regime duration                    */

xi: streg i.ourtype cgdpcap gdpsq laggdpgr avgnabo firstpol i.period if app == 0 & year >=1900, dist(llogistic) robust tr

/*   Model 3: Based on our definition of regime duration for the period 1950-1990      */

xi: streg i.ourtype cgdpcap gdpsq laggdpgr avgnabo firstpol i.period if app == 0 & ((year >=1950) & (year <=1990)), dist(llogistic) robust tr


/* Excluding caesaristic polities */
/* xi: streg i.ourtype cgdpcap gdpsq laggdpgr avgnabo firstpol i.period if app == 0 & year >=1900 & xrec !=1, dist(llogistic) robust tr */

/*   Model 4: Based on Przeworski et al. definition of regime duration      */

use "H:\papers\inst\AJPS\Institutions_AJPS_Rewrite_prz_final_RC1.dta", clear
stset
streg dd_lagreg cgdpcap gdpsq laggdpgr dd_absnabo dd_first if csip2 !=., dist(llogistic) robust tr


/*   Model 5: Based on Przeworski et al. definition of regime duration      */

streg dd_lagreg interact csip2 cgdpcap gdpsq laggdpgr dd_absnabo dd_first, dist(llogistic) robust tr

/* Table 4. Log-logistic Regression Estimates of 
   Polity Survival Time-Ratios, 1900-2000      
                             */
/* Model 6:*/

use "H:\papers\inst\AJPS\Institutions_AJPS_Rewrite_GHJS_final_RC1.dta", clear

stset endnd, id(stsetpolid) failure(status==1) origin(time entrydate) scale(365.25)
streg cpart cxconst cxconpart cgdpcap gdpsq laggdpgr avgnabo firstpol if xrec==4 & app==0 & year >=1900, dist(llogistic) robust tr

/* Model 7:*/ 

streg cpart cxconst dual cxconpart partdual cgdpcap  gdpsq laggdpgr avgnabo firstpol if xrec!=4 & app==0 & year >=1900, dist(llogistic) robust tr

/* Excluding  caesaristic polities */
streg cpart cxconst dual cxconpart partdual cgdpcap  gdpsq laggdpgr avgnabo firstpol if xrec!=4 & xrec !=1 & app==0 & year >=1900, dist(llogistic) robust tr


/* Table 5. Estimated Survival Times Relative to Baseline for 			*/
/*   Polities with Open and Competitive Executive Recruitment, 1900-2000      */

stci if app==0 & xrec==4
replace xrec=4 if app==1

/* Model 6 reestimated to predict for appendix data */
/* Reestimating to include app observations */

capture drop m5pred
streg cpart cxconst cxconpart cgdpcap gdpsq laggdpgr avgnabo firstpol if xrec==4 & app==0 & year >=1900, dist(llogistic) robust tr
predict m5pred, median time 


/* Calculating cell counts for Table 5*/
/*capture drop cons4
gen cons4 = xconst*/
replace cons4 = xconst if app==0
recode cons4 2/3=2 4/5=3 6/7=4 if app==0
/*capture drop part4
gen part4 = 0 if part ~=.*/
replace part4 = 0 if part ~=. & app==0
recode part4 0=1 if part > 0 & exp(part)<=7 & app==0
recode part4 0=2 if exp(part)>7 & exp(part)<=30 & app==0
recode part4 0=3 if exp(part)>30 & app==0

/* Tabulating for all polities with data, obtaining cell counts */
tabulate part4 cons4 if orig== 1 & xrec==4 & cgdpcap ~=. & avgnabo ~=. & firstpol~=. & app == 0

table part4 cons4 if app==1, c(mean m5pred)


/* Table 6a. Estimated Median Survival Times for Polities 
   with Designated or Ascribed Executive: 1900-2000                         */


stci if app==0 & xrec~=4

replace xrec=1 if app==1

/* Model 7 reestimated to predict for appendix data */
/* INCLUDING CAESARISTIC POLITIES */

capture drop m6pred
streg cpart cxconst dual cxconpart partdual cgdpcap  gdpsq laggdpgr avgnabo firstpol if xrec!=4  & app==0 & year >=1900, dist(llogistic) robust tr
predict m6pred, median time 



/* Calculating cell counts for Table 6a: designated executifve */
/* Tabulating for all polities with data, obtaining cell counts */
tabulate part4 cons4 if orig== 1 & xrec~=4 & dual==0 & cgdpcap ~=. & avgnabo ~=. & firstpol~=. & app == 0

table part4 cons4 if app==1 & dual==0 , c(mean m6pred)


/* Table 6b. Estimated Median Survival Times for 
   Polities with Dual Executive: 1900-2000                                  */

/* Calculating cell counts for Table 6b: dual*/
/* Tabulating for all polities with data, obtaining cell counts */

tabulate part4 cons4 if orig== 1 & xrec~=4 & dual == 1 & cgdpcap ~=. & avgnabo ~=. &firstpol~=. & app == 0

table part4 cons4 if app==1 & dual == 1, c(mean m6pred)

/* Table 6b. Estimated Median Survival Times for 
   Polities with Dual Executive: 1900-2000                                  */

/* Calculating cell counts for Table 6b: dual*/
/* Tabulating for all polities with data, obtaining cell counts */

tabulate part4 cons4 if orig== 1 & xrec~=4 & cgdpcap ~=. & avgnabo ~=. &firstpol~=. & app == 0

table part4 cons4 if app==1, c(mean m6pred)
