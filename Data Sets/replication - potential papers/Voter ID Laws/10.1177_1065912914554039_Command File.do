

*BELOW IS REPLICATION CODE FOR THE 3 MODELS PRESENTED IN:
*HICKS, WILLIAM D., SETH C. MCKEE, MITCHELL D. SELLERS, AND DANIEL A. SMITH, 
*     "A PRINCIPLE OR STRATEGY? VOTER IDENTIFICATION LAWS AND PARTISAN COMPETITION
*      IN THE AMERICAN STATES," POLITICAL RESEARCH QUARTELY  


***************************************************************************
*                                                                         **                  LOAD REPLICATION_DATA.CSV                              *
*                     AND LABEL VARIABLES                                 * 
*                                                                         *
*NOTE: QUANTITATIVE PREDICTORS IN PRQ ARTICLE ARE MOSTLY *CENTERED* TO    *
*FACILITATE INTERPRETATIONS OF MODEL PARAMETERS.THESE VARIABLES ARE       *
*LABELED WITH A SUFFIX, SUCH AS MCENT, TO DENOTE THE ADJUSTMENT           * *                                                                         *
***************************************************************************


*insheet using ".../REPLICATION_DATA.csv", clear
label variable state "state"
label variable year "year"
label variable stateno "state code"
label variable time "time"
label variable time2 "time squared" 
label variable billsproposed "voter ID bill introductions"
label variable photo "photo ID required to vote"
label variable id_req "any ID required to vote"
label variable election_margin "partisan election margin" 
label variable election_margin_mcent "partisan election margin (grand mean centered)"
label variable pct_gop "% GOP lawmakers"
label variable pct_gop_centered "% GOP lawmakers (centered on 50%)"
label variable gopxmargin_mcent "pct_gop_centered X election_margin_mcent"
label variable battleground "battleground state"
label variable vep_pres "turnout in most recent pres. election"
label variable vep_pres_mcent "turnout in most recent pres. election (grand mean centered)"
label variable battlexvep_mcent "battleground X vep_pres_mcent"
label variable gov_gop "GOP governor"
label variable pct_nonwhite_registrants "% nonwhite voter registrants"
label variable nonwhite_mcent "% nonwhite voter regristrants (grand mean centered)"
label variable nonwhite_absgrowth "% growth nonwhite registrants"
label variable nonwhite_growth_mcent "% growth nonwhite registrants (grand mean centered)"
label variable gov_ideology "government ideology (2011 & 2012 values are extrapolated)"
label variable gov_ideology "government ideology (grand mean centered)"
label variable south "former states of confederacy"
label variable diff_any_id "prop. neighbors with ANY voter ID requirements"
label variable diff_anyid_mcent "prop. neighbors with ANY ID req. (grand mean centered)"
label variable diff_photo_id "prop. neighbors with PHOTO ID req."
label variable diff_photoid_mcent "prop. neighbors with PHOTO ID req. (grand mean centered)"
label variable voterfraud "number of voter fraud cases"
label variable voterfraud_mcent "number of voter fraud cases (grand mean centered)"
label variable hava "HAVA"
label variable biennial "biennial legislative sessions"
label variable scaleid2 "existing voter ID law"  

***************************************************************************
*                                                                         **    MULTILEVEL, OVER-DISPERSED COUNT MODEL USED FOR TABLE 2              *
*  OUTCOME IS NUMBER OF VOTER ID BILL INTRODUCTIONS IN STATE/YEAR         * *                                                                         *
***************************************************************************

gen obs=_n xtmepoisson billsproposed election_margin_mcent pct_gop_centered gopxmargin_mcent ///
battleground vep_pres_mcent battlexvep_mcent gov_gop ///
nonwhite_mcent nonwhite_growth_mcent gov_ideology_mcent south diff_anyid_mcent ///
voterfraud_mcent hava biennial scaleid2 time time2 || stateno: , || obs: ,estimates store PRQ_TABLE2


****************************************************************************                                                                         **					    BINARY DURATION MODEL                             **                  THE ADOPTION OF *ANY* VOTER ID LAW                     *
*                                                                         *
*NOTE: TO FIT BINARY DURATION MODELS, THESE DATA NEED TO BE RE-ORGANIZED  *
*SO THAT ONCE A STATE ADOPTS A LAW, ITS REMOVED FROM THE "RISK SET."      *
*WE ACCOMPLISH THIS RE-ORGANIZATION WITH THE FOLLOWING COMMANDS.          ****************************************************************************            egen rnk1=rank(id_req), unique by(stateno id_req)drop if rnk1>1&id_req==1sort stateno timeegen duration=rank(time), unique by(stateno) label var duration "time linear" gen censor=0replace censor=1 if duration==12gen l_censor=0replace l_censor=1 if id_req==1&time==0replace id_req=. if l_censor==1
lowess id_req duration, gen(lowesst)label var lowesst "time lowess"  *MODEL FOR ANY FORM OF VOTER IDlogit id_req  election_margin_mcent pct_gop_centered gopxmargin_mcent  ///battleground vep_pres_mcent battlexvep_mcent ///gov_gop nonwhite_mcent nonwhite_growth_mcent ///gov_ideology_mcent south diff_anyid_mcent ///voterfraud_mcent hava ///lowesst, cluster(stateno)estimates store PRQ_TABLE3_MODEL1****************************************************************************                                                                         **					    BINARY DURATION MODEL                             **             THE ADOPTION OF *PHOTO BASED* VOTER ID LAW                  *
*                                                                         *
*NOTE: GIVEN THE CHANGE IN THE OUTCOME, WE RE-ORGANIZE THESE DATA AGAIN.  *
*THIS MEANS THE *ORIGINAL DATASET* MUST BE RELOADED TO BEGIN, FOLLOWED    *
*BY THE FOLLOWING COMMANDS TO RE-ORGANIZE THESE DATA.                     *******************************************************************************************************************************************************
*                                                                         **                  RELOAD REPLICATION_DATA.CSV                            *
*                     AND RE-LABEL VARIABLES                              * *                                                                         *
***************************************************************************


insheet using ".../REPLICATION_DATA.csv", clear

label variable state "state"
label variable year "year"
label variable stateno "state code"
label variable time "time"
label variable time2 "time squared" 
label variable billsproposed "voter ID bill introductions"
label variable photo "photo ID required to vote"
label variable id_req "any ID required to vote"
label variable election_margin "partisan election margin" 
label variable election_margin_mcent "partisan election margin (grand mean centered)"
label variable pct_gop "% GOP lawmakers"
label variable pct_gop_centered "% GOP lawmakers (centered on 50%)"
label variable gopxmargin_mcent "pct_gop_centered X election_margin_mcent"
label variable battleground "battleground state"
label variable vep_pres "turnout in most recent pres. election"
label variable vep_pres_mcent "turnout in most recent pres. election (grand mean centered)"
label variable battlexvep_mcent "battleground X vep_pres_mcent"
label variable gov_gop "GOP governor"
label variable pct_nonwhite_registrants "% nonwhite voter registrants"
label variable nonwhite_mcent "% nonwhite voter regristrants (grand mean centered)"
label variable nonwhite_absgrowth "% growth nonwhite registrants"
label variable nonwhite_growth_mcent "% growth nonwhite registrants (grand mean centered)"
label variable gov_ideology "government ideology (2011 & 2012 values are extrapolated)"
label variable gov_ideology "government ideology (grand mean centered)"
label variable south "former states of confederacy"
label variable diff_any_id "prop. neighbors with ANY voter ID requirements"
label variable diff_anyid_mcent "prop. neighbors with ANY ID req. (grand mean centered)"
label variable diff_photo_id "prop. neighbors with PHOTO ID req."
label variable diff_photoid_mcent "prop. neighbors with PHOTO ID req. (grand mean centered)"
label variable voterfraud "number of voter fraud cases"
label variable voterfraud_mcent "number of voter fraud cases (grand mean centered)"
label variable hava "HAVA"
label variable biennial "biennial legislative sessions"
label variable scaleid2 "existing voter ID law" egen rank=rank(photo), unique by(stateno photo)drop if rank>1&photo==1sort stateno timeegen duration=rank(time), unique by(stateno) label var duration "time linear" gen censor=0replace censor=1 if duration==12gen l_censor=0replace l_censor=1 if photo==1&time==0replace photo=. if l_censor==1 lowess photo duration, gen(lowesst) label var lowesst "time lowess" *MODEL FOR PHOTO-BASED FORM OF VOTER IDlogit photo election_margin_mcent pct_gop_centered gopxmargin_mcent  ///battleground vep_pres_mcent battlexvep_mcent ///gov_gop nonwhite_mcent nonwhite_growth_mcent ///gov_ideology_mcent south diff_photoid_mcent ///voterfraud_mcent hava ///lowesst, cluster(stateno)estimates store PRQ_TABLE3_MODEL2


***************************************************************************
*                                                                         **                 DISPLAY RESULTS FROM ALL MODELS                         * *                                                                         *
***************************************************************************estout PRQ_TABLE2, cells(b(star fmt(%9.4f)) /// se(par)) starlevels(+ 0.10 * 0.05 ** 0.01) stats(N aic bic, /// star(ll) fmt(%9.0g %9.3f)) varwidth(30) modelwidth(10) /// title("Multilevel, over-dispersed Poisson") /// note("Note: DV = # of voter ID bill introductions") /// varlabels(_cons constant) /// label legend style(smcl) 
 
 estout PRQ_TABLE3_MODEL1 PRQ_TABLE3_MODEL2, cells(b(star fmt(%9.4f)) /// se(par)) starlevels(+ 0.10 * 0.05 ** 0.01) stats(N aic bic, /// star(ll) fmt(%9.0g %9.3f)) varwidth(30) modelwidth(10) /// title("Duration models") /// note("Note: DV = 1 for state adoption of any (M1) or photo (M2) voter ID, 0 otherwise ") /// varlabels(_cons constant) /// label legend style(smcl) 