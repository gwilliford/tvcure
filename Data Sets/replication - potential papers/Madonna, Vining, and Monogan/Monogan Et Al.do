*load original data
use circuit.dta, clear
sort nominee_Cong_ID loc_year loc_month
bys nominee_Cong_ID: gen trend = 1+[_n-1]
stset trend, id(nominee_Cong_ID) failure(confirmed2)

*import updated R data
import delimited "C:\Users\gwill_000\Dropbox\Methods Notes\Models - Duration\Cure Models Paper\replication - potential papers\Madonna, Vining, and Monogan\circuitnew.csv", clear
drop v1
sort nominee_cong_id loc_year loc_month
bys nominee_cong_id: gen trend = 1+[_n-1]
stset trend, id(nominee_cong_id) failure(confirmed2)

*graph
sts graph
*
stsplit, at(failures) riskset(interval)

stcox exog_event fil_distance presapp scsentences aba2 min2 gender renom sterm nonpres pending_noms midtermyear_unifsen bork presmajmed oppsen key, nohr
logit _d exog_event fil_distance presapp scsentences aba2 min2 gender renom sterm nonpres pending_noms midtermyear_unifsen bork presmajmed oppsen key

poisson _d exog_event fil_distance presapp scsentences aba2 min2 gender renom sterm nonpres pending_noms midtermyear_unifsen bork presmajmed oppsen key ibn.interval
zipcv _d exog_event fil_distance presapp scsentences aba2 min2 gender renom sterm nonpres pending_noms midtermyear_unifsen bork presmajmed oppsen key ibn.interval, inflate(exog_event fil_distance presapp scsentences aba2 min2 gender renom sterm nonpres pending_noms midtermyear_unifsen bork presmajmed oppsen key)

Mongan Table 2:

Midterm neg sig
ABA pos sig
Pending Pos sig


***important
import delimited "C:\Users\gwill_000\Dropbox\Methods Notes\Models - Duration\Cure Models Paper\replication - potential papers\Madonna, Vining, and Monogan\importantnew.csv", clear 
sort proposal_id loc_year loc_month
bys proposal_id: gen trend = 1+[_n-1]
stset trend, id(proposal_id) failure(passed2)
sts graph
stsplit, at(failures) riskset(interval)
*cox
stcox scsentences exog_event fil_distance presapp sterm midtermyear_unifsen agendasize presmajmed oppsen  key  , nohr
*poisson
poisson _d scsentences exog_event fil_distance presapp sterm midtermyear_unifsen agendasize presmajmed oppsen  key ibn.interval
zipcv _d scsentences exog_event fil_distance presapp sterm midtermyear_unifsen agendasize presmajmed oppsen  key ibn.interval, inflate(scsentences exog_event fil_distance presapp sterm midtermyear_unifsen agendasize presmajmed oppsen  key)
