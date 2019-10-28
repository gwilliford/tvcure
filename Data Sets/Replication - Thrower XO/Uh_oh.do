***Set up
cd "C:/Users/gwill_000/Dropbox/Methods Notes/Survival Analysis/Cure Models Paper/Replication - LHR" 
use "thrower_ajps_replication_main_stata13.dta", clear
set seed 3211991

stset years_revoke, failure(time_revoked==1) 
stset current_year, failure(time_revoked==1) 
stset current_year, failure(time_revoked==1) id(id)

***Kaplan-Meier
sts graph, ci ylabel(, nogrid angle(verticle)) ymtick(, nogrid) xtitle() xlabel(, nogrid) title("") legend(off) scheme(s1mono)
graph export "xokm.eps", replace

stsplit, at(failures) riskset(interval)
drop if interval==.
la var interval "Risk Interval"


stcox divdgovt fds nyt public_mention war inflation electyr admchg endterm trend37 Truman IKE JFK LBJ Nixon Ford Carter Reagan Bush41 Clinton Bush43 Obama current_divdgovt current_inflation  current_electyr opposing_pres log_eo_total current_admchg current_endterm current_war current_trend37 Truman2 IKE2 JFK2 LBJ2 Nixon2 Ford2 Carter2 Reagan2 Bush412 Clinton2 Bush432 Obama2 if ceremonial==0, nohr robust
stcox divdgovt fds nyt public_mention war inflation electyr admchg endterm trend37 current_divdgovt current_inflation  current_electyr opposing_pres log_eo_total current_admchg current_endterm current_war current_trend37 if ceremonial==0, nohr robust
stcox divdgovt fds nyt public_mention war inflation electyr admchg endterm trend37 Truman IKE JFK LBJ Nixon Ford Carter Reagan Bush41 Clinton Bush43 Obama current_divdgovt current_inflation current_electyr opposing_pres log_eo_total current_admchg current_endterm current_war if ceremonial==0, nohr robust
stcox issuing_pcong_dist fds nyt public_mention war inflation electyr admchg endterm trend37 Truman IKE JFK LBJ Nixon Ford Carter Reagan Bush41 Clinton Bush43 Obama current_pcong_dist current_inflation current_electyr opposing_pres log_eo_total current_admchg current_endterm current_war if ceremonial==0, nohr robust
stcox issuing_pcong_dist fds nyt public_mention war inflation electyr admchg endterm trend37 Truman IKE JFK LBJ Nixon Ford Carter Reagan Bush41 Clinton Bush43 Obama current_pcong_dist current_inflation current_electyr pres_dist log_eo_total current_admchg current_endterm current_war current_trend37 if ceremonial==0, nohr robust
