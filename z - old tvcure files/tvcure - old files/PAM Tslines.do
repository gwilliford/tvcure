use "C:\Users\gwill\Dropbox\Research\Dissertation\PAM Data\Implementation of Individual Provisions - Figure 3.dta", clear

tsset year_count

foreach v of varlist cease_cum  demob_cum  disarm_cum   milrfm_cum   pargrp_cum   polrfm_cum  reint_cum  with_cum powtran_cum decen_cum exerefm_cum indmin_cum terpow_cum{
	tsline `v', name(`v', replace) nodraw yscale(range(0 100))
}
graph combine cease_cum demob_cum  disarm_cum   milrfm_cum   pargrp_cum   polrfm_cum  reint_cum  with_cum powtran_cum decen_cum exerefm_cum indmin_cum terpow_cum
