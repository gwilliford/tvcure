README file - contains information on all datasets used for replicating analysis in “To Revoke or Not Revoke?: The Political Determinants of Executive Order Longevity”

	1.	eo_original_list.csv - This is a dataset with the executive order as the unit of observation. It contains information on every executive order issued between 1937 and 2013.
	2.	eo_original_list.do - This do file contains commands on how to expand the eo_original_list.csv file into a dataset with executive order-year as the unit of observation, required for survival analysis. This file also shows how additional variables are generated.
	3.	codebook_eo_original_list.pdf - This document contains a description of all variables used in the eo_original_list.csv dataset.
	4.	Figure1.csv - Contains data on the number of executive orders that are amended, superseded, and revoked in various intervals of time.
	5.	Figure1.R - This file contains the R code used to generate Figure 1 in the paper.
	6.	codebook_Figure1.pdf - This document contains a description of all variables used in the Figure1.csv dataset.
	7.	thrower_ajps_replication.do - This file contains the Stata commands used to generate the tables and figures in both the manuscript and the supporting information.
	8.	thrower_ajps_replication_main.dta - This is a dataset with the executive order-year as the unit of analysis. Failure of an executive order is the year it is revoked. 
	9.	codebook_thrower_ajps_replication_main.pdf - This document contains a description of all variables used in the thrower_ajps_replication_main.dta dataset.
	10.	thrower_ajps_replication_tableb5.dta - This is a dataset with the executive order-year as the unit of analysis. Failure of an executive order is the year it is fully (excluding partially) revoked. This data is used to generate Table B5 in the supporting information.
	11.	codebook_thrower_ajps_replication_tableb5.pdf - This document contains a description of all variables used in the thrower_ajps_replication_tableb5 dataset.
	12.	thrower_ajps_replication_tableb6.dta - This is a dataset with the executive order-year as the unit of analysis. Failure of an executive order is the year it is revoked with a rejection of authority. This data is used to generate Table B6 in the supporting information.
	13.	codebook_thrower_ajps_replication_tableb6.pdf - This document contains a description of all variables used in the thrower_ajps_replication_tableb6 dataset.



Data Citations: 

	1. The U.S. National Archives and Records Administration. “Executive Orders Disposition Tables Index.” The Federal Register. url: https://www.archives.gov/federal-register/executive-orders/disposition.html. 
	2. Peters, Gerhard and John T. Woolley. 1999 - 2017. “Executive Orders in the APP Collection,” The American Presidency Project. url: http://www.presidency.ucsb.edu/executive_orders.php.
	3. Peters, Gerhard and John T. Woolley. 1999 - 2017. “The Public Papers of the President,” The American Presidency Project. url: http://presidency.proxied.lsit.ucsb.edu/ws/.
	4. Carroll, Royce, Jeff Lewis, James Lo, Nolan McCarty, Keith Poole, and Howard Rosenthal. “DW-NOMINATE Scores with Bootstrapped Standard Errors.” Voteview. url: http://voteview.com/dwnomin.htm.
	5. ProQuest Historical Database. “The New York Times (1923 - Current file).” url: http://search.proquest.com/publication/45545.
	6. Bureau of Labor Statistics, Department of Labor. “Consumer Price Index.” url: http://www.bls.gov/cpi/.
	7. Gallup Poll. “Presidential Job Approval Center.” url: http://www.gallup.com/poll/124922/presidential-job-approval-center.aspx.
	