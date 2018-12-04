library(readstata13)
library(plyr)

rfds <- read.dta13("C:/Users/gwill/Dropbox/Research/Peace Settlement Conceptulization Paper/Replication Analysis/Replication - Ruloff and Findley 2016/DS2006repl.dta")

tvcure(Surv(duration, peacend) ~ wartype + wardur + logcost + factnum + strongUN + milout + idev1 + isxp2 + lpop + mtnest + decade + pkodur, cureform = ~ wartype + wardur + logcost + factnum + strongUN + milout + idev1 + isxp2 + lpop + mtnest + decade + pkodur, data = rfds, var = F)

wartype logcost factnum strongAll milout idev1 isxp2 fragmentation wardur lpop mtnest decade, vce(cluster clust2) nolog