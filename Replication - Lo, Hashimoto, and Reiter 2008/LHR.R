# Setup
library(readstata13)
library(plyr)
library(dplyr)
library(coxphf)
cl <- makeCluster(3, "SOCK")
registerDoSNOW(cl)
options(scipen = 999)

# Load data
lhr<-read.dta13("C:/Users/gwill/Dropbox/Methods Notes/Survival Analysis/Cure Models Paper/Replication - LHR 2008/lhrIOOct08replication.dta")
lhr$start<-lhr[,"_t0"]
lhr$stop<-lhr[,"_t"]
lhr$event<-lhr[,"_d"]
#lhr<-lhr[,c("archigosFIRC", "archigosFIRClnt", "capchange", "battletide", "thirdpartycfire", "index", "onedem5", "twodem5", "twodem5lnt", "tie", "lndeaths", "cfhist", "stakes", "contiguity", "contiguitylnt", "LHRcluster", "start", "stop", "event", "newwar", "date1")]
#lhr<-na.omit(lhr)

# Set up parallel processing

# Models
tvcure.full.standard <- tvcure(Surv(start, stop, event) ~ archigosFIRC,
                      cureform = ~ archigosFIRC, data = lhr,
                      model = "ph", link = "probit", nboot = 1000)

                      + capchange
                      + battletide + thirdpartycfire + index + onedem5 + twodem5
                      + tie + lndeaths + cfhist + stakes + contiguity,

tvcure.full.flogit <- tvcure(Surv(start, stop, event) ~ archigosFIRC + capchange +
                        battletide + thirdpartycfire + index + onedem5 + twodem5
                      + tie + lndeaths + cfhist + stakes + contiguity,
                      cureform = ~ archigosFIRC + capchange
                      + battletide + thirdpartycfire + index + onedem5 + twodem5
                      + tie + lndeaths + cfhist + stakes + contiguity,
                      data = lhr,
                      model = "ph",
                      firthlogit = T)
tvcure.institutions.flogit <- tvcure(Surv(start, stop, event) ~ archigosFIRC + capchange +
                               battletide + thirdpartycfire + formal + withdraw +
                               dmz + ac + pk + ext_inv + internal + detail +
                               info + disp_res +index + onedem5 + twodem5 + tie +
                               lndeaths + cfhist + stakes + contiguity,
                             cureform = ~ archigosFIRC + capchange +
                               battletide + thirdpartycfire + formal + withdraw +
                               dmz + ac + pk + ext_inv + internal + detail +
                               info + disp_res +index + onedem5 + twodem5 + tie +
                               lndeaths + cfhist + stakes + contiguity,
                             data = lhr,
                             model = "ph",
                             firthlogit = T)
