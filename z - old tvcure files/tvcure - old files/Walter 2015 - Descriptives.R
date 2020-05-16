library(readstata13)
library(plyr)
walter <- read.dta13("C:/Users/gwill/Dropbox/Data/Peace Agreement Data/Walter 2015 Data/recurring civil war main dataset.dta")
walter <- plyr::rename(walter, replace = c("_st" = "st", "_d"="event","_t"="stop","_t0"="start"))
walter$polity21 <- walter$democpolity - walter$autocpolityl


a <- walter[walter$event == 1 & !is.na(walter$event), ]
a <- a[order(a$countryf, a$year), ]

b <- walter[walter$id == 137, ]

c <- a[, c("year", "countryf", "yearspeace", "polity21", "victory", "writconst", "unpko", "comprehensive")]

# Why do  settlements fail? What leads to failures in implementation?
  # Factionalism
  # Lack of internal control

# Tvcure
library(tvcure)
cl <- makeCluster(4, "SOCK")
registerDoSNOW(cl)
with(walter, cor(cbind(relfrac, ethfrac, lpopl, lgdpl, unpko, writconst, comprehensive, terr, victory), use = "complete.obs"))
m1 <- tvcure(Surv(start, stop, event) ~ ethfrac + terr + victory + intensityln + unpko + democpolityl + autocpolityl, cureform = ~ ethfrac + terr + victory + intensityln + unpko + democpolityl + autocpolityl, data = walter, nboot = 1000)
stopCluster(cl)
coxph(Surv(start, stop, event) ~ relfrac + ethfrac + lpopl + lgdpl + unpko + writconst + comprehensive + terr + victory + democpolityl + autocpolityl, data = walter)
