cox <- coxph(Surv(start, stop, event) ~ lndeaths + tie + battletide + thirdpartycfire + archigosFIRC + stakes + onedem5 + twodem5 + index + cfhist + contiguity + capchange, data = lhr); summary(cox)
tvtable.coxph(cox)

summary(cox)

tvtable(curefull)
tvtable(curefull, varnames = c("Capability Change", "archigosFIRC", "Deaths"))

# order allnames in terms of varnames

# varlist
#
# match(varlabs, allnames)
#
# varnames <- allnames
#
#
# a <- cbind(varlist, index)
# index <- character(length = length(allnames))
#
#
#
#
# varnames[index] <- varlist
curefull <- tvcure(Surv(start, stop, event) ~  capchange + lndeaths +
                     battletide + tie + thirdpartycfire + stakes +
                     twodem5 + onedem5 + index + cfhist + contiguity,
                   cureform = ~ archigosFIRC + capchange + lndeaths + battletide +
                     tie + thirdpartycfire + stakes + onedem5 + twodem5 + index +
                     cfhist + contiguity,
                   data = lhr, var = T, nboot = 100, brglm = T); summary(curefull)

varlist = )

tvtable(curefull)

tvtable(curefull, varlist = list("Capability Change" = "capchange",
                       "Foreign Imposed Regime Change" = "archigosFIRC",
                       "Deaths" = "lndeaths",
                       "index"))


tvtable(curefull, format = "long", varlist = list("Capability Change" = "capchange",
                                 "Foreign Imposed Regime Change" = "archigosFIRC",
                                 "Deaths" = "lndeaths",
                                 "index"))

tvtable.coxph(cox)
tvtable.coxph(cox, format = "long")
tvtable.coxph(cox, varlist = list("Capability Change" = "capchange",
                                                   "Foreign Imposed Regime Change" = "archigosFIRC",
                                                   "Deaths" = "lndeaths",
                                                   "index"))
a <- tvtable.coxph(cox, format = "long", varlist = list("Capability Change" = "capchange",
                                                  "Foreign Imposed Regime Change" = "archigosFIRC",
                                                  "Deaths" = "lndeaths",
                                                  "index"))
b <- xtable(a)
print(b, include.rownames = F)

d <- tvtable(curefull, format = "long", varlist = list("Capability Change" = "capchange",
                                                  "Foreign Imposed Regime Change" = "archigosFIRC",
                                                  "Deaths" = "lndeaths",
                                                  "index"))
e <- print(xtable(d), include.rownames = F)



f <- print(xtable(d), include.rownames = F, booktabs = T)


tvtable.coxph(cox, varlist = list("Capability Change" = "capchange",
                                  "Foreign Imposed Regime Change" = "archigosFIRC",
                                  "Deaths" = "lndeaths",
                                  "index"))








cox <- coxph(Surv(start, stop, event) ~ lndeaths + tie + battletide + twodem5 + index + cfhist + contiguity + capchange, data = lhr); summary(cox)
curepart <- tvcure(Surv(start, stop, event) ~  capchange + index +
                     stakes + cfhist + contiguity,
                   cureform = ~ archigosFIRC + capchange + lndeaths + battletide +
                     tie + thirdpartycfire + stakes + onedem5 + twodem5 + index +
                     cfhist + contiguity,
                   data = lhr, var = T, nboot = 30, brglm = T); summary(curepart)
tvtable.combine(c("cox", "curepart"))
