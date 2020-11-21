setwd("C:/Users/gwill/Dropbox/Research/Dissertation/tvcure/V10 Replication")
library(haven)
library(dplyr)
library(tvcure)
library(matrixStats)
library(xtable)
library(gridExtra)
library(ggpubr)
options(scipen = 999)
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)

##### Load data
lhr <- read_dta("LHRIOOct08replication.dta")
lhr <- rename(lhr, "st" = "_st", "event" = "_d", "stop" = "_t", "start" = "_t0")
lhr$io <- ifelse(lhr$index > 0, 1, 0)
# lhr$lnt <- lhr$stop^2 / 1000
lhr$lnt <- log(lhr$stop)
lhr$capchangelnt <- lhr$capchange * lhr$lnt
lhr$lncapchangelnt <- log(lhr$capchange) * lhr$lnt
lhr$samereg <- ifelse(lhr$twodem5 == 0 & lhr$onedem5 == 0, 1, 0)
lhr$battletidelnt <- lhr$battletide * lhr$lnt
lhr$twoaut5 = ifelse(lhr$twodem5 == 0 & lhr$onedem5 == 0, 1, 0)
lhr$twodem5t = lhr$twodem5 * lhr$stop
lhr$one <- 1
lhr$capchange <- lhr$capchange * 100
lhr$lncapchange <- log(lhr$capchange)
lhr = lhr %>% dplyr::select(start, stop, event, lncapchange, lnt, battletide, thirdpartycfire, index, twoaut5, twodem5, tie, lndeaths, cfhist, stakes, contiguity)
lhr = na.omit(lhr)

cox2 <- coxph(Surv(start, stop, event) ~ lncapchange + lncapchange:lnt + battletide +
                      thirdpartycfire + index + twoaut5 + twodem5 + tie + lndeaths +
                      cfhist + cfhist:lnt + stakes + contiguity,
              data = lhr,
              x = T); summary(cox2)

# lhr = lhr %>%
#         group_by(id) %>%
#         mutate(
#                 lcap1 = lag(cap_1),
#                 lcap2 = lag(cap_2),
#                 dcap1 = cap_1 - lcap1,
#                 dcap2 = cap_2 - lcap2,
#                 pchcap1 = dcap1/lcap1,
#                 pchcap2 = dcap2/lcap2,
#         )
# lhr$lncap1 = ifelse(lhr$cap_1 == 0, NA, log(lhr$cap_1))
# lhr$lncap2 = ifelse(lhr$cap_2 == 0, NA, log(lhr$cap_2))
# lhr$lnd1 = lncap1 - lncap2
# lhr$lnpchcap1 = ifelse(lhr$pchcap1 == 0, NA, log(lhr$pchcap1))
# lhr$lnpchcap2 = ifelse(lhr$pchcap2 == 0, NA, log(lhr$pchcap2))
#                    lnpchcap2 = log(pchcap2)
#
# absolutevalue((d.cap_1 /l.cap_1) - (d.cap_2 / l.cap_2))

##### Notes
# Cure equation - positive coefficients increase probability of failure, negative coefficients decrease probability of failure

##### Descriptive Statistics
a <- lhr %>%
        group_by(id) %>%
        summarize(sum1 = sum(one))


##### Models ---------------------------------------------------------------------------
cox2 <- coxph(Surv(start, stop, event) ~ lncapchange + lncapchange:lnt + battletide +
               thirdpartycfire + index + twoaut5 + twodem5 + tie + lndeaths +
               cfhist + cfhist:lnt + stakes + contiguity,
             data = lhr,
             x = T); summary(cox2)
cure2 <- tvcure(Surv(start, stop, event) ~ lncapchange + lncapchange:lnt +
              thirdpartycfire + index + cfhist + cfhist:lnt + stakes,
            cureform = ~ tie + battletide + lndeaths +
              index + cfhist + twodem5 + twoaut5 + contiguity + stakes,
            data = lhr,
            var = T, nboot = 100,
            brglm = T); summary(cure2)
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)

cure3 <- tvcure(Surv(start, stop, event) ~ lncapchange + lncapchange:lnt +
                        thirdpartycfire + index + cfhist + cfhist:lnt + stakes,
                cureform = ~ tie + battletide + lndeaths +
                        index + cfhist + twodem5 + twoaut5 + contiguity + stakes,
                data = lhr,
                var = T, nboot = 100,
                brglm = T); summary(cure3)


##### ROC Curves
## Obtain the linear predictor
lhr$coxlp  = predict(cox2, type = "lp")
# curelp = predict(cure2$curemod$latency_fit, type = "lp")
lhr$curelp = as.numeric(cure2$X %*% cure2$beta)

library(survivalROC)



cox2_helper <- function(t) {
        survivalROC(entry        = lhr$start,
                    Stime        = lhr$stop,
                    status       = lhr$event,
                    marker       = lhr$coxlp,
                    predict.time = t,
                    method       = "NNE",
                    span = 0.25 * nrow(lhr)^(-0.20))
}
cox2_roc <- data_frame(t = 3650 * c(1,2,3,4,5, 6)) %>%
        mutate(survivalROC = map(t, cox2_helper),
               auc = map_dbl(survivalROC, magrittr::extract2, "AUC"),
               df_survivalROC = map(survivalROC, function(obj) {
                       as_data_frame(obj[c("cut.values","TP","FP")])
               })) %>%
        dplyr::select(-survivalROC) %>%
        unnest() %>%
        arrange(t, FP, TP)
cox2_rocplot = cox2_roc %>%
        ggplot(mapping = aes(x = FP, y = TP)) +
        geom_point() +
        geom_line() +
        geom_label(data = survivalROC_data %>% dplyr::select(t,auc) %>% unique,
                   mapping = aes(label = sprintf("%.3f", auc)), x = 0.5, y = 0.5) +
        facet_wrap( ~ t) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
              legend.key = element_blank(),
              plot.title = element_text(hjust = 0.5),
              strip.background = element_blank())

cure2_helper <- function(t) {
        survivalROC(entry        = lhr$start,
                    Stime        = lhr$stop,
                    status       = lhr$event,
                    marker       = lhr$curelp,
                    predict.time = t,
                    method       = "NNE",
                    span = 0.25 * nrow(lhr)^(-0.20))
}
cure2_roc <- data_frame(t = 365 * c(1,2,3,4,5)) %>%
        mutate(survivalROC = map(t, cure2_helper),
               auc = map_dbl(survivalROC, magrittr::extract2, "AUC"),
               df_survivalROC = map(survivalROC, function(obj) {
                       as_data_frame(obj[c("cut.values","TP","FP")])
               })) %>%
        dplyr::select(-survivalROC) %>%
        unnest() %>%
        arrange(t, FP, TP)
cure2_rocplot = cure2_roc %>%
        ggplot(mapping = aes(x = FP, y = TP)) +
        geom_point() +
        geom_line() +
        geom_label(data = survivalROC_data %>% dplyr::select(t,auc) %>% unique,
                   mapping = aes(label = sprintf("%.3f", auc)), x = 0.5, y = 0.5) +
        facet_wrap( ~ t) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
              legend.key = element_blank(),
              plot.title = element_text(hjust = 0.5),
              strip.background = element_blank())



##### PLOTS --------------------------------------------------------------------
cn <- c("time", "surv", "lower", "upper", "num")
breaks <- seq(0, 90, 20) * 365
labs <- seq(0, 90, 20)

### Baseline hazard Comparison -------------------------------------------
#cox basesurv
coxbase <- survfit(cox2)
coxbasepdat <- as.data.frame(cbind(coxbase$time, coxbase$surv, coxbase$lower, coxbase$upper))
colnames(coxbasepdat) <- cn[-5]
coxbase <- ggplot(mapping = aes(x = time, y = surv), data = coxbasepdat) +
        geom_line() + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
        ylab("Probability of Survival") + xlab("Time in years") +
        scale_x_continuous(breaks = breaks, labels = labs) +
        theme(panel.border = element_rect(colour = "black", fill = NA)) + ylim(0, 1)


#cure basesurv
curebase <- prediction4(cure2, "tie", c(0, 1), type = "basesurv")
curebase <- curebase + scale_x_continuous(breaks = breaks, labels = labs) +
        xlab("Time in years") + ylab("Probability of Survival") +
        theme(axis.title.y=element_blank())

# combine
base <- grid.arrange(coxbase, curebase + theme(axis.title.y=element_blank()), ncol = 2); base
ggsave("C:/Users/gwill/Dropbox/Research/Dissertation/Manuscript/chapter2/img/base.png",
       plot = base, width = 5.5, height = 3, units = "in")


### Tie comparison ---------------------------------------------------------------
legendtitle <- "Tie"
vals <- c(0, 1)

# Cox plot
cox2 <- coxph(Surv(start, stop, event) ~ lncapchange + lncapchangelnt + battletide +
                      thirdpartycfire + index + twoaut5 + twodem5 + tie + lndeaths +
                      cfhist + cfhistlnt + stakes + contiguity,
              data = lhr,
              x = T); summary(cox2)

newdata1 <- apply(cox2$x, 2, median, na.rm = T)
# newdata1$lnt <- 9.08
newdata1 <- as.data.frame(rbind(newdata1, newdata1))
newdata1[2, "tie"] <- 1
pcoxtie1 <- survfit(cox2, newdata = newdata1[1, ], se.fit = T)
pcoxtie2 <- survfit(cox2, newdata = newdata1[2, ], se.fit = T)

pcoxtie1pdat <- cbind(as.data.frame(cbind(pcoxtie1$time, pcoxtie1$surv, pcoxtie1$lower, pcoxtie1$upper)), 1)
pcoxtie2pdat <- cbind(as.data.frame(cbind(pcoxtie2$time, pcoxtie2$surv, pcoxtie2$lower, pcoxtie2$upper)), 2)
colnames(pcoxtie1pdat) <- cn
colnames(pcoxtie2pdat) <- cn
pcoxtiepdat <- rbind(pcoxtie1pdat, pcoxtie2pdat)


pcoxtieplot <- ggplot(mapping = aes(x = time, y = surv, col = as.factor(num), linetype = as.factor(num)), data = pcoxtiepdat) +
        geom_line() + geom_ribbon(aes(ymin = lower, ymax = upper,
                                      col = as.factor(num),
                                      linetype = as.factor(num),
                                      fill = as.factor(num)), alpha = 0.2) +
        labs(fill = legendtitle, linetype = legendtitle, col = legendtitle) +
        scale_linetype_discrete(labels = vals)  +
        scale_color_discrete(labels = vals) +
        scale_fill_discrete(labels = vals) +
        ylab("Probability of Survival") + xlab("Time in years") +
        scale_x_continuous(breaks = breaks, labels = labs) +
        theme(panel.border = element_rect(colour = "black", fill = NA),
                      legend.position = "bottom")

# Cureplot
tpred1 <- prediction4(cure2, "tie", c(0, 1), type = "uncureprob")
tplot1 <- tpred1 + xlab("Tie")
tpred2 <- prediction4(cure2, "tie", c(0, 1), type = "spop", legendtitle = "Tie")
tplot2 <- tpred2 + xlab("Time in years") + theme(legend.position = "none") +
        ylab("Probability of Survival") + xlab("Time in years") +
        scale_x_continuous(breaks = breaks, labels = labs)

# combine
legend <- get_legend(pcoxtieplot)
pcoxtieplot <- pcoxtieplot + theme(legend.position = "none")
# b <- grid.arrange(
#         grobs = list(legend, pcoxtieplot, tplot1, tplot2),
#         ncol = 2, nrow = 2,
#         # widths = c(0.1, 1, 1),
#         layout_matrix = rbind(c(1, 2),
#                               c(3, 4))
# )
b <- grid.arrange(
        grobs = list(pcoxtieplot, tplot2, tplot1, legend),
        ncol = 3, nrow = 2, heights = c(1, 0.1), layout_matrix = rbind(c(1, 2, 3),
                                                  c(4,4,4))
)
ggsave("C:/Users/gwill/Dropbox/Research/Dissertation/Manuscript/chapter2/img/tie.png",
       plot = b, width = 6, height = 2.5, units = "in")


### Deaths comparison
legendtitle <- "ln(Battle Deaths)"
deathmin <- round(min(lhr$lndeaths, na.rm = T), 1)
deathmax <- round(max(lhr$lndeaths, na.rm = T), 1)
vals <- c(deathmin, deathmax)

# Cox plot
newdata2 <- apply(cox2$x, 2, median, na.rm = T)
newdata2 <- as.data.frame(rbind(newdata2, newdata2))
newdata2[1, "lndeaths"] <- deathmin
newdata2[2, "lndeaths"] <- deathmax
pcoxdeaths1 <- survfit(cox2, newdata = newdata2[1, ], se.fit = T)
pcoxdeaths2 <- survfit(cox2, newdata = newdata2[2, ], se.fit = T)

pcoxdeaths1pdat <- cbind(as.data.frame(cbind(pcoxdeaths1$time, pcoxdeaths1$surv, pcoxdeaths1$lower, pcoxdeaths1$upper)), 1)
pcoxdeaths2pdat <- cbind(as.data.frame(cbind(pcoxdeaths2$time, pcoxdeaths2$surv, pcoxdeaths2$lower, pcoxdeaths2$upper)), 2)
colnames(pcoxdeaths1pdat) <- cn
colnames(pcoxdeaths2pdat) <- cn
pcoxdeathspdat <- rbind(pcoxdeaths1pdat, pcoxdeaths2pdat)

pcoxdeathsplot <- ggplot(mapping = aes(x = time, y = surv, col = as.factor(num),
                                       linetype = as.factor(num)), data = pcoxdeathspdat) +
        geom_line() +
        geom_ribbon(aes(ymin = lower, ymax = upper,
                        col = as.factor(num),
                        linetype = as.factor(num),
                        fill = as.factor(num)),
                    alpha = 0.2) +
        labs(fill = legendtitle, linetype = legendtitle, col = legendtitle) +
        scale_linetype_discrete(labels = vals)  +
        scale_color_discrete(labels = vals) +
        scale_fill_discrete(labels = vals) +
        ylab("Probability of Survival") + xlab("Time in years") +
        scale_x_continuous(breaks = breaks, labels = labs) +
        theme(panel.border = element_rect(colour = "black", fill = NA), legend.position = "bottom")

# Cureplots
dpred1 <- prediction4(cure2, "lndeaths", c(deathmin, deathmax), type = "uncureprob")
dplot1 <- dpred1 + xlab("ln(Battle Deaths)")
dpred2 <- prediction4(cure2, "lndeaths", c(deathmin, deathmax), type = "spop",
                      legendtitle = "ln(Battle Deaths)")
dplot2 <- dpred2 + xlab("Time in years") + theme(legend.position = "none") +
        ylab("Probability of Survival") + xlab("Time in years") +
        scale_x_continuous(breaks = breaks, labels = labs)

# combine
legend2 <- get_legend(pcoxdeathsplot)
pcoxdeathsplot <- pcoxdeathsplot + theme(legend.position = "none")
# d <- grid.arrange(
#         grobs = list(legend, pcoxdeathsplot, tplot1, tplot2),
#         ncol = 2, nrow = 2,
#         # widths = c(0.1, 1, 1),
#         layout_matrix = rbind(c(1, 2),
#                               c(3, 4))
# )
d <- grid.arrange(
        grobs = list(pcoxdeathsplot, dplot2, dplot1, legend2),
        ncol = 3, nrow = 2, heights = c(1, 0.1), layout_matrix = rbind(c(1, 2, 3),
                                                                       c(4,4,4))
)

ggsave("C:/Users/gwill/Dropbox/Research/Dissertation/Manuscript/chapter2/img/deaths.png",
       plot = d, width = 6, height = 2.5, units = "in")



### Battletide
# bpred1 <- prediction4(cure2, "battletide", c(0, 1), type = "uncureprob")
# bplot1 <- bpred1 + xlab("Battle Consistency")
# bpred2 <- prediction4(cure2, "battletide", c(0, 1), type = "spop", legendtitle = "Battle Consistency")
# bplot2 <- bpred2 + xlab("Time in years") + theme(legend.position = "none") +
#         ylab("Probability of Survival") + xlab("Time in years") +
#         scale_x_continuous(breaks = breaks, labels = labs)


#### Low risk cases 1st row - high risk cases second column
# caplo <- min(lhr$capchange, na.rm = T)
# caphi <- max(lhr$capchange, na.rm = T)
# lncaplo <- min(lhr$lncapchange, na.rm = T)
# lncaphi <- max(lhr$lncapchange, na.rm = T)
# sim <- apply(cox2$x, 2, median, na.rm = T)
# sim <- rbind(sim, sim)
# sim[,"lncapchange"] <- c(-11.7, 1.5)
# # sim[,"lncapchangelnt"] <- c(lncaplo * log(21), lncaphi * log(21))
# # sim[,"lncapchange:lnt"] <- c(lncaplo* log(21), lncaphi * log(21))
# sim[,"battletide"] <- c(1, 0)
# sim[,"thirdpartycfire"] <- c(1, 0)
# sim[,"index"] <- c(12, 0)
# sim[,"lndeaths"] <- c(15.6, 5.3)
# sim[,"cfhist"] <- c(0, 1.6)
# sim[,"cfhistlnt"] <- c(0, 1.6 * log(21))
# sim[, "stakes"] <- c(0, 1)
# sim[,"contiguity"] <- c(0, 1)
# sim[, "twodem5"] <- c(1, 0)
# sim[,"twoaut5"] <- c(0, 1)
# sim[,"tie"] <- c(0, 1)
# sim <- sim[, ncol(sim) - 1: ncol(sim)]
# sim <- cbind(sim, c(0, 1))
# colnames(sim)[ncol(sim)] <- "Intercept"
# sim <- cbind(sim, c(34, 34))
# colnames(sim)[ncol(sim)] <- "lnt"
#
#
# simlo <- sim[1, ]
# simlo <- rbind(simlo, simlo)
# simlo[2, "capchange"] <- lncaphi
# simhi <- sim[2, ]
# simhi <- rbind(simhi, simhi)
# simhi[1, "capchange"] <- lncaplo
#
# simloX <- simlo[, cure2$bnames]
# a <- prediction4(cure2, "lndeaths", newX = simlo[, 1])
#
#
# sim <- apply(cox2$x, 2, median, na.rm = T)
# sim <- rbind(sim, sim)
# # caplo <- min(lhr$capchange, na.rm = T)
# # caphi <- max(lhr$capchange, na.rm = T)
# caplo <- quantile(lhr$capchange, 0.05, na.rm = T)
# caphi <- quantile(lhr$capchange, 0.95, na.rm = T)
# sim[1, "capchange"] <- caplo
# sim[2, "capchange"] <- caphi
# sim[1, "capchange:lnt"] <- caplo * log(32)
# sim[2, "capchange:lnt"] <- caphi * log(32)
# sim[, "lnt:cfhist"] <- sim[, "cfhist"] * log(32)
# sim <- cbind(sim, 1)
# colnames(sim)[ncol(sim)] <- "Intercept"
#
# simx <- sim[, cure2$bnames]
# simz <- sim[, cure2$gnames]
# a <- prediction4(cure2, newX = simx, newZ = simz, type = "suncure")
#
#
#
#
# sim <- apply(cox2$x, 2, median, na.rm = T)
# sim <- rbind(sim, sim)
# # sim[,"lncapchange"] <- c(-11.7, 1.5)
# # sim[,"lncapchangelnt"] <- c(lncaplo * log(21), lncaphi * log(21))
# # sim[,"lncapchange:lnt"] <- c(lncaplo* log(21), lncaphi * log(21))
# sim[,"battletide"] <- c(1, 0)
# sim[,"thirdpartycfire"] <- c(1, 0)
# sim[,"index"] <- c(12, 0)
# sim[,"lndeaths"] <- c(15.6, 5.3)
# sim[,"cfhist"] <- c(0, 1.6)
# sim[,"lnt:cfhist"] <- c(0, 1.6 * log(32))
# sim[, "stakes"] <- c(0, 1)
# sim[,"contiguity"] <- c(0, 1)
# sim[, "twodem5"] <- c(1, 0)
# sim[,"twoaut5"] <- c(0, 1)
# sim[,"tie"] <- c(0, 1)
# sim[, "capchange"] <- c(caplo, caphi)
# sim[, "capchange:lnt"] <- c(caplo * log(32), caphi * log(32))
# sim <- cbind(sim, 1)
# colnames(sim)[ncol(sim)] <- "Intercept"
#
# simlo <- rbind(sim[1, ], sim[1, ])
# simlo[2, "capchange"] <- caphi
# simlo[2, "capchange:lnt"] <- caphi * log(32)
# simlox <- simlo[, cure2$bnames]
# simloz <- simlo[, cure2$gnames]
# b <- prediction4(cure2, newX = simlox, newZ = simloz, type = "suncure")
#
# simhi <- rbind(sim[2, ], sim[2, ])
# simhi[1, "capchange"] <- caplo
# simhi[1, "capchange:lnt"] <- caplo * log(32)
# simhi[2, "capchange:lnt"] <- caphi * log(32)
# simhix <- simhi[, cure2$bnames]
# simhiz <- simhi[, cure2$gnames]
# d <- prediction4(cure2, newX = simhix, newZ = simhiz, type = "spop")

##### Tables --------------------------------------------------------------------
vl <- list("Battle Deaths" = "lndeaths",
     "Tie" = "tie",
     "Battle Consistency" = "battletide",
     "Third Party Intervention" = "thirdpartycfire",
     # "Capability Change" = "capchange",
     # "Capability Change $\\times \\ln(t)$" = "capchangelnt",
     "Capability Change" = "lncapchange",
     "Capability Change $\\times \\ln(t)$" = "lncapchange:lnt",
     "Existential Stakes" = "stakes",
     "Agreement Strength" = "index",
     # "Foreign-Imposed Regime Change" = "archigosFIRC",
     # "Mixed Regime Type" = "onedem5",
     "Joint Democracy" = "twodem5",
     "Joint Autocracy" = "twoaut5",
     "Conflict History" = "cfhist",
     "Conflict History $\\times \\ln(t)$" = "lnt:cfhist",
     "Contiguity" = "contiguity",
     "Intercept" = "Intercept")
df <- as.data.frame(matrix(nrow = length(vl), ncol = 0))
rownames(df) <- unlist(vl)

coxb <- round(coef(cox2), 2)
coxse <- round(sqrt(diag(cox2$var)), 2)
bz    <- coxb/coxse
bpval <- (1 - pnorm(abs(bz))) * 2
bstar <- gtools::stars.pval(bpval)
coxse <- paste0("(", coxse, ")", bstar)

coxdf <- cbind(coxb, coxse)
coxdf <- as.data.frame(coxdf)
rownames(coxdf) <- names(coxb)
a <- merge(df, coxdf, by = "row.names", all.x = T, sort = F)
w <- a[, 1]
a <- a[, -1]
rownames(a) <- w

c2gamma <- round(as.numeric(cure2$gamma), 2)
c2gse <- round(cure2$g_sd, 2)
gz    <- c2gamma/c2gse
gpval <- (1 - pnorm(abs(gz))) * 2
gstar <- gtools::stars.pval(gpval)
c2gse <- paste0("(", c2gse, ")", gstar)

c2df <- cbind(c2gamma, c2gse)
c2df <- as.data.frame(c2df)
rownames(c2df) <- cure2$gnames
f <- merge(a, c2df, by = "row.names", all.x = T, sort = F)
g <- f[, 1]
f <- f[, -1]
rownames(f) <- g


c2beta <- round(as.numeric(cure2$beta), 2)
c2bse <- round(cure2$b_sd, 2)
cz    <- c2beta/c2bse
cpval <- (1 - pnorm(abs(cz))) * 2
cstar <- gtools::stars.pval(cpval)
c2bse <- paste0("(", c2bse, ")", cstar)

c2bdf <- cbind(c2beta, c2bse)
# c2bdf <- cbind(c2bdf, rownames(c2bdf))
c2bdf <- as.data.frame(c2bdf)
rownames(c2bdf) <- cure2$bnames
b <- merge(f, c2bdf, by = "row.names", all.x = T, sort = F)
# b <- b[, -1]
# rownames(b) <- rownames(df)

d <- match(b$Row.names, rownames(df))
e <- b[order(d), ]
e <- e[, -1]
rownames(e) <- rownames(df)
e <- as.matrix(e)
# colnames(e) <- c("")

# clist1 <- as.numeric(e[, 2])
# clist2 <- as.numeric(e[, 4])
# clist3 <- as.numeric(e[, 6])
# slist1 <- as.numeric(e[, 1])
# slist2 <- as.numeric(e[, 3])
# slist3 <- as.numeric(e[, 5])
# # h <- as.vector(rbind(e[, 1], e[, 2]))
# h <- list()
# j <- list()
# k <- list()
# for (i in 1:nrow(e)) {
#         h[i] <- c(e[i, 1], e[i, 2])
#         j[i] <- c(e[i, 3], e[i, 4])
#         k[i] <- c(e[i, 5], e[i, 6])
# }
# j <- rbind(e[, 3], e[, 4])
# k <- rbind(e[, 5], e[, 6])
# h <- ifelse(is.na(h), "", h)
# j <- ifelse(is.na(j), "", h)
# k <- ifelse(is.na(k), "", h)
# for (i in 1:ncol(j)) {
#         j[2, i] <- paste("(", j[2, i], ")", gstar[i])
# }
# for (i in 1:ncol(k)) {
#         k[2, i] <- paste("(", k[2, i], ")", cstar[i])
# }
# for (i in 1:ncol(j)) {
#         j[2, i] <- paste("(", j[2, i], ")", gstar[i])
# }
# for (i in 1:ncol(k)) {
#         k[2, i] <- paste("(", k[2, i], ")", cstar[i])
# }



# m[, 1] <- h
# m[, 2] <- j
# m[, 3] <- k

# rownames(m) <- n
# n <- as.vector(rbind(rownames(e), ""))
# rownames(m) <- n
m <- matrix(ncol = 3, nrow = nrow(e) * 2)
m[, 1] <- as.vector(rbind(e[, 1], e[, 2]))
m[, 2] <- as.vector(rbind(e[, 3], e[, 4]))
m[, 3] <- as.vector(rbind(e[, 5], e[, 6]))

n <- as.vector(rbind(names(vl), " "))
m <- cbind(n, m)
colnames(m) <- c("", "Hazard", "Pr(Failure)", "Hazard")

stargazer(m, summary = F, style = "ajps", digits = 2,
file = 'C:/Users/gwill/Dropbox/Research/Dissertation/Manuscript/chapter2/tab/res.tex',
          rownames = F)




#
# stargazer(cox2, cox2, cox2, coef = list(clist1, clist2, clist3),
#           se = list(slist1, slist2, slist3),
#           order = unlist(vl),
#           covariate.labels = names(vl),
#           style = "apsr",
#           summary = F
#           )
#



t1 <- tvtable(cox2, cure2, varlist = vl, modnames = F)
x1 <- tvtable_xtable(t1, digits = 2)

c2beta <- cure2$beta
names(c2beta) <- cure2$bnames
c2bse <- cure2$b_sd
names(c2bse) <- cure2$bnames
a <- unlist(vl)
order(c2beta, names(unlist(vl))[cure2$bnames])



a <- as.data.frame(rbind(c2beta, c2bse))
b <- rbind(c2gamma, c2gse)
allnames <- vl

arrange(a, allnames)


c2beta <- as.vector(rbind(, cure2$b_sd))
stargazer(c2beta)


                     #caption = "Cox proportional hazards and cure model estimates of ceasefire duration")?
# printer(x1)
# addtorow <- list()
# addtorow$pos <- list(0, 0)
# addtorow$command <- c("& \\multicolumn{1}{c}{Model 1} \\multicolumn{2}{c}{Model 2} \\\\ \\cmidrule(lr){2-2}\\cmidrule(lr){3-4}",
#                       "\\bottomrule \\n \\footnote{\\footnotesize Note: $'***' p \\leq 0.001, '**' p \\leq 0.01, '*' p \\leq 0.5, + \\leq 0.1$}\n"
# )

print(x1,
     booktabs = T,
     sanitize.text.function = identity,
     include.rownames = F,
     include.colnames = F,
     # add.to.row = addtorow,
     file = 'C:/Users/gwill/Dropbox/Research/Dissertation/Manuscript/chapter2/tab/res.tex')


# hline.after = c(-1, 2, nrow(t1) - 2))



#assign a position argument to addtorow
#rws are the row indexes for the row to be colored,
#0 is the row index for longtable argument

#assign corresponding commands to addtorow

