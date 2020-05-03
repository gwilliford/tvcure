
a <- unique(c(agiss$bnames, agiss$gnames))
a <- a[c(1:5, 7:14)]
a <- c(a, "igosum")
b <- icow_part_cyr[, a]
apply(b, 2, function(x) sum(is.na(x)))
c <- b[!is.na(b$lagdee), ]
apply(c, 2, function(x) sum(is.na(x)))

d <- c[!is.na(c$ldefense),]
apply(d, 2, function(x) sum(is.na(x)))
e <- c[!is.na(c$igosum),]
apply(e, 2, function(x) sum(is.na(x)))


Only sizeable missing values are ldefense and igosum


all <- tvcure(Surv(agiss_start, agiss_stop, agissterm) ~ lagdee + 
                  recmidwt + recfatwt + recnowt + recyeswt,
                #lag_pch_gdp_min,
                cureform =  ~ lagdee +
                  icowsal + riveriss + mariss +
                  demdy + autdy +
                  lcaprat + contdir + ldefense + igosum, 
                data = icow_part_cyr, 
                var = F, nboot = 100, brglm = T); summary(all) #2001/169


noigo <- tvcure(Surv(agiss_start, agiss_stop, agissterm) ~ lagdee + 
                  recmidwt + recfatwt + recnowt + recyeswt,
                #lag_pch_gdp_min,
                cureform =  ~ lagdee +
                  icowsal + riveriss + mariss +
                  demdy + autdy +
                  lcaprat + ldefense + contdir, 
                data = icow_part_cyr, 
                var = F, nboot = 100, brglm = T); summary(noigo) #2749/195
noally <- tvcure(Surv(agiss_start, agiss_stop, agissterm) ~ lagdee + 
                   recmidwt + recfatwt + recnowt + recyeswt,
                 #lag_pch_gdp_min,
                 cureform =  ~ lagdee +
                   icowsal + riveriss + mariss +
                   demdy + autdy +
                   lcaprat + contdir + igosum, 
                 data = icow_part_cyr, 
                 var = F, nboot = 100, brglm = T); summary(noally) #2997/243
noallyigo <- tvcure(Surv(agiss_start, agiss_stop, agissterm) ~ lagdee + 
                   recmidwt + recfatwt + recnowt + recyeswt,
                 #lag_pch_gdp_min,
                 cureform =  ~ lagdee +
                   icowsal + riveriss + mariss +
                   demdy + autdy +
                   lcaprat + contdir, 
                 data = icow_part_cyr, 
                 var = T, nboot = 100, brglm = T); summary(noallyigo) #5528/340
f <- prediction3(noallyigo, "lagdee", seq(-30, -15, 5), "spop")
plot(f)
g <- prediction3(noallyigo, "lagdee", c(-30, -15), "spop")
plot(g)
h <- prediction3(noallyigo, "lagdee", seq(-30, -15, 5), "uncureprob")
plot(h)
i <- prediction3(noallyigo, "lagdee", c(-30, -15), "suncure")
plot(i)


noallyigo2 <- tvcure(Surv(clstart, clstop, agissterm) ~ lagdee + 
                      recmidwt + recfatwt + recnowt + recyeswt,
                    #lag_pch_gdp_min,
                    cureform =  ~ lagdee +
                      icowsal + riveriss + mariss +
                      demdy + autdy +
                      lcaprat + contdir, 
                    data = icow_part_cyr, 
                    var = T, nboot = 100, brglm = T); summary(noallyigo) #5528/340
f <- prediction3(noallyigo2, "lagdee", c(-30, -15), "spop")
plot(f)
g <- prediction3(noallyigo2, "lagdee", c(-30, -15), "spop")
plot(g)
h <- prediction3(noallyigo2, "lagdee", seq(-30, -15, 5), "uncureprob")
plot(h)
i <- prediction3(noallyigo2, "lagdee", c(-30, -15), "suncure")
plot(i)

coll = icow_part_cyr %>%
  group_by(claimdy) %>% summarize(
    max(cumatt)
  )
sum(coll$`max(cumatt)`)

g <- prediction3(noallyigo2, "lagdee", c(-30, -15), "spop")
plot(g)


340/5528
j <- prediction3(noallyigo2, "lagdee", c(-30, -15), "spop", internals = T)
index <- j$spm[[1]][, "Time"] == 25
mean(j$spm[[1]][index, 1])
mean(j$spm[[2]][index, 1])
mean(j$spm[[1]][index, 1]) - mean(j$spm[[2]][index, 1])
j$splot <- j$splot + geom_vline(aes(xintercept = 25))
j$splot

### Create function w/in prediction3 to calculate mean prediction for each time point and difference (diff requires abs value)
### Within plotting function, allow changes to x axis

index2 <- j$spm[[1]][, "Time"] == 50
mean(j$spm[[1]][index2, 1]) - mean(j$spm[[2]][index2, 1])

100 - 96.12

index3 <- j$spm[[1]][, "Time"] == 100
mean(j$spm[[1]][index3, 1]) - mean(j$spm[[2]][index3, 1])
