library(readstata13)
library(plyr)

walter<-read.dta13("AOWYReplicationWalter2014.dta")
walter<-rename(walter, replace = c("_st" = "st", "_d"="event","_t"="stop","_t0"="start"))

m1 <- tvcure(Surv(start,stop,event)~ethfrac+lgdpl+lmtnest+instabl+ncontig+terr+comprehensive+intensityln+unpko,cureform=~ethfrac+relfrac+lgdpl+lpopl+lmtnest+instabl+ncontig+victory+terr+comprehensive+intensityln+unpko,emmax=10000,data=walter,nboot=10, firthlogit=T)

m1.nocomp<-coxphf(walter1.surv~ag1+ag2+ag3+ag4+ethfrac+relfrac+lgdpl+lpopl+lmtnest+instabl+ncontig+victory+terr+intensityln+unpko,data=walter1)

"Negative Agreement","Societal Agreement","Political Agreement","Political and Societal Agreement","Ethnic Fractionalization","Religious Fractionalization","GDP","Population","Mountainous Terrain","Political Instability","Noncontiguous Land Mass","Clear Victory","Territorial Conflict","Comprehensive Peace Agreements","Previous War Intensity (ln Battle Deaths)","Peace Keepers in Country")


###Model 2
walter2<-walter[,c('start','stop','event','ag1','ag2','ag3','ag4','ag2all','ag3all','ag1full','ag2full','ag3full','ag4full','ethfrac','relfrac','lgdpl','lpopl','lmtnest','instabl','ncontig','victory','terr','comprehensive','intensityln','unpko','autocpolityl','democpolityl')]
walter2<-na.omit(walter2)
walter2.surv<-Surv(walter2$start,walter2$stop,walter2$event)
m2<-coxphf(walter2.surv~ag1+ag2+ag3+ag4+ethfrac+relfrac+lgdpl+lpopl+lmtnest+instabl+ncontig+autocpolityl+democpolityl+victory+terr+comprehensive+intensityln+unpko,data=walter2+
m2a<-coxphf(walter2.surv~ag1+ag2+ag3+ag4+ethfrac+relfrac+lgdpl+lpopl+lmtnest+instabl+ncontig+autocpolityl+democpolityl+victory+terr+intensityln+unpko,data=walter2)

m2b<-coxphf(walter2.surv~ag1full+ag2full+ag3full+ag4full+ethfrac+relfrac+lgdpl+lpopl+lmtnest+instabl+ncontig+autocpolityl+democpolityl+victory+terr+comprehensive+intensityln+unpko,data=walter2)

###Model 3
walter3<-walter[,c('start','stop','event','ag1','ag2','ag3','ag4','ag2all','ag3all','ethfrac','relfrac','lgdpl','lpopl','lmtnest','instabl','ncontig','victory','terr','comprehensive','intensityln','unpko','writconstl')]
walter3<-na.omit(walter3)
walter3.surv<-Surv(walter3$t0,walter3$t,walter3$d)
m3<-coxphf(walter3.surv~ag1+ag2+ag3+ag4+ethfrac+relfrac+lgdpl+lpopl+lmtnest+instabl+ncontig+writconstl+victory+terr+comprehensive+intensityln+unpko,data=walter3,firth=F)

###Model 4
walter4<-walter[,c('start','stop','event','ag1','ag2','ag3','ag4','ag2all','ag3all','ethfrac','relfrac','lgdpl','lpopl','lmtnest','instabl','ncontig','victory','terr','comprehensive','intensityln','unpko','vhpartl1')]
walter4<-na.omit(walter4)
walter4.surv<-Surv(walter4$t0,walter4$t,walter4$d)
m4<-coxphf(walter4.surv~ag1+ag2+ag3+ag4+ethfrac+relfrac+lgdpl+lpopl+lmtnest+instabl+ncontig+vhpartl1+victory+terr+comprehensive+intensityln+unpko,data=walter4)

###Model 5
walter5<-walter[,c('start','stop','event','ag1','ag2','ag3','ag4','ag2all','ag3all','ethfrac','relfrac','lgdpl','lpopl','lmtnest','instabl','ncontig','victory','terr','comprehensive','intensityln','unpko','fhcompor1')]
walter5<-na.omit(walter5)
walter5.surv<-Surv(walter5$t0,walter5$t,walter5$d)
m5<-coxphf(walter5.surv~ag1+ag2+ag3+ag4+ethfrac+relfrac+lgdpl+lpopl+lmtnest+instabl+ncontig+fhcompor1+victory+terr+comprehensive+intensityln+unpko,data=walter5)

###Model 6
walter6<-walter[,c('start','stop','event','ag1','ag2','ag3','ag4','ag2all','ag3all','ethfrac','relfrac','lgdpl','lpopl','lmtnest','instabl','ncontig','victory','terr','comprehensive','intensityln','unpko','freepressfhlr')]
walter6<-na.omit(walter6)
walter6.surv<-Surv(walter6$t0,walter6$t,walter6$d)
m6<-coxphf(walter6.surv~ag1+ag2+ag3+ag4+ethfrac+relfrac+lgdpl+lpopl+lmtnest+instabl+ncontig+freepressfhlr+victory+terr+comprehensive+intensityln+unpko,data=walter6)
