#Results computed using R 3.1.1.
#The following versions of libraries were used:
#foreign v. 0.8-6
#lme4 v. 1.1-7
#survival v. 2.37-7
#mgcv v. 1.8-0

#clean up
rm(list=ls())
options(digits=4,scipen=4)

#SET WORKING DIRECTORY TO YOUR WORKING FOLDER
#setwd('/Volumes/MONOGAN/presCapital/data/')
setwd("C:/Users/gwill_000/Dropbox/Methods Notes/Models - Duration/Cure Models Paper/replication - potential papers/Madonna, Vining, and Monogan")

#libraries
library(foreign)
library(lme4)
library(survival)
library(mgcv)
library(xtable)
#new
library(survival)

#load data
district<-read.dta("district.dta")
circuit<-read.dta("circuit.dta")
important<-read.dta("important.dta")
routine<-read.dta("routine.dta")

table(district$loc_year)
table(circuit$loc_year)
table(important$loc_year)
table(routine$loc_year)


###NEW DATA TO ADD###
#absolute distance between president and median of majority party
#Common Space NOMINATE, 1st-113th Congresses
#Accessed from voteview.com on 25 January 2016
pres.maj.med<-c(0.0314999967813492,0.0314999967813492,0.870999976992607,0.870999976992607,0.861999988555908,0.861999988555908,0.872999966144562,0.872999966144562,0.825999975204468,0.825999975204468,0.192000031471252,0.192000031471252,0.200000017881393,0.200000017881393,0.393000021576881,0.393000021576881,0.397000014781952,0.397000014781952,0.397000014781952,0.397000014781952,1.00700002908707,1.00700002908707,0.888999998569489,0.888999998569489,0.897000014781952,0.897000014781952,0.130999982357025,0.130999982357025,0.810000002384186,0.810000002384186,0.829999983310699,0.829999983310699,0.825999990105629,0.825999990105629,1.04249997437,1.04249997437,0.355999976396561,0.355999976396561,0.340999960899353,0.340999960899353,1.05149997770786,1.05149997770786,0.0439999997615814,0.0439999997615814)
maj.dist<-as.data.frame(cbind(pres.maj.med,1967:2010))
colnames(maj.dist)[2]<-"loc_year"
district<-merge(x=district,y= maj.dist,by="loc_year")
circuit<-merge(x=circuit,y= maj.dist,by="loc_year")
important<-merge(x=important,y= maj.dist,by="loc_year")

#number of senators from the opposing party
loc_year<-c(1967:2010)
opp.sen<-c(36,36,58,58,54,54,56,56,61,61,38,38,41,41,47,47,46,46,47,47,55,55,55,55,56,56,43,43,52,52,55,55,55,55,50,51,48,48,44,44,51,51,40,41)
sen.control<-as.data.frame(cbind(loc_year,opp.sen))
district<-merge(x=district,y=sen.control,by="loc_year")
circuit<-merge(x=circuit,y=sen.control,by="loc_year")
important<-merge(x=important,y=sen.control,by="loc_year")
routine<-merge(x=routine,y=sen.control,by="loc_year")

#policy agenda size
important.agenda<-as.data.frame(table(important$loc_time));names(important.agenda)<-c("loc_time","agendaSize");important<-merge(x=important,y=important.agenda,by="loc_time")
routine.agenda<-as.data.frame(table(routine$loc_time));names(routine.agenda)<-c("loc_time","agendaSize");routine<-merge(x=routine,y=routine.agenda,by="loc_time")

#number of months an SC nomination took
circuit.months<-cbind(by(data=circuit$sctrend,INDICES=circuit$nominee_Cong_ID,FUN=max),by(data=circuit$nominee_Cong_ID,INDICES=circuit$nominee_Cong_ID,FUN=head,1));colnames(circuit.months)<-c("scmonths","nominee_Cong_ID");circuit<-merge(x=circuit,y=circuit.months,by="nominee_Cong_ID")
district.months<-cbind(by(data=district$sctrend,INDICES=district$nominee_Cong_ID,FUN=max),by(data=district$nominee_Cong_ID,INDICES=district$nominee_Cong_ID,FUN=head,1));colnames(district.months)<-c("scmonths","nominee_Cong_ID");district<-merge(x=district,y=district.months,by="nominee_Cong_ID")
important.months<-cbind(by(data=important$sctrend,INDICES=important$proposal_ID,FUN=max),by(data=important$proposal_ID,INDICES=important$proposal_ID,FUN=head,1));colnames(important.months)<-c("scmonths","proposal_ID");important<-merge(x=important,y=important.months,by="proposal_ID")
routine.months<-cbind(by(data=routine$sctrend,INDICES=routine$proposal_ID,FUN=max),by(data=routine$proposal_ID,INDICES=routine$proposal_ID,FUN=head,1));colnames(routine.months)<-c("scmonths","proposal_ID");routine<-merge(x=routine,y=routine.months,by="proposal_ID")

#indicator for key justice nominated, either chief justice or swing justice being replaced
#chief justice: abe fortas (june-october 1968), warren burger (may-june 1969), william rehnquist (june-september 1986), john roberts (september 2005)
#swing justice (Martin-Quinn scores, accessed 21 January 2016, http://mqscores.berkeley.edu/measures.php): Rehnquist (Replacing Harlan in 1971), Kennedy (Replacing Powell in 1987), Alito (Replacing O'Connor in 2005)
important$key<-as.numeric((196806<=important$loc_time & important$loc_time<=196810) | (196905<=important$loc_time & important$loc_time<=196906) | (198606<=important$loc_time & important$loc_time<=198609) | (important$loc_time==200509) | (197110<=important$loc_time & important$loc_time<=197112) | (198711<=important$loc_time & important$loc_time<=198802) | (200511<=important$loc_time & important$loc_time<=200601))
routine$key<-as.numeric((196806<=routine$loc_time & routine$loc_time<=196810) | (196905<=routine$loc_time & routine$loc_time<=196906) | (198606<=routine$loc_time & routine$loc_time<=198609) | (routine$loc_time==200509) | (197110<=routine$loc_time & routine$loc_time<=197112) | (198711<=routine$loc_time & routine$loc_time<=198802) | (200511<=routine$loc_time & routine$loc_time<=200601))
circuit$key<-as.numeric((196806<=circuit$loc_time & circuit$loc_time<=196810) | (196905<=circuit$loc_time & circuit$loc_time<=196906) | (198606<=circuit$loc_time & circuit$loc_time<=198609) | (circuit$loc_time==200509) | (197110<=circuit$loc_time & circuit$loc_time<=197112) | (198711<=circuit$loc_time & circuit$loc_time<=198802) | (200511<=circuit$loc_time & circuit$loc_time<=200601))
district$key<-as.numeric((196806<=district$loc_time & district$loc_time<=196810) | (196905<=district$loc_time & district$loc_time<=196906) | (198606<=district$loc_time & district$loc_time<=198609) | (district$loc_time==200509) | (197110<=district$loc_time & district$loc_time<=197112) | (198711<=district$loc_time & district$loc_time<=198802) | (200511<=district$loc_time & district$loc_time<=200601))

#indicator for critical nomination, data from Shipan et al. (2014)
#nominees: CLement Haynsworth (August-November 1969), Harold Carswell (January-April 1970), Harry Blackmun (April-May 1970), John Paul Stevens (November-December 1975)
important$critical<-as.numeric((196908<=important$loc_time & important$loc_time<=196911) | (197001<=important$loc_time & important$loc_time<=197004) | (197004<=important$loc_time & important$loc_time<=197005) | (197511<=important$loc_time & important$loc_time<=197512))
routine$critical <-as.numeric((196908<= routine$loc_time & routine$loc_time<=196911) | (197001<=routine$loc_time & routine$loc_time<=197004) | (197004<=routine$loc_time & routine$loc_time<=197005) | (197511<=routine$loc_time & routine$loc_time<=197512))
circuit$critical <-as.numeric((196908<=circuit$loc_time & circuit$loc_time<=196911) | (197001<=circuit$loc_time & circuit$loc_time<=197004) | (197004<=circuit$loc_time & circuit$loc_time<=197005) | (197511<=circuit$loc_time & circuit$loc_time<=197512))
district$critical <-as.numeric((196908<=district$loc_time & district$loc_time<=196911) | (197001<=district$loc_time & district$loc_time<=197004) | (197004<=district$loc_time & district$loc_time<=197005) | (197511<=district$loc_time & district$loc_time<=197512))

write.csv(important, "importantnew.csv")
write.csv(circuit, "circuitnew.csv")
write.csv(routine, "routinenew.csv")
write.csv(district, "districtnew.csv")

###Create year trend
circuit<-with(circuit, circuit[order(nominee_Cong_ID, loc_year, loc_month),])

### BASE MODELS ###
###Model of CIRCUIT Court Nominations###
gam.re.circuit<-gam(confirmed2~exog_event+fil_distance+presApp+scsentences+aba2+min2+gender+renom+sTerm+nonpres+pending_noms+midtermyear_unifsen+bork+pres.maj.med+opp.sen +key+s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=circuit); summary(gam.re.circuit);BIC(gam.re.circuit)
###Model of CIRCUIT Court Nominations###
circ.surv<-Surv()
gam.re.circuit<-gam(confirmed2~exog_event+fil_distance+presApp+scsentences+aba2+min2+gender+renom+sTerm+nonpres+pending_noms+midtermyear_unifsen+bork+pres.maj.med+opp.sen +key+s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=circuit); summary(gam.re.circuit);BIC(gam.re.circuit)


#plot(gam.re.circuit)

###Model of DISTRICT Court Nominations###
gam.re.district<-gam(confirmed2~exog_event+fil_distance+presApp+scsentences+aba2+min2+gender+renom+sTerm+nonpres+pending_noms+midtermyear_unifsen+bork+pres.maj.med+opp.sen +key+s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=district);summary(gam.re.district);BIC(gam.re.district)
#plot(gam.re.district)
#xtable(cbind(summary(gam.re.circuit)$p.coeff,summary(gam.re.circuit)$se[1:17],summary(gam.re.circuit)$p.pv,summary(gam.re.district)$p.coeff,summary(gam.re.district)$se[1:17],summary(gam.re.district)$p.pv),digits=4)

###Model of IMPORTANT proposals###
gam.re.imp<-gam(passed2~scsentences+exog_event+fil_distance+presApp+sTerm+midtermyear_unifsen+agendaSize+pres.maj.med+opp.sen +key +s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=important);summary(gam.re.imp);BIC(gam.re.imp)
#plot(gam.re.imp)

###Model of ROUTINE proposals###
gam.re.unimp<-gam(passed2~scsentences+exog_event+fil_distance+presApp+sTerm+midtermyear_unifsen+agendaSize+pres.maj.med+opp.sen +key +s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=routine);summary(gam.re.unimp);BIC(gam.re.unimp)
#plot(gam.re.unimp)
#xtable(cbind(summary(gam.re.imp)$p.coeff,summary(gam.re.imp)$se[1:11],summary(gam.re.imp)$p.pv,summary(gam.re.unimp)$p.coeff,summary(gam.re.unimp)$se[1:11],summary(gam.re.unimp)$p.pv),digits=4)


###ALTERNATIVE FUNCTIONAL FORMS OF THE TREATMENT VARIABLE###
#Alternative specifications with quadratic functional form of sentences
gam.re.circuit.square<-gam(confirmed2~exog_event+fil_distance+presApp+scsentences+I(scsentences^2)+aba2+min2+gender+renom+sTerm+nonpres+pending_noms+midtermyear_unifsen+bork+pres.maj.med+opp.sen +key+s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=circuit); summary(gam.re.circuit.square);BIC(gam.re.circuit.square)
gam.re.district.square<-gam(confirmed2~exog_event+fil_distance+presApp+scsentences+I(scsentences^2)+aba2+min2+gender+renom+sTerm+nonpres+pending_noms+midtermyear_unifsen+bork+pres.maj.med+opp.sen +key+s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=district);summary(gam.re.district.square);BIC(gam.re.district.square)
gam.re.imp.square<-gam(passed2~scsentences+I(scsentences^2)+exog_event+fil_distance+presApp+sTerm+midtermyear_unifsen+agendaSize+pres.maj.med+opp.sen +key +s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=important);summary(gam.re.imp.square);BIC(gam.re.imp.square)
gam.re.unimp.square<-gam(passed2~scsentences+I(scsentences^2)+exog_event+fil_distance+presApp+sTerm+midtermyear_unifsen+agendaSize+pres.maj.med+opp.sen +key +s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=routine);summary(gam.re.unimp.square);BIC(gam.re.unimp.square)

#Alternative specifications with cubic functional form of sentences
gam.re.circuit.cube<-gam(confirmed2~exog_event+fil_distance+presApp+scsentences+I(scsentences^2)+I(scsentences^3)+aba2+min2+gender+renom+sTerm+nonpres+pending_noms+midtermyear_unifsen+bork+pres.maj.med+opp.sen +key+s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=circuit); summary(gam.re.circuit.cube);BIC(gam.re.circuit.cube)
gam.re.district.cube<-gam(confirmed2~exog_event+fil_distance+presApp+scsentences+I(scsentences^2)+I(scsentences^3)+aba2+min2+gender+renom+sTerm+nonpres+pending_noms+midtermyear_unifsen+bork+pres.maj.med+opp.sen +key+s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=district);summary(gam.re.district.cube);BIC(gam.re.district.cube)
gam.re.imp.cube<-gam(passed2~scsentences+I(scsentences^2)+I(scsentences^3)+exog_event+fil_distance+presApp+sTerm+midtermyear_unifsen+agendaSize+pres.maj.med+opp.sen +key +s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=important);summary(gam.re.imp.cube);BIC(gam.re.imp.cube)
gam.re.unimp.cube<-gam(passed2~scsentences+I(scsentences^2)+I(scsentences^3)+exog_event+fil_distance+presApp+sTerm+midtermyear_unifsen+agendaSize+pres.maj.med+opp.sen +key +s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=routine);summary(gam.re.unimp.cube);BIC(gam.re.unimp.cube)


###ALTERNATIVE MEASURES OF THE TREATMENT MEASURE###
#Alternate specifications with a "nomination pending" variable in lieu of a "relative effort" variable.
nom.circuit<-gam(confirmed2~exog_event+fil_distance+presApp+scmonth+aba2+min2+gender+renom+sTerm+nonpres+pending_noms+midtermyear_unifsen+bork+pres.maj.med+opp.sen +key+s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=circuit); summary(nom.circuit)
nom.district<-gam(confirmed2~exog_event+fil_distance+presApp+scmonth+aba2+min2+gender+renom+sTerm+nonpres+pending_noms+midtermyear_unifsen+bork+pres.maj.med+opp.sen +key+s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=district);summary(nom.district)
nom.imp<-gam(passed2~scmonth+exog_event+fil_distance+presApp+sTerm+midtermyear_unifsen+agendaSize+pres.maj.med+opp.sen +key+s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=important);summary(nom.imp)
nom.unimp<-gam(passed2~scmonth+exog_event+fil_distance+presApp+sTerm+midtermyear_unifsen+agendaSize+pres.maj.med+opp.sen +key+s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=routine);summary(nom.unimp)

#Alternate specifications with a "number of months" variable in lieu of a "relative effort" variable.
months.circuit<-gam(confirmed2~exog_event+fil_distance+presApp+scmonths+aba2+min2+gender+renom+sTerm+nonpres+pending_noms+midtermyear_unifsen+bork+pres.maj.med+opp.sen +key+s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=circuit); summary(months.circuit); BIC(months.circuit)
months.district<-gam(confirmed2~exog_event+fil_distance+presApp+scmonths+aba2+min2+gender+renom+sTerm+nonpres+pending_noms+midtermyear_unifsen+bork+pres.maj.med+opp.sen +key+s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=district);summary(months.district); BIC(months.district)
months.imp.2<-gam(passed2~scmonths+exog_event+fil_distance+presApp+sTerm+midtermyear_unifsen+agendaSize+pres.maj.med+opp.sen +key+s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=important); summary(months.imp.2);BIC(months.imp.2)
months.unimp.2<-gam(passed2~scmonths+exog_event+fil_distance+presApp+sTerm+midtermyear_unifsen+agendaSize+pres.maj.med+opp.sen +key+s(livemonth,by=post_mid)+s(congress_2,bs='re'), family=binomial(link="logit"), data=routine);summary(months.unimp.2);BIC(months.unimp.2)
#xtable(cbind(summary(months.circuit)$p.coeff,summary(months.circuit)$se[1:17],summary(months.circuit)$p.pv,summary(months.district)$p.coeff,summary(months.district)$se[1:17],summary(months.district)$p.pv),digits=4)
#xtable(cbind(summary(months.imp.2)$p.coeff,summary(months.imp.2)$se[1:11],summary(months.imp.2)$p.pv,summary(months.unimp.2)$p.coeff,summary(months.unimp.2)$se[1:11],summary(months.unimp.2)$p.pv),digits=4)


####FIGURES####
###plot of number of sentences over time###
imp.subset.0<-subset(important,select=c(loc_year,loc_month,scsentences))
imp.subset<-imp.subset.0[!duplicated(imp.subset.0),]
routine.subset.0<-subset(routine,select=c(loc_year,loc_month,scsentences))
routine.subset<-routine.subset.0[!duplicated(routine.subset.0),]
dist.subset.0<-subset(district,select=c(loc_year,loc_month,scsentences))
dist.subset<-dist.subset.0[!duplicated(dist.subset.0),]
circ.subset.0<-subset(circuit,select=c(loc_year,loc_month,scsentences))
circ.subset<-circ.subset.0[!duplicated(circ.subset.0),]
total.subset.0<-rbind(imp.subset,routine.subset,dist.subset,circ.subset)
total.subset<-total.subset.0[!duplicated(total.subset.0),]
table(total.subset$loc_year,total.subset$loc_month)
sentence.year<-as.vector(by(total.subset$scsentences,INDICES=total.subset$loc_year,FUN=sum))
#pdf('../numberSentences.pdf',family='Times',width=6,height=6)
plot(y=sentence.year,x=c(1967:2010),type='l',xlab='Year',ylab='Number of Sentences'); abline(h=0,col='gray80')
#dev.off()

###subsets for figures###
circ<-summary(gam.re.circuit)
dist<-summary(gam.re.district)
imp<-summary(gam.re.imp)
unimp<-summary(gam.re.unimp)

###baseline hazard: circuit courts###
#pdf('../circuitBaseline.pdf',family='Times',width=6,height=6)
#pdat.1<-data.frame(livemonth=seq(1,24,by=.01),post_mid=0)
#predict(gam.re.circuit,pdat.1,type='terms',se.fit=TRUE)
plot(x=c(1,48),y=c(-10,3),type='n',axes=F,xlab="Months of Presidential Term",ylab="Smoothed Intercept")
axis(1,at=seq(0,48,by=4))
axis(2);box()
text("Pre-Midterm",x=12, y=-10)
text("Post-Midterm",x=36, y=-10)
abline(v=24.5,col='red',lwd=3)
par(new=T,plt=c(.135,.535,.145,.885))
plot(gam.re.circuit,rug=F,xaxt='n',xlim=c(1,24),ylim=c(-10,3),select=1,axes=F,ann=F)
par(new=T,plt=c(.5225,.925,.145,.885))
plot(gam.re.circuit,rug=F,xlim=c(1,24),ylim=c(-10,3),axes=F,ann=F,xaxt='n',select=2)
#dev.off()
#abline(h=-10,col='gray80');abline(h=2,col='gray80');abline(v=4,col='gray80');abline(v=24,col='gray80')

###baseline hazard: district courts###
#pdf('../districtBaseline.pdf',family='Times',width=6,height=6)
plot(x=c(1,48),y=c(-10,3),type='n',axes=F,xlab="Months of Presidential Term",ylab="Smoothed Intercept")
axis(1,at=seq(0,48,by=4))
axis(2);box()
text("Pre-Midterm",x=12, y=-10)
text("Post-Midterm",x=36, y=-10)
abline(v=24.5,col='red',lwd=3)
par(new=T,plt=c(.135,.535,.145,.885))
plot(gam.re.district,rug=F,xaxt='n',xlim=c(1,24),ylim=c(-10,3),select=1,axes=F,ann=F)
par(new=T,plt=c(.5225,.925,.145,.885))
plot(gam.re.district,rug=F,xlim=c(1,24),ylim=c(-10,3),axes=F,ann=F,xaxt='n',select=2)
#dev.off()

###forest plot: judicial models###
#pdf("../judLine.pdf",width=6, height=6, family='Times', pointsize=10)
c.coef.0<-circ$p.coef[-c(1,3,4,5,6,10,11,12,14,15)]
c.coef<-exp(c.coef.0)
c.se<-circ$se[c(2,7:9,13,16,17)]
c.low.90<-exp(c.coef.0-1.645*c.se)
c.high.90<-exp(c.coef.0+1.645*c.se)
c.low.80<-exp(c.coef.0-1.282*c.se)
c.high.80<-exp(c.coef.0+1.282*c.se)
d.coef.0<-dist$p.coef[-c(1,3,4,5,6,10,11,12,14,15)]
d.coef<-exp(d.coef.0)
d.se<-dist$se[c(2,7:9,13,16,17)]
d.low.90<-exp(d.coef.0-1.645*d.se)
d.high.90<-exp(d.coef.0+1.645*d.se)
d.low.80<-exp(d.coef.0-1.282*d.se)
d.high.80<-exp(d.coef.0+1.282*d.se)

x.high<-max(max(d.high.90),max(c.high.90))
par(omi=c(.2,1.1,.1,.01))
plot(x=c.coef,y=c(1:length(c.coef)-.2),xlim=c(0,x.high),ylim=c(0,length(c.coef)+5), xlab="Odds Ratio", ylab="",axes=F)
axis(1)
axis(2, at=c(1:length(c.coef)), labels=c('Exogenous events','Minority','Female','Renomination','Midterm unified','Opposition senators','Chief or median justice'),las=1)
box()
segments(x0=c.low.90,x1=c.high.90,y0=c(1:length(c.coef)-.2),y1=c(1:length(c.coef)-.2), lty=3, lwd=2)
#arrows(x0=c.low.80,x1=c.high.80,y0=c(1:length(c.coef)-.2),y1=c(1:length(c.coef)-.2), lty=3, lwd=2,angle=90,length=.05,code=3)
points(x=d.coef,y=c(1:length(d.coef)+.2), col='red',pch=2)
segments(x0=d.low.90,x1=d.high.90,y0=c(1:length(d.coef)+.2),y1=c(1:length(d.coef)+.2), col='red', lwd=2)
#arrows(x0=d.low.80,x1=d.high.80,y0=c(1:length(c.coef)+.2),y1=c(1:length(c.coef)+.2), lwd=2,angle=90,length=.05,code=3,col='red')
legend(x=1.75, y=1.5, pch=c(2,1), lty=c(1,3), col=c('red', 'black'), legend=c('District', 'Circuit'))
##dev.off()
abline(h=7.8,col='black')
abline(h=7.7,col='black')
segments(x0=1,x1=1,y0=-1,y1=7.7, col='gray60')

par(new=T,omi=c(.2,1.1,.1,.01))
c.coef.2.a<-circ$p.coef[c(4,6,12,5)]
c.coef.2<-exp(circ$p.coef[c(4,6,12,5)])
c.se.2<-circ$se[c(4,6,12,5)]
c.low.90.2<-exp(c.coef.2.a-1.645*c.se.2)
c.high.90.2<-exp(c.coef.2.a+1.645*c.se.2)
c.low.80.2<-exp(c.coef.2.a-1.282*c.se.2)
c.high.80.2<-exp(c.coef.2.a+1.282*c.se.2)
d.coef.2.a<-dist$p.coef[c(4,6,12,5)]
d.coef.2<-exp(dist$p.coef[c(4,6,12,5)])
d.se.2<-dist$se[c(4,6,12,5)]
d.low.90.2<-exp(d.coef.2.a-1.645*d.se.2)
d.high.90.2<-exp(d.coef.2.a+1.645*d.se.2)
d.low.80.2<-exp(d.coef.2.a-1.282*d.se.2)
d.high.80.2<-exp(d.coef.2.a+1.282*d.se.2)

x.low.2<-min(min(d.low.90.2),min(c.low.90.2))
x.high.2<-max(max(d.high.90.2),max(c.high.90.2))
plot(x=c.coef.2,y=c(9:12-.2),xlim=c(x.low.2,x.high.2),ylim=c(0,length(c.coef)+5), xlab="", ylab="",axes=F)
axis(3)
segments(x0=c.low.90.2,x1=c.high.90.2,y0=c(9:12-.2),y1=c(9:12-.2), lty=3, lwd=2)
points(x=d.coef.2,y=c(9:12+.2), col='red',pch=2)
segments(x0=d.low.90.2,x1=d.high.90.2,y0=c(9:12+.2),y1=c(9:12+.2), col='red', lwd=2)
segments(x0=1,x1=1,y0=7.8,y1=20, col='gray60')
axis(2, at=c(9:12), labels=c('Approval','ABA','Pending','SC sentences'),las=1)
#dev.off()

###baseline hazard: important legislation###
#pdf('../impBaseline.pdf',family='Times',width=6,height=6)
plot(x=c(1,48),y=c(-10,3),type='n',axes=F,xlab="Months of Presidential Term",ylab="Smoothed Intercept")
axis(1,at=seq(0,48,by=4))
axis(2);box()
text("Pre-Midterm",x=12, y=-10)
text("Post-Midterm",x=36, y=-10)
abline(v=24.5,col='red',lwd=3)
par(new=T,plt=c(.135,.535,.145,.885))
plot(gam.re.imp,rug=F,xaxt='n',xlim=c(1,24),ylim=c(-10,3),select=1,axes=F,ann=F)
par(new=T,plt=c(.5225,.925,.145,.885))
plot(gam.re.imp,rug=F,xlim=c(1,24),ylim=c(-10,3),axes=F,ann=F,xaxt='n',select=2)
#dev.off()

###baseline hazard: routine legislation###
#pdf('../unimpBaseline.pdf',family='Times',width=6,height=6)
plot(x=c(1,48),y=c(-10,3),type='n',axes=F,xlab="Months of Presidential Term",ylab="Smoothed Intercept")
axis(1,at=seq(0,48,by=4))
axis(2);box()
text("Pre-Midterm",x=12, y=-10)
text("Post-Midterm",x=36, y=-10)
abline(v=24.5,col='red',lwd=3)
par(new=T,plt=c(.135,.535,.145,.885))
plot(gam.re.unimp,rug=F,xaxt='n',xlim=c(1,24),ylim=c(-10,3),select=1,axes=F,ann=F)
par(new=T,plt=c(.5225,.925,.145,.885))
plot(gam.re.unimp,rug=F,xlim=c(1,24),ylim=c(-10,3),axes=F,ann=F,xaxt='n',select=2)
#dev.off()
#abline(h=-10,col='gray80');abline(h=2,col='gray80');abline(v=4,col='gray80');abline(v=24,col='gray80')

###forest plot: policy models###
#pdf("../policyLine.pdf",width=6, height=6, family='Times', pointsize=10)
i.coef.0<-imp$p.coef[c(10,8,6:5,3:2)]
i.coef<-exp(i.coef.0)
i.se<-imp$se[c(10,8,6:5,3:2)]
i.low.90<-exp(i.coef.0-1.645*i.se)
i.high.90<-exp(i.coef.0+1.645*i.se)
i.low.80<-exp(i.coef.0-1.282*i.se)
i.high.80<-exp(i.coef.0+1.282*i.se)
u.coef.0<-unimp$p.coef[c(10,8,6:5,3:2)]
u.coef<-exp(u.coef.0)
u.se<-unimp$se[c(10,8,6:5,3:2)]
u.low.90<-exp(u.coef.0-1.645*u.se)
u.high.90<-exp(u.coef.0+1.645*u.se)
u.low.80<-exp(u.coef.0-1.282*u.se)
u.high.80<-exp(u.coef.0+1.282*u.se)

x.high<-max(max(i.high.90),max(u.high.90))
par(omi=c(.2,1.1,.1,.01))
plot(x=i.coef,y=c(1:length(i.coef)-.1),xlim=c(0,x.high),ylim=c(0,length(i.coef)+.5), xlab="Odds Ratio", ylab="",axes=F)
axis(1)
axis(2, at=c(1:length(i.coef)), labels=c('Opposition senators','Agenda size','Second term','Approval','Exogenous events','SC sentences'),las=1)
box()
segments(x0=i.low.90,x1=i.high.90,y0=c(1:length(i.coef)-.1),y1=c(1:length(i.coef)-.1), lty=3, lwd=2)
#arrows(x0=i.low.80,x1=i.high.80,y0=c(1:length(i.coef)-.1),y1=c(1:length(i.coef)-.1), lty=2, lwd=2,angle=90,length=.05,code=3)
points(x=u.coef,y=c(1:length(u.coef)+.1), col='red',pch=2)
segments(x0=u.low.90,x1=u.high.90,y0=c(1:length(u.coef)+.1),y1=c(1:length(u.coef)+.1), col='red', lwd=2)
#arrows(x0=u.low.80,x1=u.high.80,y0=c(1:length(u.coef)+.1),y1=c(1:length(u.coef)+.1), lwd=2,angle=90,length=.05,code=3,col='red')
legend(x=2, y=1.5, pch=c(2,1), lty=c(1,3), col=c('red', 'black'), legend=c('Routine', 'Important'))
abline(v=1,col='gray60')
#dev.off()

