?  Limdep commands to read and process war duration data.

?  These commands should produce the same output as is presented
?     in the Bennett/Stam 1996 APSR article on war duration.

?  Note:  You may want to change the path name pointing to the location
?  of the data file, depending on where you put the data on your computer.


read; nvar=32; 
   file=1peryrF.raw;
   names = warnum, year, ccode1, ccode2, lastyear, time, logtime,
   censor, nperiod, 
   oadm, oada, oadp, omdm, omda, omdp, opdm, opda, opdp, durscale, 
   Rterrain, Rterrstr, sumpop, popratio, summper, bofadj, qualrat,   
   salscale, reprsum, demosum, surpdiff, nactors, adis3010  $
 
skip $
 
? *** Survival Analysis, annualized war duration data ***
 
namelist; indivstr=oadm, oada, oadp, opda $
 
? Main Model
namelist; X = one, indivstr, Rterrain, Rterrstr, bofadj, summper,
sumpop, popratio, qualrat, surpdiff, reprsum, demosum,
salscale, adis3010, nactors $
survival; lhs=logtime, censor, nperiod;
rhs = X; model= weibull; keep=hazfunc; res=inthaz $

? Constant only Model
namelist; X = one $
survival; lhs=logtime, censor, nperiod;
rhs = X; model= weibull; keep=hazfunc; res=inthaz $
 
? Realpolitique Model
namelist; X = one, indivstr, Rterrain, Rterrstr, bofadj, summper,
sumpop, popratio, qualrat, surpdiff $
survival; lhs=logtime, censor, nperiod;
rhs = X; model= weibull; keep=hazfunc; res=inthaz $
 
? Domestic only Model
namelist; X = one, reprsum,  demosum, salscale, adis3010, nactors $
survival; lhs=logtime, censor, nperiod;
rhs = X; model= weibull; keep=hazfunc; res=inthaz $
 
? Vuch and Teach model.
create ; Vyear = year - 1816 $
namelist; X = one, nactors, popratio, Vyear $
survival; lhs=logtime, censor, nperiod;
rhs = X; model= weibull; keep=hazfunc; res=inthaz $
 

-----------------------------------------------------------------------
-----------------------------------------------------------------------

?  LIMDEP commands, 1 Per war data set.

read; nvar=31;
   file=1perwarF.raw;
   names = warnum, frstyear, lastyear, ccode1, ccode2, 
   length, logleng, censor, oadm, oada, oadp, omdm, omda, omdp, 
   opdm, opda, opdp, durscale, 
   Rterrain, Rterrstr, summperB, sumpopbg, popratbg, bofadjbg, qualratB, 
   salscale, reprsumB, demosumB, surpdiff, nactors, adis3010 $
skip $
 

? *** Survival Analysis, 1 case per war duration data ***
 
namelist ; indivstr=oadm, oada, oadp, opda $
 
? *  First variables at beginning of war.  Main final model.
namelist; X= one, indivstr, rterrain, rterrstr, bofadjbg, summperB,
sumpopBG, popratbg, qualratB, surpdiff, reprsumB, demosumB, salscale,
adis3010, nactors $
survival; lhs=logleng, censor;  rhs = X;  units=months;
model=weibull; keep = hazfunc; res=inthaz  $
 
? *  Main final model, check with heterogeneity.
survival; lhs=logleng, censor;
rhs = one, indivstr, rterrain, rterrstr, bofadjbg,
summperB, sumpopBG, popratbg, qualratB, surpdiff,
reprsumB, demosumB, salscale, adis3010, nactors;
units=months; model=weibull; heterogeneity $
 
? *  Now beginning of war realpol subset.
namelist ; X= one, indivstr, rterrain, rterrstr, bofadjbg,
summperB, sumpopBG, popratbg, qualratB, surpdiff $
survival; lhs=logleng, censor; rhs = X; units=months;
model=weibull; keep = hazfunc; res=inthaz  $
 
? *  Now beginning of war domestic subset.
namelist ; X= reprsumB, demosumB, salscale, adis3010, nactors $
survival; lhs=logleng, censor; rhs = X; units=months;
model=weibull; keep = hazfunc; res=inthaz  $
 
? *  Now constant only model.
namelist ; X= one $
survival; lhs=logleng, censor; rhs = X; units=months;
model=weibull; keep = hazfunc; res=inthaz  $
 
?  Now V+T model
create ; vyear=frstyear-1816 $
namelist ; X= one, popratbg, nactors, vyear $
survival; lhs=logleng, censor; rhs = X; units=months;
model=weibull; keep = hazfunc; res=inthaz  $

