# Replication Code: "The Logic of Collective Inaction: Senatorial Delay in Executive Nominations
# Ian Ostrander; 4-28-2015
# Code used to create Figure 2 in R using output from Table 3

coef.vec <- c(.827,.048,1.006,2.45, .762,.855,1.003,1.012, .828,.963,.921,1.139,1.091,.959,3.587,1.068,.705,.739)

se.vec<-c(.0323,.0107,.0015,.1436,.0351,.0310,.00076,.0322,.0372,.0364,.03296,.05688,.0685,.05100,.38419,.0467,.0479,.0302 )


var.names <- c("Senate Divided", "Polarization", "Presidential Approval", "First 90 Days", "Presidential Election",
               "Lame Duck", "Workload", "Female", "Prior Confirmation", "Allied Ideology", "Opposed Ideology", "Defense", "Infrastructure",
               "Social", "Cabinet Level", "High Level", "Major Commission", "Low Level")    


y.axis <- c(length(coef.vec):1) 

par(mar=c(2, 10, 0, 0)) 


plot(coef.vec, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 18, col="dark red", cex = 1.1,   
     xlim = c(0,4.3), xaxs = "r", main = "") 


segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd =  2.3)


axis(1, at = seq(-5,4,by=.5), labels = NA, tick = T,
     cex.axis = 1.2, mgp = c(2,.7,0))
axis(1, at = seq(-4,4,by=1), labels =  c(-4,-3,-2,-1,0,1,2,3,4), tick = T,
     cex.axis = 1.2, mgp = c(2,.7,0))



axis(2, at = y.axis, label = var.names, las = 1, tick = T, ,mgp = c(2,.6,0),
     cex.axis = 1.2) 

segments(1,0,1,21,lty=2) 
box(bty = "l") 
