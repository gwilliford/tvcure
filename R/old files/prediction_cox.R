survpred.coxph <- function(pred, lnt = NULL, CI = T, bw = F, 
                           xlab = "Time", legendtitle, 
                           ylab = "Predicted Survival Probability") {
  # alpha <- survfit(m1, newdata = values[1, ], se.fit = T)
  # beta  <- survfit(m1, newdata = values[2, ], se.fit = T)
  # a(c("alpha", "beta"), F, legendtitle = "Any Leadership Transition")
  pname <- eval(parse(text = pred[1]))
  p  <- matrix(nrow = length(pname$surv), ncol = length(pred))
  pl <- matrix(nrow = length(pname$surv), ncol = length(pred))
  ph <- matrix(nrow = length(pname$surv), ncol = length(pred))
  t <- pname$time
  for (i in 1:length(pred)) {
    pname   <- eval(parse(text = pred[i]))
    p[, i]  <- pname$surv
    pl[, i] <- pname$lower
    ph[, i] <- pname$upper
  }
  
  spm  <- split(p,  rep(1:ncol(p), each = nrow(p)))
  splo <- split(pl, rep(1:ncol(p), each = nrow(p)))
  sphi <- split(ph, rep(1:ncol(p), each = nrow(p)))
  for (i in 1:length(spm)) {
    spm[[i]] <- cbind(spm[[i]], t, num = i, splo[[i]], sphi[[i]])
  }
  spf <- as.data.frame(do.call(rbind, spm))
  colnames(spf) <- c("spm", "Time", "num", "splo", "sphi")
  
  splot <- ggplot()
  if (bw == F) {
    splot <- splot + geom_line(spf, mapping = aes(Time, spm, 
                                                  linetype = as.factor(num), col = as.factor(num))) +
      labs(fill = legendtitle, linetype = legendtitle, col = legendtitle)
    if (CI == T) {
      splot = splot + geom_ribbon(spf, mapping = aes(x = Time, ymin = splo, ymax = sphi,
                                                     col = as.factor(num), fill = as.factor(num), 
                                                     linetype = as.factor(num)), alpha = 0.2)
    }
  } else {
    splot <- splot + geom_line(spf, mapping = aes(Time, spm, linetype = as.factor(num))) + 
      labs(fill = legendtitle, linetype = legendtitle, col = legendtitle) + theme_bw()
    if (CI == T) {
      splot = splot + geom_ribbon(spf, mapping = aes(x = Time, ymin = splo, ymax = sphi, 
                                     fill = as.factor(num), linetype = as.factor(num)), alpha = 0.2) + 
      scale_fill_grey() #start = .6, end = .9)
    }
  }
  splot = splot + ylab(ylab) + xlab(xlab)
  return(splot)
}


