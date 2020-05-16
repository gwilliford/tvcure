
alpha <- survfit(m1, newdata = values[1, ], se.fit = T)
beta  <- survfit(m1, newdata = values[2, ], se.fit = T)

p1  <- alpha$surv
p1l <- alpha$lower
p1h <- alpha$upper
p1t <- alpha$time

p2 <-  beta$surv
p2l <- beta$lower
p2h <- beta$upper
p2t <- beta$time

# survfit object


ggplot(mapping = aes(x = p1t, y = p1)) + geom_ribbon(aes(ymin = p1l, ymax = p1h))
ggplot(mapping = aes(x = p2t, y = p2)) + geom_ribbon(aes(ymin = p2l, ymax = p2h))


# # Add CIs
# if (CI == T) {
#   if (bw == F) {
#     splot = splot + geom_ribbon(scf, mapping = aes(x = Time, ymin = sclo, ymax = schi, col = as.factor(num),
#                                                    fill = as.factor(num), linetype = as.factor(num)), alpha=0.2) +
#       labs(fill = legendtitle, linetype = legendtitle, col = legendtitle)
#   } else {
#     splot = splot + geom_ribbon(scf, mapping = aes(x = Time, ymin = sclo, ymax = schi,
#                                                    linetype = as.factor(num)), alpha=0.2) +
#       labs(linetype = legendtitle)
#   }
# }
# splot = splot + ylab(ylab) + xlab(xlab)
# }


spm  <- split(survpredmean, rep(1:ncol(survpredmean), each = nrow(survpredmean)))
  splo <- split(survpredlo, rep(1:ncol(survpredlo), each = nrow(survpredlo)))
  sphi <- split(survpredhi, rep(1:ncol(survpredhi), each = nrow(survpredhi)))
  for (i in 1:length(spm)) {
    spm[[i]] <- cbind(spm[[i]], Time, num = i, splo[[i]], sphi[[i]])
  }
  spf <- as.data.frame(do.call(rbind, spm))
  colnames(spf) <- c("spm", "Time", "num", "splo", "sphi")
}

# Plot line

# Extract lines from 
pred <- c("alpha", "beta")

survpred.coxph <- function(pred, bw = F, xlab = "Time", legendtitle, ylab = "Predicted Survival Probability") {
  pname <- eval(parse(text = pred[1]))
  p  <- matrix(nrow = length(pname$surv), ncol = length(pred))
  pl <- matrix(nrow = length(pname$surv), ncol = length(pred))
  ph <- matrix(nrow = length(pname$surv), ncol = length(pred))
  t <- pname$time
  for (i in 1:length(pred)) {
    pname <- eval(parse(text = pred[i]))
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

  if (bw == F) {
    splot <- ggplot()
    splot <- splot + geom_line(spf, mapping = aes(Time, spm, linetype = as.factor(num), col = as.factor(num))) + 
      geom_ribbon(spf, mapping = aes(x = Time, ymin = splo, ymax = sphi, col = as.factor(num),
                                                   fill = as.factor(num), linetype = as.factor(num)), alpha=0.2) +
      labs(fill = legendtitle, linetype = legendtitle, col = legendtitle)
  } else {
    splot <- ggplot()
    splot <- splot + geom_line(spf, mapping = aes(Time, spm, linetype = as.factor(num))) + 
      geom_ribbon(spf, mapping = aes(x = Time, ymin = splo, ymax = sphi, 
                                     fill = as.factor(num), linetype = as.factor(num)), alpha=0.2) + 
      labs(fill = legendtitle, linetype = legendtitle, col = legendtitle) +
      scale_fill_grey(start = 0, end = .9)
  }
  splot = splot + ylab(ylab) + xlab(xlab)
  return(splot)
}

  #   s <- sboot + geom_line(mapping = aes(x = t, y = p, col = as.factor(num)))
  #   geom_ribbon(aes(ymin = pl, ymax = ph, x = t), fill = i)
  #   sname <- paste("s", i, sep = "")
  #   assign(sname, s)
  #   if (i > 1) {
  #     splot <- s + sl
  #   }
  # }
  # for (i in 1:length(pred)) {
  #   ss
  # }
  # 

a(c("alpha", "beta"), F, legendtitle = "Any Leadership Transition")

