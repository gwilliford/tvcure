  tvem <- function(Time, Start, Stop, Status, X, Z, offsetvar, gamma, beta,
                   link, emmax, eps, brglm, firthcox, survobj, survtype){
    w <- Status
    n <- length(Status)
    s <- tvsurv(Time, Status, X, beta, w)$survival
    convergence <- 1000

    i <- 1
    while (convergence > eps & i < emmax) {
      if (link == "logit") {
        uncureprob <- matrix(exp((gamma) %*% t(Z))/(1 + exp((gamma) %*% t(Z))), ncol = 1) #matrix(glmres$fitted.values,
      }
      if (link == "probit") {
        uncureprob <- matrix(pnorm(gamma %*% t(Z)), ncol = 1)
      }
      survival <- drop(s^(exp((beta) %*% t(X))))
      w <- Status + (1 - Status) * (uncureprob * survival)/((1 -
          uncureprob) + uncureprob * survival)

      # Update incidence coefficients
      if (brglm) {
        if (is.null(offsetvar)) {
          incidence_fit <- eval(parse(text = paste("brglm::brglm", "(", "as.integer(w) ~ Z[, -1],",
                                                    "family = binomial(link = '", link, "'", ")",
                                                   ")", sep = "")))
        } else {
          incidence_fit <- eval(parse(text = paste("brglm::brglm","(", "as.integer(w) ~ Z[, -1] + offset(offsetvar),",
                                                   "family = binomial(link='", link, "'", ")",
                                                   ")", sep = "")))
        }
      } else {
        if (is.null(offsetvar)) {
          incidence_fit <- eval(parse(text = paste("glm", "(", "as.integer(w) ~ Z[, -1],",
                                                    "family = binomial(link = '", link, "'", ")",
                                                   ")", sep = "")))
        } else {
          incidence_fit <- eval(parse(text = paste("glm", "(", "as.integer(w) ~ Z[, -1] + offsetvar(offsetvar),",
                                                    "family = binomial(link = '", link, "'", ")",
                                                   ")", sep = "")))
        }
      }
      update_cureg <- incidence_fit$coef

      # Update latency coefficients
      if (!is.null(offsetvar)) {
        coxit <- coxph(survobj ~ X + offset(offsetvar + log(w)),
                             subset = w != 0, method = "breslow")$coef

      } else {
        coxit <- coxph(survobj ~ X + offset(log(w)), subset = w != 0,
                       method = "breslow")
      }
      update_a <- tvsurv(Time, Status, X, beta, w)
      update_s <- update_a$survival

      if (!inherits(coxit,"error")){
        update_beta <- coxit$coefficients
        convergence <- sum(c(update_cureg - gamma, update_beta - beta)^2)
                       + sum((s - update_s)^2)
        # if (is.na(convergence)) {
        #   if (is.na(update_cureg)) {
        #     stop("Infinite coefficient. EM algorithm failed to converge.")
        #   }
        #   if (is.na(update_cureb)) {
        #     stop("Infinite coefficient. EM algorithm failed to converge.")
        #   }
        # }
        #if (i >= 53) browser()
        if (is.infinite(convergence)) stop("EM algorithm failed to converge.")
        gamma <- update_cureg
        beta<- update_beta
        s <- update_s
        basehaz <- update_a$basehaz
        if (link == "logit")
          uncureprob <- matrix(exp((gamma) %*% t(Z))/(1 + exp((gamma) %*% t(Z))), ncol = 1)#matrix(incidence_fit$fitted.values,
        if (link == "probit")
          uncureprob <- matrix(pnorm(gamma %*% t(Z)), ncol = 1)
        i <- i + 1
      }
    }
    em <- list(incidence_fit = incidence_fit, gamma = gamma, latencyfit = beta,
               Survival = s, Basehaz = basehaz, uncureprob = uncureprob, tau = convergence, emrun = i)
  }
