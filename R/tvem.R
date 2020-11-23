  tvem <- function(Time, Status, X, Z, offset, gamma, beta,
                   link, emmax, eps, brglm, survobj, survtype, method){

    w <- Status
    n <- length(Status)
    s <- tvsurv(Time, Status, X, beta, w)$survival
    convergence <- 1000

    i <- 1
    while (convergence > eps & i < emmax) {
      if (link == "logit") {
        uncureprob <- matrix(exp((gamma) %*% t(Z))/(1 + exp((gamma) %*% t(Z))), ncol = 1)
      }
      if (link == "probit") {
        uncureprob <- matrix(pnorm(gamma %*% t(Z)), ncol = 1)
      }

      survival <- drop(s^(exp((beta) %*% t(X))))
      w <- Status + (1 - Status) * (uncureprob * survival)/((1 - uncureprob) + uncureprob * survival)

      # Update incidence coefficients
      if (is.null(offset)) {
        incidence_fit <- eval(parse(text = paste("glm", "(", "as.integer(w) ~ Z[, -1],",
                                                 "family = binomial(link = '", link, "'", "), ",
                                                 "method = '", method, "'",
                                                 ")", sep = "")))
      } else {
        incidence_fit <- eval(parse(text = paste("glm", "(", "as.integer(w) ~ Z[, -1],",
                                                 "family = binomial(link = '", link, "'", "), ",
                                                 "method = '", method, "'",
                                                 ")", sep = "")))
      }
      update_cureg <- incidence_fit$coef

      # Update latency coefficients
      if (!is.null(offset)) {
        coxit <- coxph(survobj ~ X + offset(offset + log(w)), subset = w != 0,
                       method = "breslow", x = T)
      } else {
        coxit <- coxph(survobj ~ X + offset(log(w)), subset = w != 0,
                       method = "breslow", x = T)
      }
      update_a <- tvsurv(Time, Status, X, beta, w)
      update_s <- update_a$survival

      if (!inherits(coxit,"error")){
        update_beta <- coxit$coefficients
        convergence <- sum(c(update_cureg - gamma, update_beta - beta)^2) + sum((s - update_s)^2)
        if (is.infinite(convergence) | is.na(convergence)
            ) stop("EM algorithm failed to converge.")
        gamma <- update_cureg
        beta<- update_beta
        s <- update_s
        basehaz <- update_a$basehaz
        if (link == "logit")
          uncureprob <- matrix(exp((gamma) %*% t(Z))/(1 + exp((gamma) %*% t(Z))), ncol = 1)
        if (link == "probit")
          uncureprob <- matrix(pnorm(gamma %*% t(Z)), ncol = 1)
        i <- i + 1
      }
    }

    em <- list(incidence_fit = incidence_fit, gamma = gamma, beta = beta, latency_fit = coxit,
               Survival = s, Basehaz = basehaz, uncureprob = uncureprob, w = w,
               tau = convergence, emrun = i)
  }
