tvem <- function(Time, Start, Stop, Status, X, Z, offsetvar, gamma, beta, model,
                 link, emmax, eps, firthlogit, firthcox, survobj, survtype){
  w <- Status
  n <- length(Status)
  if (model == "ph")
      s <- tvsurv(Time, Status, X, beta, w, model)$survival
  if (model == "aft") {
    if (!is.null(offsetvar))
      Time <- Time/exp(offsetvar)
    error <- drop(log(Time) - beta %*% t(X))
    s <- tvsurv(error, Status, X, beta, w, model)$survival
  }
  convergence <- 1000
  while (convergence > eps & i < emmax) {
    uncureprob <- matrix(exp((gamma) %*% t(Z))/(1 + exp((gamma) %*% t(Z))),
                         ncol = 1)
    if (model == "ph") {
      survival <- drop(s^(exp((beta) %*% t(X[, -1]))))
    }
    if (model == "aft") {
      error <- drop(log(Time) - beta %*% t(X))
      survival <- s
    }
    w <- Status + (1 - Status) * (uncureprob * survival)/((1 -
        uncureprob) + uncureprob * survival)
    if (firthlogit) {
      incidence_fit <- eval(parse(text = paste("logistf","(", "w~Z[,-1]",
                                               ")",sep="")))
    } else {
      incidence_fit <- eval(parse(text = paste("glm", "(", "w~Z[,-1]",",
                            family = quasibinomial(link='", link, "'",
                                               ")", ")", sep = "")))
    }

    # Update glm
    update_cureg <- incidence_fit$coef
    if (!is.null(offsetvar))
      update_cureg <- as.numeric(eval(parse(text = paste("glm","(", "w~Z[,-1]
                       +offset(offsetvar)",",family = quasibinomial(link='",
                      link, "'", ")", ")", sep = "")))$coef)

    # Update cox
    if (model == "ph") {
      if (!is.null(offsetvar)) {
        if (firthcox) {
          update_beta <- coxphf(survobj ~ X[,-1] + offset(offsetvar + log(w)),
                                pl=F)$coefficients
        } else {
          update_beta <- coxph(survobj ~ X[,-1] + offset(offsetvar + log(w)),
                               subset = w != 0, method = "breslow")$coef
        }
      } else {
        if (firthcox) {
          update_beta <- coxphf(survobj ~ X[,-1] + offset(log(w)),pl=F)$coefficients
        } else {
          tryCatch(
            coxit <- coxph(survobj ~ X[, -1] + offset(log(w)), subset = w != 0,
                           method = "breslow"),
            error = function(e) e
          )
        }
      }
      update_s <- tvsurv(Time, Status, X, beta, w, model)$survival
    }
    if (model == "aft") {
      update_beta <- optim(rep(0, ncol(X)), smrank, Time = Time, X = X, n = n,
                           w = w, Status = Status, method = "Nelder-Mead",
                           control = list(reltol = 1e-04, maxit = 500))$par
      update_s <- tvsurv(error, Status, X, beta, w, model)$survival
    }
    if (!inherits(coxit,"error")){
      update_beta <- coxit$coefficients
      convergence <- sum(c(update_cureg - gamma, update_beta - beta)^2) +
        sum((s - update_s)^2)
      gamma <- update_cureg
      beta <- update_beta
      s <- update_s
      uncureprob <- matrix(exp((gamma) %*% t(Z))/(1 + exp((gamma) %*% t(Z))),
                           ncol = 1)
      i<-i+1
    }
  }
  em <- list(incidence_fit = incidence_fit, gamma = gamma, latencyfit = beta,
             Survival = s, Uncureprob = uncureprob, tau = convergence)
}
