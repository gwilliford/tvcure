tvcure <- function(formula, cureform, offset = NULL, model=c("ph","aft"), data, na.action = na.omit, link = "logit", Var = TRUE, firthlogit = FALSE, emmax = 50, eps = 1e-07, nboot = 100){
  # Data set up ------------------------------------
    call <- match.call()
    model <- match.arg(model)
    cat("Program is running..be patient...")
    data <- na.action(data)
    n <- dim(data)[1]
    mf <- model.frame(formula, data)
    cvars <- all.vars(cureform)
    Z <- as.matrix(cbind(rep(1, n), data[, cvars]))
    colnames(Z) <- c("(Intercept)", cvars)
    if (!is.null(offset)) {
      offsetvar <- all.vars(offset)
      offsetvar <- data[, offsetvar]
    }
    else offsetvar <- NULL
    Y <- model.extract(mf, "response")
    X <- model.matrix(attr(mf, "terms"), mf)
    if (!inherits(Y, "Surv"))
      stop("Response must be a survival object")
    survtype <- attr(Y,"type")
    if (survtype=="right"){
      Time <- Y[, 1]
      Status <- Y[, 2]
      survobj <- Surv(Time,Status)
    }
    if (survtype=="counting"){
      Start <- Y[, 1]
      Stop <- Y[, 2]
      Status <- Y[, 3]
      Time <- Stop
      survobj <- Surv(Start, Stop, Status)
    }
    gnames <- colnames(Z)
    ngamma <- ncol(Z)
    if (model == "ph") {
      bnames <- colnames(X)[-1]
      nbeta <- ncol(X) - 1
    }
    if (model == "aft") {
      bnames <- colnames(X)
      nbeta <- ncol(X)
    }

  # Initial values -------------------------------------------------------------
    w <- Status
    g <- eval(parse(text = paste("glm", "(", "w~Z[,-1]", ",family = quasibinomial(link='", link, "'", ")", ")", sep = "")))$coef
    if (model == "ph")
      beta <- coxph(survobj ~ X[, -1] + offset(log(w)), subset = w != 0, method = "breslow")$coef
    if (model == "aft")
      beta <- survreg(survobj ~ X[, -1])$coef
    #if (survtype=="right") {
    #  coxinit<-coxph(Surv(Time, Status) ~ X[, -1] + offset(log(w)),subset = w != 0, method = "breslow")
    #  beta <- coxinit$coefficients
    #}
    #if (survtype=="counting") {
    #  coxinit <- coxph(Surv(Start,Stop,Status) ~X[,-1] + offset(log(w)),subset = w != 0, method="breslow")
    #  beta <- coxinit$coefficients
    #}

  # Call to EM functions -------------------------------------------------------
    if (survtype=="right") {
      emfit <- emorig(Time, Status, X, Z, offsetvar, g, beta, model, link, emmax, eps, firthlogit, survobj)
    }
    if (survtype=="counting") {
      emfit <- emcount(Start, Stop, Status, X, Z, offsetvar, g, beta, model, link, emmax, eps, firthlogit, survobj)
    }
    g <- emfit$g
    beta <- emfit$latencyfit
    s <- emfit$Survival
    incidence_fit <- emfit$emfit

  # Bootstrap standard errors --------------------------------------------------
    if (Var) {
      varout <- tvboot(nboot, nbeta, ngamma, survtype, Time, Start, Stop, Status, X, Z, gnames, bnames, offsetvar, g, beta, model, link, emmax, eps, firthlogit, survobj, n)
    }

  #####Final fit details
      fit <- list()
      class(fit) <- c("tvcure")
      fit$incidence_fit <- incidence_fit
      fit$g <- g
      fit$beta <- beta
      if (Var) {
        fit$g_var <- varout$g_var
        fit$g_sd <- varout$g_sd
        fit$g_zvalue <- fit$g/fit$g_sd
        fit$g_pvalue <- (1 - pnorm(abs(fit$g_zvalue))) * 2
        fit$b_var <- varout$b_var
        fit$b_sd <- varout$b_sd
        fit$b_zvalue <- fit$beta/fit$b_sd
        fit$b_pvalue <- (1 - pnorm(abs(fit$b_zvalue))) * 2
      }
      cat(" done.\n")
      fit$call <- call
      fit$gnames <- gnames
      fit$bnames <- bnames
      fit$s <- s
      if (ncol(Y)==2) fit$Time <- Time
      fit
}

###Function to do em
emorig<-function(Time, Status, X, Z, offsetvar, g, beta, model, link, emmax, eps, firthlogit, survobj){
  w <- Status
  n <- length(Status)
  if (model == "ph")
    #if (survtype=="right")
      s <- tvsurv(Time, Status, X, beta, w, model)$survival
    #if (survtype=="counting")
    #  s <- tvsurv(Stop, Status, X, beta, w, model)$survival
  if (model == "aft") {
    if (!is.null(offsetvar))
      Time <- Time/exp(offsetvar)
    error <- drop(log(Time) - beta %*% t(X))
    s <- tvsurv(error, Status, X, beta, w, model)$survival
  }
  convergence <- 1000
  i <- 1
  while (convergence > eps & i < emmax) {
    uncureprob <- matrix(exp((g) %*% t(Z))/(1 + exp((g) %*% t(Z))), ncol = 1)
    if (model == "ph") {
      survival <- drop(s^(exp((beta) %*% t(X[, -1]))))
    }
    if (model == "aft") {
      error <- drop(log(Time) - beta %*% t(X))
      survival <- s
    }
    w <- Status + (1 - Status) * (uncureprob * survival)/((1 -                               uncureprob) + uncureprob * survival)
    if (firthlogit) {
        incidence_fit <- eval(parse(text = paste("logistf","(", "w~Z[,-1]",")",sep="")))
      } else {
        incidence_fit <- eval(parse(text = paste("glm", "(", "w~Z[,-1]",",family = quasibinomial(link='", link, "'", ")", ")", sep = "")))
      }
    update_cureb <- incidence_fit$coef
    if (!is.null(offsetvar))
      update_cureb <- as.numeric(eval(parse(text = paste("glm","(", "w~Z[,-1]+offset(offsetvar)", ",family = quasibinomial(link='",link, "'", ")", ")", sep = "")))$coef)
    if (model == "ph") {
      update_beta <- coxph(survobj ~ X[, -1] + offset(log(w)), subset = w != 0, method = "breslow")$coef
      if (!is.null(offsetvar))
        update_beta <- coxph(survobj ~ X[,-1] + offset(offsetvar + log(w)), subset = w != 0, method = "breslow")$coef
      update_s <- tvsurv(Time, Status, X, beta, w, model)$survival
    }
    if (model == "aft") {
      update_beta <- optim(rep(0, ncol(X)), smrank, Time = Time, X = X, n = n, w = w, Status = Status, method = "Nelder-Mead", control = list(reltol = 1e-04, maxit = 500))$par
      update_s <- tvsurv(error, Status, X, beta, w, model)$survival
    }
    convergence <- sum(c(update_cureb - g, update_beta - beta)^2) + sum((s - update_s)^2)
    g <- update_cureb
    beta <- update_beta
    s <- update_s
    uncureprob <- matrix(exp((g) %*% t(Z))/(1 + exp((g) %*% t(Z))), ncol = 1)
    i <- i + 1
  }
  em <- list(incidence_fit = incidence_fit, g = g, latencyfit = beta,Survival = s, Uncureprob = uncureprob, tau = convergence)
}

emcount<-function(Start, Stop, Status, X, Z, offsetvar, g, beta, model, link,emmax, eps, firthlogit, survobj){
  w <- Status
  n <- length(Status)
  if (model == "ph")
    s<-smsurv2(Start, Stop, Status, X, beta, w, model)$survival
    #s <- smsurv2(stopsubs)$survival
  #if (model == "aft") {
   # if (!is.null(offsetvar))
    #  Stop <- Stop/exp(offsetvar)
    #error <- drop(log(Stop) - beta %*% t(X))
    #s <- tvsurv(error, Status, X, beta, w, model)$survival
    #}
  convergence <- 1000
  i <- 1
  while (convergence > eps & i < emmax) {
    uncureprob <- matrix(exp((g) %*% t(Z))/(1 + exp((g) %*% t(Z))), ncol = 1)
    if (model == "ph") {
      survival <- drop(s^(exp((beta) %*% t(X[, -1]))))
    }
    if (model == "aft") {
      error <- drop(log(Time) - beta %*% t(X))
      survival <- s
    }
    w <- Status + (1 - Status) * (uncureprob * survival)/((1 -  uncureprob) + uncureprob * survival)
    if (firthlogit){
      incidence_fit <- eval(parse(text = paste("logistf","(", "w~Z[,-1]",")",sep="")))
    } else {
      incidence_fit <- eval(parse(text = paste("glm", "(", "w~Z[,-1]",",family = quasibinomial(link='", link, "'", ")", ")", sep = "")))
    }
    update_cureb <- incidence_fit$coef
    if (!is.null(offsetvar))
      update_cureb <- as.numeric(eval(parse(text = paste("glm","(", "w~Z[,-1]+offset(offsetvar)", ",family = quasibinomial(link='",link, "'", ")", ")", sep = "")))$coef)
    if (model == "ph") {
      update_coxinit <- coxph(survobj ~ X[, -1] + offset(log(w)), subset = w != 0, method = "breslow")
      update_beta <- update_coxinit$coefficients
      if (!is.null(offsetvar))
        update_coxinit <-coxph(survobj ~ X[,-1] + offset(offsetvar + log(w)), subset = w != 0, method = "breslow")
        update_beta <- update_coxinit$coefficients
      update_s <-smsurv2(Start, Stop, Status, X, beta, w, model)$survival
    }
    if (model == "aft") {
      update_beta <- optim(rep(0, ncol(X)), smrank, Time = Stop, X = X, n = n, w = w, Status = Status, method = "Nelder-Mead", control = list(reltol = 1e-04, maxit = 500))$par
      update_s <- tvsurv(error, Status, X, beta, w, model)$survival
    }
    convergence <- sum(c(update_cureb - g, update_beta - beta)^2) + sum((s - update_s)^2)
    g <- update_cureb
    beta <- update_beta
    s <- update_s
    uncureprob <- matrix(exp((g) %*% t(Z))/(1 + exp((g) %*% t(Z))), ncol = 1)
    i <- i + 1
  }
  em <- list(incidence_fit = incidence_fit, g = g, latencyfit = beta,Survival = s, Uncureprob = uncureprob, tau = convergence)
}

