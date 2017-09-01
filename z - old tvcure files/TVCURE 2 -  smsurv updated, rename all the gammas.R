tvcure<-function (formula, cureform, offset = NULL, model=c("ph","aft"), data, na.action = na.omit, link = "logit", Var = TRUE, emmax = 50, eps = 1e-07, nboot = 100){
  #####Data set up
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
      if (ncol(Y)==2){
        Time <- Y[, 1]
        Status <- Y[, 2]
      } else {
        Start <- Y[,1]
        Stop <- Y[,2]
        Status <- Y[,3]
      }
      gammanm <- colnames(Z)
      ngamma <- ncol(Z)
      betanm <- colnames(X)[-1]
      nbeta <- ncol(X) - 1
  
  #####Initial values of beta
      w <- Status
      gamma <- eval(parse(text = paste("glm", "(", "w~Z[,-1]", ",family = quasibinomial(link='", link, "'", ")", ")", sep = "")))$coef
      if (ncol(Y)==2){
        coxinit<-coxph(Surv(Time, Status) ~ X[, -1] + offset(log(w)),subset = w != 0, method = "breslow")
        beta <- coxinit$coefficients
      } else {
        coxinit <- coxph(Surv(Start,Stop,Status) ~X[,-1] + offset(log(w)),subset = w != 0, method="breslow")
        beta <- coxinit$coefficients
      }
  
  #####EM Routine
      if (ncol(Y)==2){
        emfit <- emorig(Time, Status, X, Z, offsetvar, gamma, beta, model, link, emmax, eps)  
      } else {
        emfit <- emcount(Start,Stop,Status, X, Z, offsetvar, gamma, beta, model, link, emmax, eps)
      }
      gamma <- emfit$gamma
      beta <- emfit$latencyfit
      s <- emfit$Survival
      logistfit <- emfit$logistfit
      
  #####Bootstrap standard errors
      if (Var) {
        b_boot <- matrix(rep(0, nboot * ngamma), nrow = nboot)
        beta_boot <- matrix(rep(0, nboot * (nbeta)), nrow = nboot)
        iter <- matrix(rep(0, nboot), ncol = 1)
        if (ncol(Y)==2){
          tempdata <- cbind(Time, Status, X, Z)
        } else {
          tempdata <- cbind(Start, Stop, Status, X, Z)
        }
        data1 <- subset(tempdata, Status == 1)
        data0 <- subset(tempdata, Status == 0)
        n1 <- nrow(data1)
        n0 <- nrow(data0)
        i <- 1
        while (i <= nboot) {
          id1 <- sample(1:n1, n1, replace = TRUE)
          id0 <- sample(1:n0, n0, replace = TRUE)
          bootdata <- rbind(data1[id1, ], data0[id0, ])
          bootZ <- bootdata[, gammanm]
          bootX <- as.matrix(cbind(rep(1, n), bootdata[, betanm]))
          if (ncol(Y)==2){
            bootfit <- emorig(bootdata[, 1], bootdata[, 2], bootX, bootZ, offsetvar, gamma, beta, model, link, emmax,eps)
          } else {
            bootfit <- emcount(bootdata[, 1], bootdata[, 2], bootdata[,3], bootX, bootZ, offsetvar, gamma, beta, model, link, emmax,eps)
          }
          b_boot[i, ] <- bootfit$gamma
          beta_boot[i, ] <- bootfit$latencyfit
          if (bootfit$tau < eps) i <- i + 1
        }
        gamma_var <- apply(gamma_boot, 2, var)
        beta_var <- apply(beta_boot, 2, var)
        gamma_sd <- sqrt(gamma_var)
        beta_sd <- sqrt(beta_var)
      }
      
  #####Final fit details
      fit <- list()
      class(fit) <- c("smcure")
      fit$logistfit <- logistfit
      fit$gamma <- gamma
      fit$beta <- beta
      if (Var) {
        fit$gamma_var <- gamma_var
        fit$gamma_sd <- gamma_sd
        fit$gamma_zvalue <- fit$gamma/gamma_sd
        fit$gamma_pvalue <- (1 - pnorm(abs(fit$gamma_zvalue))) * 2
        fit$beta_var <- beta_var
        fit$beta_sd <- beta_sd
        fit$beta_zvalue <- fit$beta/beta_sd
        fit$beta_pvalue <- (1 - pnorm(abs(fit$beta_zvalue))) * 
          2
      }
      cat(" done.\n")
      fit$call <- call
      fit$gammanm <- gammanm
      fit$betanm <- betanm
      fit$s <- s
      if (ncol(Y)==2) fit$Time <- Time
      fit
      printsmcure(fit, Var)
}

###Function to do em
emorig<-function (Time, Status, X, Z, offsetvar, gamma, beta, model, link, emmax, eps){  
  w <- Status
  n <- length(Status)
  if (model == "ph") 
    s <- smsurv(Time, Status, X, beta, w, model)$survival
  if (model == "aft") {
    if (!is.null(offsetvar)) 
      Time <- Time/exp(offsetvar)
    error <- drop(log(Time) - beta %*% t(X))
    s <- smsurv(error, Status, X, beta, w, model)$survival
  }
  convergence <- 1000
  i <- 1
  while (convergence > eps & i < emmax) {
    uncureprob <- matrix(exp((gamma) %*% t(Z))/(1 + exp((gamma) %*% t(Z))), ncol = 1)
    if (model == "ph") {
      survival <- drop(s^(exp((beta) %*% t(X[, -1]))))
    }
    if (model == "aft") {
      error <- drop(log(Time) - beta %*% t(X))
      survival <- s
    }
    w <- Status + (1 - Status) * (uncureprob * survival)/((1 -                               uncureprob) + uncureprob * survival)
    logistfit <- eval(parse(text = paste("glm", "(", "w~Z[,-1]",",family = quasibinomial(link='", link, "'", ")", ")", sep = "")))
    update_gamma <- logistfit$coef
    if (!is.null(offsetvar)) 
      update_gamma <- as.numeric(eval(parse(text = paste("glm","(", "w~Z[,-1]+offset(offsetvar)", ",family = quasibinomial(link='",link, "'", ")", ")", sep = "")))$coef)
    if (model == "ph") {
      update_cox <- coxph(Surv(Time, Status) ~ X[, -1] + offset(log(w)), subset = w != 0, method = "breslow")
      update_beta <- update_cox$coefficients
      if (!is.null(offsetvar)) 
        update_cox <- coxph(Surv(Time, Status) ~ X[,-1] + offset(offsetvar + log(w)), subset = w != 0, method = "breslow")
        update_beta <- update_cox$coefficients
      
      update_s <- smsurv(Time, Status, X, beta, w, model)$survival
    }
    if (model == "aft") {
      update_beta <- optim(rep(0, ncol(X)), smrank, Time = Time, X = X, n = n, w = w, Status = Status, method = "Nelder-Mead", control = list(reltol = 1e-04, maxit = 500))$par
      update_s <- smsurv(error, Status, X, beta, w, model)$survival
    }
    convergence <- sum(c(update_gamma - gamma, update_beta - beta)^2) + sum((s - update_s)^2)
    gamma <- update_gamma
    beta <- update_beta
    s <- update_s
    uncureprob <- matrix(exp((gamma) %*% t(Z))/(1 + exp((gamma) %*% t(Z))), ncol = 1)
    i <- i + 1
  }
  em <- list(logistfit = logistfit, gamma = gamma, latencyfit = beta,Survival = s, Uncureprob = uncureprob, tau = convergence)
}

emcount<-function(Start, Stop, Status, X, Z, offsetvar, gamma, beta, model, link,emmax, eps){
  w <- Status
  n <- length(Status)
  if (model == "ph") 
    s<-smsurv2(Start, Stop, Status, X, beta, w, model)$survival
    #s <- smsurv2(stopsubs)$survival
  #if (model == "aft") {
   # if (!is.null(offsetvar)) 
    #  Stop <- Stop/exp(offsetvar)
    #error <- drop(log(Stop) - beta %*% t(X))
    #s <- smsurv(error, Status, X, beta, w, model)$survival
    #}
  convergence <- 1000
  i <- 1
  while (convergence > eps & i < emmax) {
    uncureprob <- matrix(exp((gamma) %*% t(Z))/(1 + exp((gamma) %*% t(Z))), ncol = 1)
    if (model == "ph") {
      survival <- drop(s^(exp((beta) %*% t(X[, -1]))))
    }
    if (model == "aft") {
      error <- drop(log(Time) - beta %*% t(X))
      survival <- s
    }
    w <- Status + (1 - Status) * (uncureprob * survival)/((1 -  uncureprob) + uncureprob * survival)
    logistfit <- eval(parse(text = paste("glm", "(", "w~Z[,-1]",",family = quasibinomial(link='", link, "'", ")", ")", sep = "")))
    update_gamma <- logistfit$coef
    if (!is.null(offsetvar)) 
      update_gamma <- as.numeric(eval(parse(text = paste("glm","(", "w~Z[,-1]+offset(offsetvar)", ",family = quasibinomial(link='",link, "'", ")", ")", sep = "")))$coef)
    if (model == "ph") {
      update_coxinit <- coxph(Surv(Start,Stop, Status) ~ X[, -1] + offset(log(w)), subset = w != 0, method = "breslow")
      update_beta <- update_coxinit$coefficients
      if (!is.null(offsetvar)) 
        update_coxinit <-coxph(Surv(Start,Stop, Status) ~ X[,-1] + offset(offsetvar + log(w)), subset = w != 0, method = "breslow")
        update_beta <- update_coxinit$coefficients
      update_s <-smsurv2(Start, Stop, Status, X, beta, w, model)$survival
    }
    if (model == "aft") {
      update_beta <- optim(rep(0, ncol(X)), smrank, Time = Stop, X = X, n = n, w = w, Status = Status, method = "Nelder-Mead", control = list(reltol = 1e-04, maxit = 500))$par
      update_s <- smsurv(error, Status, X, beta, w, model)$survival
    }
    convergence <- sum(c(update_gamma - gamma, update_beta - beta)^2) + sum((s - update_s)^2)
    gamma <- update_gamma
    beta <- update_beta
    s <- update_s
    uncureprob <- matrix(exp((gamma) %*% t(Z))/(1 + exp((gamma) %*% t(Z))), ncol = 1)
    i <- i + 1
  }
  em <- list(logistfit = logistfit, gamma = gamma, latencyfit = beta,Survival = s, Uncureprob = uncureprob, tau = convergence)
}

#smsurv2<-function(coxinit,w){
#  sm <- exp(-predict(coxtest1,type="expected")
#  list(survival = sm)
#}

