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
      bnm <- colnames(Z)
      nb <- ncol(Z)
      betanm <- colnames(X)[-1]
      nbeta <- ncol(X) - 1
  
  #####Initial values of beta
      w <- Status
      b <- eval(parse(text = paste("glm", "(", "w~Z[,-1]", ",family = quasibinomial(link='", link, "'", ")", ")", sep = "")))$coef
      if (ncol(Y)==2){
        beta <- coxph(Surv(Time, Status) ~ X[, -1] + offset(log(w)),subset = w != 0, method = "breslow")$coef
      } else {
        beta <- coxph(Surv(Start,Stop,Status) ~X[,-1] + offset(log(w)),subset = w != 0, method="breslow")$coef
      }
  
  #####EM Routine
      if (ncol(Y)==2){
        emfit <- emorig(Time, Status, X, Z, offsetvar, b, beta, model, link, emmax, eps)  
      } else {
        emfit <- emcount(Start,Stop, Status, X, Z, offsetvar, b, beta, model, link, emmax, eps)
      }
      b <- emfit$b
      beta <- emfit$latencyfit
      s <- emfit$Survival
      logistfit <- emfit$logistfit
      
  #####Bootstrap standard errors
      if (Var) {
        b_boot <- matrix(rep(0, nboot * nb), nrow = nboot)
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
          bootZ <- bootdata[, bnm]
          bootX <- as.matrix(cbind(rep(1, n), bootdata[, betanm]))
          bootfit <- em(bootdata[, 1], bootdata[, 2], bootX, bootZ, offsetvar, b, beta, model, link, emmax,eps)
          b_boot[i, ] <- bootfit$b
          beta_boot[i, ] <- bootfit$latencyfit
          if (bootfit$tau < eps) i <- i + 1
        }
        b_var <- apply(b_boot, 2, var)
        beta_var <- apply(beta_boot, 2, var)
        b_sd <- sqrt(b_var)
        beta_sd <- sqrt(beta_var)
      }
      
  #####Final fit details
      fit <- list()
      class(fit) <- c("smcure")
      fit$logistfit <- logistfit
      fit$b <- b
      fit$beta <- beta
      if (Var) {
        fit$b_var <- b_var
        fit$b_sd <- b_sd
        fit$b_zvalue <- fit$b/b_sd
        fit$b_pvalue <- (1 - pnorm(abs(fit$b_zvalue))) * 2
        fit$beta_var <- beta_var
        fit$beta_sd <- beta_sd
        fit$beta_zvalue <- fit$beta/beta_sd
        fit$beta_pvalue <- (1 - pnorm(abs(fit$beta_zvalue))) * 
          2
      }
      cat(" done.\n")
      fit$call <- call
      fit$bnm <- bnm
      fit$betanm <- betanm
      fit$s <- s
      if (ncol(Y)==2) fit$Time <- Time
      fit
      printsmcure(fit, Var)
}

###Function to do em
emorig<-function (Time, Status, X, Z, offsetvar, b, beta, model, link,emmax, eps){
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
    uncureprob <- matrix(exp((b) %*% t(Z))/(1 + exp((b) %*% 
                                                      t(Z))), ncol = 1)
    if (model == "ph") {
      survival <- drop(s^(exp((beta) %*% t(X[, -1]))))
    }
    if (model == "aft") {
      error <- drop(log(Time) - beta %*% t(X))
      survival <- s
    }
    w <- Status + (1 - Status) * (uncureprob * survival)/((1 - 
                                                             uncureprob) + uncureprob * survival)
    logistfit <- eval(parse(text = paste("glm", "(", "w~Z[,-1]",",family = quasibinomial(link='", link, "'", ")", ")", sep = "")))
    update_cureb <- logistfit$coef
    if (!is.null(offsetvar)) 
      update_cureb <- as.numeric(eval(parse(text = paste("glm","(", "w~Z[,-1]+offset(offsetvar)", ",family = quasibinomial(link='",link, "'", ")", ")", sep = "")))$coef)
    if (model == "ph") {
      update_beta <- coxph(Surv(Time, Status) ~ X[, -1] + offset(log(w)), subset = w != 0, method = "breslow")$coef
      if (!is.null(offsetvar)) 
        update_beta <- coxph(Surv(Time, Status) ~ X[,-1] + offset(offsetvar + log(w)), subset = w != 0, method = "breslow")$coef
      update_s <- smsurv(Time, Status, X, beta, w, model)$survival
    }
    if (model == "aft") {
      update_beta <- optim(rep(0, ncol(X)), smrank, Time = Time, X = X, n = n, w = w, Status = Status, method = "Nelder-Mead", control = list(reltol = 1e-04, maxit = 500))$par
      update_s <- smsurv(error, Status, X, beta, w, model)$survival
    }
    convergence <- sum(c(update_cureb - b, update_beta - beta)^2) + sum((s - update_s)^2)
    b <- update_cureb
    beta <- update_beta
    s <- update_s
    uncureprob <- matrix(exp((b) %*% t(Z))/(1 + exp((b) %*% t(Z))), ncol = 1)
    i <- i + 1
  }
  em <- list(logistfit = logistfit, b = b, latencyfit = beta,Survival = s, Uncureprob = uncureprob, tau = convergence)
}

emcount<-function (Start,Stop, Status, X, Z, offsetvar, b, beta, model, link,emmax, eps){
  w <- Status
  n <- length(Status)
  if (model == "ph") 
    s <- smsurv(Stop, Status, X, beta, w, model)$survival
  if (model == "aft") {
    if (!is.null(offsetvar)) 
      Stop <- Stop/exp(offsetvar)
    error <- drop(log(Stop) - beta %*% t(X))
    s <- smsurv(error, Status, X, beta, w, model)$survival
  }
  convergence <- 1000
  i <- 1
  while (convergence > eps & i < emmax) {
    uncureprob <- matrix(exp((b) %*% t(Z))/(1 + exp((b) %*% 
                                                      t(Z))), ncol = 1)
    if (model == "ph") {
      survival <- drop(s^(exp((beta) %*% t(X[, -1]))))
    }
    if (model == "aft") {
      error <- drop(log(Time) - beta %*% t(X))
      survival <- s
    }
    w <- Status + (1 - Status) * (uncureprob * survival)/((1 - 
                                                             uncureprob) + uncureprob * survival)
    logistfit <- eval(parse(text = paste("glm", "(", "w~Z[,-1]",",family = quasibinomial(link='", link, "'", ")", ")", sep = "")))
    update_cureb <- logistfit$coef
    if (!is.null(offsetvar)) 
      update_cureb <- as.numeric(eval(parse(text = paste("glm","(", "w~Z[,-1]+offset(offsetvar)", ",family = quasibinomial(link='",link, "'", ")", ")", sep = "")))$coef)
    if (model == "ph") {
      update_beta <- coxph(Surv(Start,Stop, Status) ~ X[, -1] + offset(log(w)), subset = w != 0, method = "breslow")$coef
      if (!is.null(offsetvar)) 
        update_beta <- coxph(Surv(Start,Stop, Status) ~ X[,-1] + offset(offsetvar + log(w)), subset = w != 0, method = "breslow")$coef
      update_s <- smsurv(Stop, Status, X, beta, w, model)$survival
    }
    if (model == "aft") {
      update_beta <- optim(rep(0, ncol(X)), smrank, Time = Stop, X = X, n = n, w = w, Status = Status, method = "Nelder-Mead", control = list(reltol = 1e-04, maxit = 500))$par
      update_s <- smsurv(error, Status, X, beta, w, model)$survival
    }
    convergence <- sum(c(update_cureb - b, update_beta - beta)^2) + sum((s - update_s)^2)
    b <- update_cureb
    beta <- update_beta
    s <- update_s
    uncureprob <- matrix(exp((b) %*% t(Z))/(1 + exp((b) %*% t(Z))), ncol = 1)
    i <- i + 1
  }
  em <- list(logistfit = logistfit, b = b, latencyfit = beta,Survival = s, Uncureprob = uncureprob, tau = convergence)
}


#emcount<-function(Start,Stop, Status, X, Z, offsetvar, b, beta, link, emmax, eps){     
  w <- Status
  n <- length(Status)
  s <- smsurv(Start,Stop,Status,X,beta,w)$survival
  
  #EM Loop
  convergence<- 1000;i <-1
  while (convergence > eps & i < emmax){  
    uncureprob <- matrix(exp((b)%*%t(Z))/(1+exp((b)%*%t(Z))),ncol=1) #initial logit for uncured probability
    survival<-drop(s^(exp((beta)%*%t(X[,-1])))) #initial survival function
    w <- Status+(1-Status)*(uncureprob*survival)/((1-uncureprob)+uncureprob*survival) #initial value for w
    logistfit<- eval(parse(text = paste("glm", "(", "w~Z[,-1]",",family = quasibinomial(link='", link, "'",")",")",sep = "")))
    update_cureb <- logistfit$coef 
    if(!is.null(offsetvar)) update_cureb <- as.numeric(eval(parse(text = paste("glm", "(", "w~Z[,-1]+offset(offsetvar)",",family = quasibinomial(link='", link, "'",")",")",sep = "")))$coef)
    update_beta <- coxph(Surv(Start,Stop,Status)~X[,-1]+offset(log(w)), subset=w!=0, method="breslow")$coef
    if(!is.null(offsetvar)) update_beta <- coxph(Surv(Start,Stop, Status)~X[,-1]+offset(offsetvar+log(w)), subset=w!=0, method="breslow")$coef
    update_s <- smsurv(Stop,Status,X,beta,w)$survival
    convergence<-sum(c(update_cureb-b,update_beta-beta)^2)+sum((s-update_s)^2)
    b <- update_cureb
    beta <- update_beta 
    s<-update_s
    uncureprob <- matrix(exp((b)%*%t(Z))/(1+exp((b)%*%t(Z))),ncol=1)
    i <- i+1
  }
  em <- list(logistfit=logistfit,b=b, latencyfit= beta,Survival=s,Uncureprob=uncureprob,tau=convergence)
}

###Function to calculate survival estimate
  #Todo - should I keep time, or convert to start stop


smsurv<-function(Time, Status, X, beta, w, model){
  death_point <- sort(unique(subset(Time, Status == 1)))
  if (model == "ph") 
    coxexp <- exp((beta) %*% t(X[, -1]))
  lambda <- numeric()
  event <- numeric()
  for (i in 1:length(death_point)) {
    event[i] <- sum(Status * as.numeric(Time == death_point[i]))
    if (model == "ph") 
      temp <- sum(as.numeric(Time >= death_point[i]) * 
                    w * drop(coxexp))
    if (model == "aft") 
      temp <- sum(as.numeric(Time >= death_point[i]) * 
                    w)
    temp1 <- event[i]
    lambda[i] <- temp1/temp
  }
  HHazard <- numeric()
  for (i in 1:length(Time)) {
    HHazard[i] <- sum(as.numeric(Time[i] >= death_point) * 
                        lambda)
    if (Time[i] > max(death_point)) 
      HHazard[i] <- Inf
    if (Time[i] < min(death_point)) 
      HHazard[i] <- 0
  }
  survival <- exp(-HHazard)
  list(survival = survival)
}
