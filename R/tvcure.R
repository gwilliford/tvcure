tvcure <- function(formula, cureform, offset = NULL, model=c("ph","aft"), data,
                   na.action = na.omit, link = "logit", var = T, firthlogit = F,
                   firthcox = FALSE, emmax = 50, eps = 1e-07, nboot = 100,
                   parallel = T){

  # Data set up ----------------------------------------------------------------
    call <- match.call()
    model <- match.arg(model)
    cat("tvcure started at ");print(Sys.time());cat("Estimating coefficients...\n")
    #data <- na.omit(data)
    lvars <- all.vars(formula,)
    cvars <- all.vars(cureform)
    avars <- unique(c(lvars,cvars))
    data <- na.action(data[,c(avars)])
    n <- dim(data)[1]
    mf <- model.frame(formula, data)
    Z <- as.matrix(cbind(rep(1, n), data[, cvars]))
    colnames(Z) <- c("(Intercept)", cvars)
    if (!is.null(offset)) {
      offsetvar <- all.vars(offset)
      offsetvar <- data[, offsetvar]
    } else {
      offsetvar <- NULL
    }
    Y <- model.extract(mf, "response")
    X <- model.matrix(attr(mf, "terms"), mf)
    if (parallel == T) {
        clstatus <- getDoParRegistered()
        if (clstatus == F) stop("Please register a cluster
                                object to use parallel functionality.")
    }
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

# Obtain initial estimates------------------------------------------------------
  w <- Status
  w[w==0]<-.001
  if (firthlogit) {
    gamma <- logistf(w~Z[,-1])$coef
  } else {
    gamma <- eval(parse(text = paste("glm", "(", "w~Z[,-1]", ",
                                 family = quasibinomial(link='", link, "'", ")",
                                 ")", sep = "")))$coef
  }
  if (model == "ph") {
    if (firthcox) {
      beta <- coxphf(survobj ~ X[, -1] + offset(log(w)), pl=F)$coefficients
    } else {
      beta <- coxph(survobj ~ X[, -1] + offset(log(w)), subset = w!=0,
                    method = "breslow")$coef
    }
  }
  if (model == "aft")
    beta <- survreg(survobj ~ X[, -1])$coef
  cat("Initial cox estimates obtained, beginning em algorithm...\n")

# Call to EM function -------------------------------------------------------
  emfit <- tvem(Time, Start, Stop, Status, X, Z, offsetvar, gamma, beta, model,
                link, emmax, eps, firthlogit, firthcox, survobj, survtype)
  gamma <- emfit$gamma
  beta <- emfit$latencyfit
  s <- emfit$Survival
  incidence_fit <- emfit$emfit
  cat("Coefficient estimation complete, estimating variance...\n")

# Bootstrap standard errors --------------------------------------------------
  if (var) {
    varout <- tvboot(nboot, nbeta, ngamma, survtype, Time, Start, Stop, Status,
                     X, Z, gnames, bnames, offsetvar, gamma, beta, model, link, emmax,
                     eps, firthlogit, firthcox, survobj, n, parallel)
  } # close bootstrap bracket

# Final fit details
  fit <- list()
  class(fit) <- c("tvcure")
  fit$incidence_fit <- incidence_fit
  fit$gamma <- gamma
  fit$beta <- beta
  if (var) {
    fit$g_var <- varout$g_var
    fit$g_sd <- varout$g_sd
    fit$g_zvalue <- fit$gamma/fit$g_sd
    fit$g_pvalue <- (1 - pnorm(abs(fit$g_zvalue))) * 2
    fit$b_var <- varout$b_var
    fit$b_sd <- varout$b_sd
    fit$b_zvalue <- fit$beta/fit$b_sd
    fit$b_pvalue <- (1 - pnorm(abs(fit$b_zvalue))) * 2
  }
  cat("tvcure finished running at ");print(Sys.time())
  fit$call <- call
  fit$gnames <- gnames
  fit$bnames <- bnames
  fit$s <- s
  if (survtype == "right") fit$Time <- Time
  if (suvrtype == "counting") fit$Stop <- Time
  fit$model <- model
  fit
  printtvcure(fit, var)
}
