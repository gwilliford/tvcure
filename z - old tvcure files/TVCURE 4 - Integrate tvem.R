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

  # Call to EM function -------------------------------------------------------
    emfit <- tvem(Time, Start, Stop, Status, X, Z, offsetvar, g, beta, model, link, emmax, eps, firthlogit, survobj, survtype)
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
