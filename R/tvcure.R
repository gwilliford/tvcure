#' Estimate cure models.
#'
#' @param formula
#' @param cureform
#' @param offset
#' @param model
#' @param data
#' @param na.action
#' @param link
#' @param var If
#' @param brglm
#' @param firthcox
#' @param emmax Specifies the maximum number of iterations for the EM algorithm.
#' @param eps
#' @param nboot Specifies the number of bootstrap samples to draw.
#' @param parallel If true, bootstraps will be run in parallel. A progress bar displaying the number of completed boostraps will be displayed. This option requires the user to set up a \link{snow} object and register it using the \link{doSNOW} package (see example below).
tvcure <- function(formula, cureform, offset = NULL, model = "ph", data,
                   na.action = na.omit, link = "logit", var = T, brglm = F,
                   firthcox = F, emmax = 1000, eps = 1e-07, nboot = 100,
                   parallel = T){

  # Preliminaries and error checking--------------------------------------------
    # If parallel is true, ensure that snow cluster is registered
    if (parallel == T) {
      clstatus <- foreach::getDoParRegistered()
      if (clstatus == T) {
        if (getDoParName() == "doSEQ") stop("Please register a snow cluster object to use parallel functionality or set parallel = F.")
      } else stop("Please register a snow cluster object to use parallel functionality or set parallel = F.")
    }

  # Data set up ----------------------------------------------------------------
    call <- match.call()
    model <- match.arg(model)

    # Pull variables from data
    xvars <- all.vars(formula)
    zvars <- all.vars(cureform)

    # Create data frame and apply missing data function
    avars <- unique(c(xvars, zvars))
    data  <- na.action(data[, c(avars)])
    nobs  <- nrow(data)


    browser()
    # Create IV matrices
    mf <- model.frame(formula, data)
    X <- model.matrix(attr(mf, "terms"), mf)
    X <- X[, -1]
    mf2 <- model.frame(cureform, data)
    Z <- model.matrix(attr(mf2, "terms"), mf2)

    #colnames(Z) <- c("(Intercept)", xvars)

    # Set up offset variables
    if (!is.null(offset)) {
      offsetvar <- all.vars(offset)
      offsetvar <- data[, offsetvar]
    } else {
      offsetvar <- NULL
    }

    # Format dependent variable
    Y <- model.extract(mf, "response")
    if (!inherits(Y, "Surv"))
      stop("Response must be a survival object")
    survtype <- attr(Y, "type")
    if (survtype == "right"){
      Time <- Y[, 1]
      Status <- Y[, 2]
      survobj <- Surv(Time, Status)
    } else if (survtype == "counting") {
      Start <- Y[, 1]
      Stop <- Y[, 2]
      Status <- Y[, 3]
      Time <- Stop
      survobj <- Surv(Start, Stop, Status)
    } else stop("tvcure only accepts survival objects of type \"right\" or \"counting\"")
    gnames <- colnames(Z)
    ngamma <- ncol(Z)
    if (model == "ph") {
      bnames <- colnames(X)
      nbeta <- ncol(X)
    }
    cat("tvcure started at "); print(Sys.time());cat("Estimating coefficients...\n")

  # Obtain initial estimates------------------------------------------------------
  w <- Status
  if (brglm) {
    gamma <- eval(parse(text = paste("brglm::brglm", "(", "as.integer(w) ~ Z[, -1],",
                                    "family = binomial(link = '", link, "'", ")",
                                    ")", sep = "")))$coef
  } else {
    gamma <- eval(parse(text = paste("glm", "(", "as.integer(w) ~ Z[, -1],",
                                    "family = binomial(link = '", link, "'", ")",
                                    ")", sep = "")))$coef
  }
  if (model == "ph") {
    # if (firthcox) {
    #   beta <- coxphf(survobj ~ X + offset(log(w)), pl = F)$coefficients
    # } else {
      beta <- coxph(survobj ~ X + offset(log(w)), subset = w!=0,
                    method = "breslow")$coef
    # }
  }
  cat("Initial estimates obtained, beginning em algorithm...\n")

# Call to EM function -------------------------------------------------------
  emfit <- tvem(Time, Start, Stop, Status, X, Z, offsetvar, gamma, beta, model,
                link, emmax, eps, brglm, firthcox, survobj, survtype)
  if (emfit$emrun == emmax) {
    warning("Maximum number of EM iterations reached. Model has not have converged.")
  }
  gamma <- emfit$gamma
  beta <- emfit$latencyfit
  Survival <- emfit$Survival
  Basehaz  <- emfit$Basehaz
  incidence_fit <- emfit$incidence_fit
  cat("Coefficient estimation complete, estimating variance...\n")


  browser()

# Fang et al. standard errors
#varfit <- tvvar(X = X, Z = Z, beta = beta, gamma = gamma, nbeta = nbeta, ngamma = ngamma, Time = Time, Status = Status, Basehaz = Basehaz, nobs = nobs)

# Bootstrap standard errors --------------------------------------------------
if (var) {
  varout <- tvboot(nboot, nbeta, ngamma, survtype, Time, Start, Stop, Status,
                   X, Z, gnames, bnames, offsetvar, gamma, beta, model, link, emmax,
                   eps, brglm, firthcox, survobj, nobs, parallel)
}

# Final fit details
  fit <- list()
  class(fit) <- c("tvcure")
  fit$gamma <- gamma
  fit$beta <- beta
  fit$X <- X
  fit$Z <- Z
  if (var) {
    fit$vcovg <- varout$vcovg
    fit$vcovb <- varout$vcovb
    fit$g_var <- varout$g_var
    fit$g_sd <- varout$g_sd
    fit$g_zvalue <- fit$gamma/fit$g_sd
    fit$g_pvalue <- (1 - pnorm(abs(fit$g_zvalue))) * 2
    fit$b_var <- varout$b_var
    fit$b_sd <- varout$b_sd
    fit$b_zvalue <- fit$beta/fit$b_sd
    fit$b_pvalue <- (1 - pnorm(abs(fit$b_zvalue))) * 2
    fit$bootcomp <- varout$bootcomp
  }
  fit$call <- call
  fit$gnames <- gnames
  fit$gnames[1] <- "Intercept"
  fit$bnames <- bnames
  fit$Survival <- Survival
  fit$BaseHaz  <- Basehaz
  fit$uncureprob <- emfit$uncureprob
  # fit$ordBaseHaz <- varfit$ordBasehaz

  if (survtype == "right")
    fit$Time <- Time
  if (survtype == "counting")
    fit$Time <- Stop
  fit$Status <- Status
  fit$model <- model
  fit$link  <- link
  fit$nfail <- sum(Status)
  fit$nobs  <- nobs
  fit$nboot <- nboot
  fit$emmax <- emmax
  fit$emrun <- emfit$emrun
  fit$var <- var
  fit
  #print_tvcure(fit, var)
}
