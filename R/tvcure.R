#' Estimate cure models.
#'
#' @param formula
#' @param cureform
#' @param offset
#' @param data
#' @param na.action
#' @param link
#' @param var If
#' @param brglm
#' @param emmax Specifies the maximum number of iterations for the EM algorithm.
#' @param eps
#' @param nboot Specifies the number of bootstrap samples to draw.
#' @param parallel If true, bootstraps will be run in parallel. A progress bar displaying the number of completed boostraps will be displayed. This option requires the user to set up a \link{snow} object and register it using the \link{doSNOW} package (see example below).
tvcure <- function(survform, cureform, link = "logit",
                   data, na.action = na.omit, offset = NULL, subset = NULL,
                   var = T, nboot = 100,
                   parallel = T,
                   brglm = F,
                   emmax = 1000, eps = 1e-07)
{
  # Preliminaries and error checking--------------------------------------------
    # If parallel is true, ensure that snow cluster is registered
    if (parallel == T & var == T) {
      clstatus <- foreach::getDoParRegistered()
      if (clstatus == T) {
        if (getDoParName() == "doSEQ") stop("Please register a snow cluster object to use parallel functionality or set parallel = F.")
      } else stop("Please register a snow cluster object to use parallel functionality or set parallel = F.")
    }

  # Data set up ----------------------------------------------------------------
    call <- match.call()
    if (!is.null(subset)) {
      data <- subset(data, subset)
    }

    method <- ifelse(brglm, "brglmFit", "glm.fit")
    if (brglm) require(brglm2)

    # Create data frame and apply missing data function
    xvars <- all.vars(survform)
    zvars <- all.vars(cureform)
    avars <- unique(c(xvars, zvars))
    data  <- na.action(data[, c(avars)])
    nobs  <- nrow(data)

    # Create IV matrices
    mf <- model.frame(survform, data)
    X <- model.matrix(attr(mf, "terms"), mf)
    if (ncol(X) == 2) {
      X1 <- X[, -1]
      X1 <- matrix(X1, ncol = 1)
      colnames(X1) <- colnames(X)[2]
      X <- X1
    } else {
      X <- X[, -1]
    }
    bnames <- colnames(X)
    nbeta <- ncol(X)

    mf2 <- model.frame(cureform, data)
    Z <- model.matrix(attr(mf2, "terms"), mf2)
    gnames <- colnames(Z)
    ngamma <- ncol(Z)

    # Set up offset variables
    if (!is.null(offset)) {
      offset <- all.vars(offset)
      offset <- data[, offset]
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

    cat("tvcure started at "); print(Sys.time()); ("Estimating coefficients...\n")

  # Obtain initial estimates------------------------------------------------------
  browser()
  w = rep(1, length(Time))
  w[Status == 0] = seq(1, 0, along = w[Status == 0])
  # w <- Status
  gamma <- eval(parse(text = paste("glm", "(", "as.integer(w) ~ Z[, -1],",
                                   "family = binomial(link = '", link, "'", "), ",
                                   "method = '", method, "'",
                                   ")", sep = "")))$coef
  uncuremod <- coxph(survobj ~ X + offset(log(w)), subset = w != 0, method = "breslow")
  beta <- uncuremod$coef
  cat("Initial estimates obtained, beginning em algorithm...\n")

# Call to EM function -------------------------------------------------------
  emfit <- tvem(Time, Status, X, Z, offset, gamma, beta,
                link, emmax, eps, brglm, survobj, survtype, method)
  if (emfit$emrun == emmax) {
    warning("Maximum number of EM iterations reached. Estimates have not have converged.")
  }
  gamma <- emfit$gamma
  beta <- emfit$beta
  Survival <- emfit$Survival
  Basehaz  <- emfit$Basehaz
  cat("Coefficient estimation complete, estimating variance...\n")


# Bootstrap standard errors --------------------------------------------------
if (var) {
  varout <- tvboot(nboot, nbeta, ngamma, survtype, Time, Start, Stop, Status,
                   X, Z, gnames, bnames, offset, gamma, beta, link, emmax,
                   eps, brglm, survobj, nobs, parallel, method)
}

# Final fit details
  fit <- list()
  class(fit) <- "tvcure"
  fit$uncuremod <- uncuremod
  fit$curemod$incidence_fit <- emfit$incidence_fit
  fit$curemod$latency_fit <- emfit$latency_fit
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
  fit$w <- emfit$w
  fit$Survival <- emfit$Survival
  fit$BaseHaz  <- Basehaz
  fit$uncureprob <- emfit$uncureprob
  # fit$ordBaseHaz <- varfit$ordBasehaz
  fit$Time <- Time
  fit$Status <- Status
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
