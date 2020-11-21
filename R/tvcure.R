#' Estimate cure models.
#'
#' @param formula Formula specifying variables used to model the hazard rate. The response variable must be a \link{survival} object.
#' @param cureform Formula specifying variables used to model the cure rate. Must start with a tilde followed by  independent variables.
#' @param link Link function to use for generalized linear model. Only "logit" and "probit" are supported.
#' @param data A data.frame containing the variables in the model.
#' @param na.action Indicates how missing values are handled.
#' @param offset An optional numeric vector that specifies an offset term.
#' @param subset An optional vector identifying which observations should be used.
#' @param var If True, estimates variance using bootstrap resampling.
#' @param nboot Specifies the number of bootstrap samples for variance estimation.
#' @param parallel If True, variance estimation will be run using parallel processing. Requires the user to set up a \link{snow} object and register it using the \link{doSNOW} package.
#' @param brglm Estimate bias-reduced
#' @param emmax Specifies the maximum number of iterations to run.
#' @param eps A positive number. Model converges when the change in the log-likelihood between iterations is less than this value.

tvcure = function(formula, cureform, link = "logit",
                   data, na.action = na.omit, offset = NULL, subset = NULL,
                   var = T, nboot = 100, parallel = T,
                   brglm = F,
                   emmax = 1000, eps = 1e-07)
{
  # Preliminaries and error checking--------------------------------------------
    # If parallel is true, ensure that snow cluster is registered
    if (parallel == T) {
      clstatus = foreach::getDoParRegistered()
      if (clstatus == T) {
        if (getDoParName() == "doSEQ") stop("Please register a snow cluster object to use parallel functionality or set parallel = F.")
      } else stop("Please register a snow cluster object to use parallel functionality or set parallel = F.")
    }

  # Data set up ----------------------------------------------------------------
    call = match.call()
    if (!is.null(subset)) {
      data = subset(data, subset)
    }

    method = ifelse(brglm, "brglmFit", "glm.fit")
    if (brglm) require(brglm2)

    # Pull variables from data
    xvars = all.vars(formula)
    zvars = all.vars(cureform)

    # Create data frame and apply missing data function
    avars = unique(c(xvars, zvars))
    data  = na.action(data[, c(avars)])
    nobs  = nrow(data)

    # Create IV matrices
    mf = model.frame(formula, data)
    X = model.matrix(attr(mf, "terms"), mf)
    if (ncol(X) == 2) {
      X1 = X[, -1]
      X1 = matrix(X1, ncol = 1)
      colnames(X1) = colnames(X)[2]
      X = X1
    } else {
      X = X[, -1]
    }
    bnames = colnames(X)
    nbeta = ncol(X)

    mf2 = model.frame(cureform, data)
    Z = model.matrix(attr(mf2, "terms"), mf2)
    gnames = colnames(Z)
    ngamma = ncol(Z)

    # Set up offset variables
    if (!is.null(offset)) {
      offset = all.vars(offset)
      offset = data[, offset]
    }

    # Format dependent variable
    Y = model.extract(mf, "response")
    if (!inherits(Y, "Surv"))
      stop("Response must be a survival object")
    survtype = attr(Y, "type")
    if (survtype == "right"){
      Time = Y[, 1]
      Status = Y[, 2]
      survobj = Surv(Time, Status)
    } else if (survtype == "counting") {
      Start = Y[, 1]
      Stop = Y[, 2]
      Status = Y[, 3]
      Time = Stop
      survobj = Surv(Start, Stop, Status)
    } else stop("tvcure only accepts survival objects of type \"right\" or \"counting\"")

    cat("tvcure started at "); print(Sys.time()); ("Estimating coefficients...\n")

  # Obtain initial estimates------------------------------------------------------
  w = Status
  gamma = eval(parse(text = paste("glm", "(", "as.integer(w) ~ Z[, -1],",
                                   "family = binomial(link = '", link, "'", "), ",
                                   "method = '", method, "'",
                                   ")", sep = "")))$coef
  # if (firthcox) {
  #   firthdat = subset(X, w != 0)
  #   firthobj = subset(survobj, w != 0)
  #   beta = coxphf::coxphf(firthobj ~ X + offset(log(w)))$coef
  # } else {
    uncuremod = coxph(survobj ~ X + offset(log(w)), subset = w != 0, method = "breslow")
    beta = uncuremod$coef
  # }
  cat("Initial estimates obtained, beginning em algorithm...\n")

# Call to EM function -------------------------------------------------------
  emfit = tvem(Time, Status, X, Z, offset, gamma, beta,
                link, emmax, eps, brglm, firthcox, survobj, survtype, method)
  if (emfit$emrun == emmax) {
    warning("Maximum number of EM iterations reached. Estimates have not have converged.")
  }
  gamma = emfit$gamma
  beta = emfit$beta
  Survival = emfit$Survival
  Basehaz  = emfit$Basehaz
  cat("Coefficient estimation complete, estimating variance...\n")

# Fang et al. standard errors
#varfit = tvvar(X = X, Z = Z, beta = beta, gamma = gamma, nbeta = nbeta, ngamma = ngamma, Time = Time, Status = Status, Basehaz = Basehaz, nobs = nobs)

# Bootstrap standard errors --------------------------------------------------
if (var) {
  varout = tvboot(nboot, nbeta, ngamma, survtype, Time, Start, Stop, Status,
                   X, Z, gnames, bnames, offset, gamma, beta, link, emmax,
                   eps, brglm, firthcox, survobj, nobs, parallel, method)
}

# Final fit details
  fit = list()
  class(fit) = "tvcure"

  # Standard cox model object
  fit$uncuremod = uncuremod

  # Cure model object
  fit$curemod$incidence_fit = emfit$incidence_fit
  fit$curemod$latency_fit   = emfit$latency_fit
  fit$curemod$w             = emfit$w
  fit$curemod$uncureprob    = emfit$uncureprob

  # Data
  fit$data$X = X
  fit$data$Z = Z
  fit$data$Time = Time
  fit$data$Status = Status

  # Coefficients
  fit$parameters$gamma = gamma
  fit$parameters$beta = beta

  # Variance parameters$
  if (var) {
    fit$parameters$vcovg    = varout$vcovg
    fit$parameters$vcovb    = varout$vcovb
    fit$parameters$g_var    = varout$g_var
    fit$parameters$g_sd     = varout$g_sd
    fit$parameters$g_zvalue = fit$parameters$gamma/fit$parameters$g_sd
    fit$parameters$g_pvalue = (1 - pnorm(abs(fit$parameters$g_zvalue))) * 2
    fit$parameters$b_var    = varout$b_var
    fit$parameters$b_sd     = varout$b_sd
    fit$parameters$b_zvalue = fit$parameters$beta/fit$parameters$b_sd
    fit$parameters$b_pvalue = (1 - pnorm(abs(fit$parameters$b_zvalue))) * 2
    fit$bootcomp            = varout$bootcomp
  }

  # call
  fit$call = call
  fit$formula = formula
  fit$terms = terms(formula)

  # varnames
  fit$varnames$gnames = gnames
  fit$varnames$gnames[1] = "Intercept"
  fit$varnames$bnames = bnames

  # options
  fit$options$var   = var
  fit$options$brglm = brglm
  fit$options$link  = link
  fit$options$emmax = emmax
  fit$options$emrun = emfit$emrun

  # Survival and hazard functions
  fit$Survival = emfit$Survival
  fit$BaseHaz  = Basehaz

  # Summary statistics
  fit$nfail = sum(Status)
  fit$nobs  = nobs
  fit$nboot = nboot

  fit
  # print_tvcure(fit, var)
}
