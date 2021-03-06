#' Fit a proportional hazards cure model
#'
#' Here is a description
#'
#' The coefficients in the cure equation are parameterized in terms of the probability of being susceptible to an event. Positive coefficients indicate that a variable is associated with higher susceptibility to experiencing the event of interest (i.e., a lower probability of being cured).
#' @param survform A formula for the hazard function. Must have a Surv object on the right-hand side of type "right" or "counting".
#' @param cureform A formula for the cure function. Must begin with a tilde followed by variables to include in the equation
#' @param data A data frame containing the data to be used in estimation.
#' @param na.action Specifies how missing data should be handled
#' @param offset Specify an offset variable
#' @param link Link function for the cure equation. Either "logit" or "probit".
#' @param brglm Logical value indicating whether bias-reduced logistic regression should be used to estimate the cure equation
#' @param var Logical value indicating whether standard errors should be estimated.
#' @param nboot The number of bootstrap samples to draw for estimating standard errors.
#' @param parallel Logical value indicating whether bootstrap replications should be run using parallel processing. This option requires the user to set up a  object and register it using the  package.
#' @param emmax Specifies the maximum number of iterations for the EM algorithm.
#' @param eps Convergence criterion
# \link{[doSNOW]{doSNOW}}
# \link{[snow]{snow}}
# \link{na.action}
tvcure = function(survform, cureform, data, subset = NULL,
                   na.action = na.omit, offset = NULL,
                   link = "logit", brglm = T,
                   var = T, nboot = 100, parallel = T,
                   emmax = 1000, eps = 1e-07) {



# Preliminaries and error checking--------------------------------------------
  # If parallel is true, ensure that snow cluster is registered
  if (parallel == T & var == T) {
    clstatus = foreach::getDoParRegistered()
    if (clstatus == T) {
      if (getDoParName() == "doSEQ")
        stop("Please register a snow cluster object to use parallel functionality or set parallel = F.")
    } else stop("Please register a snow cluster object to use parallel functionality or set parallel = F.")
  }
  # if (link != "logit" | link != "probit")
  #   stop ("Link function must be either logit or probit")

# Data set up ----------------------------------------------------------------
  call = match.call()

  method = ifelse(brglm, "brglmFit", "glm.fit")
  if (brglm) require(brglm2)

  # Create data frame and apply missing data function
  if (!is.null(subset)) {
    data = subset(data, subset)
  }
  xvars = all.vars(survform)
  zvars = all.vars(cureform)
  avars = unique(c(xvars, zvars))
  data  = na.action(data[, c(avars)])
  nobs  = nrow(data)

  # Create IV matrices
  mf = model.frame(survform, data)
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
  # w = rep(1, length(Time))
  # w[Status == 0] = seq(1, 0, along = w[Status == 0])
  w = Status
  gamma = eval(parse(text = paste("glm", "(", "as.integer(w) ~ Z[, -1],",
                                   "family = binomial(link = '", link, "'", "), ",
                                   "method = '", method, "'",
                                   ")", sep = "")))$coef
  uncuremod = coxph(survobj ~ X + offset(log(w)), subset = w != 0, method = "breslow")
  beta = uncuremod$coef
  cat("Initial estimates obtained, beginning em algorithm...\n")

# Call to EM function -------------------------------------------------------
  emfit <- tvem(Time, Status, X, Z, w, offset, gamma, beta,
                link, emmax, eps, brglm, survobj, survtype, method)
  if (emfit$emrun == emmax) {
    warning("Maximum number of EM iterations reached. Estimates have not have converged.")
  }
  gamma = emfit$gamma
  beta = emfit$beta
  cat("Coefficient estimation complete, estimating variance...\n")


# Bootstrap standard errors --------------------------------------------------
  if (var) {
    varout <- tvboot(nboot, nbeta, ngamma, survtype, Time, Start, Stop, Status,
                     X, Z, w, gnames, bnames, offset, gamma, beta, link, emmax,
                     eps, brglm, survobj, nobs, parallel, method)
  }

# Final fit details ------------------------------------------------------------
  fit = list()
  class(fit) = "tvcure"
  fit$uncuremod = uncuremod
  fit$curemod$incidence_fit = emfit$incidence_fit
  fit$curemod$latency_fit = emfit$latency_fit
  fit$gamma = gamma
  fit$beta = beta
  fit$X = X
  fit$Z = Z
  if (var) {
    fit$vcovg = varout$vcovg
    fit$vcovb = varout$vcovb
    fit$g_var = varout$g_var
    fit$g_sd = varout$g_sd
    fit$g_zvalue = fit$gamma/fit$g_sd
    fit$g_pvalue = (1 - pnorm(abs(fit$g_zvalue))) * 2
    fit$b_var = varout$b_var
    fit$b_sd = varout$b_sd
    fit$b_zvalue = fit$beta/fit$b_sd
    fit$b_pvalue = (1 - pnorm(abs(fit$b_zvalue))) * 2
    fit$bootcomp = varout$bootcomp
  }
  fit$call = call
  fit$gnames = gnames
  fit$gnames[1] = "Intercept"
  fit$bnames = bnames
  fit$w = emfit$w
  fit$Survival = emfit$Survival
  fit$uncureprob = emfit$uncureprob
  # fit$ordBaseHaz = varfit$ordBasehaz
  fit$Time = Time
  fit$Status = Status
  fit$link  = link
  fit$nfail = sum(Status)
  fit$nobs  = nobs
  fit$nboot = nboot
  fit$emmax = emmax
  fit$emrun = emfit$emrun
  fit$options$var = var
  fit$loglik = emfit$loglik
  summary.tvcure(fit)
  return(fit)
}
