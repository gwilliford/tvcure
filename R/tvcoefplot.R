tvcoefplot <- function(model, parameters, varnames = NULL, ...) {
  if (parameters == "beta") {
    if (is.null(varnames)) varnames <- model$bnames
    arm::coefplot(model$beta, model$b_sd, varnames = varnames, main = "Hazard Coefficients", ...)
  }
  if (parameters == "gamma") {
    if (is.null(varnames)) varnames <- model$gnames
    arm::coefplot(model$gamma, model$g_sd, varnames = varnames, main = "Incidence Coefficients", ...)
  }
}

#tvcoefplot(lhr_tvcurefull, "gamma", col.pts = "blue")
#tvcoefplot(lhr_tvcurefull, "beta", col.pts = "blue")
#coefplot(mcox$coefficients, sqrt(diag(mcox$var)), col.pts = "red", add = T)
#coefplot(mcoxb$coefficients, sqrt(diag(mcox$var)), col.pts = "green", add = T, offset = .02)
