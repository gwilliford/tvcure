#' Plot baseline survival function.
#'
#' @param object An object returned by the basesurv_tvcure function.
#' @param ylab Label for the y-axis.
#' @param xlab Label for the x-axis.
#' @param ... Further arguments passed to \link{plot}.
plot_basesurv_tvcure <- function(object, ylab = "Baseline Survival Function", xlab = "Time", ...) {
  pred <- cbind(object$s, object$Time)
  pdsort <- pred[order(pred[, 2]), ]
  plot(pdsort[, 2], pdsort[, 1], type = "S", xlab = xlab, ylab = ylab, ...)
}
