#' Plot baseline survival function.
#'
#' @param object An object returned by the tvcure function.
#' @param ylab Label for the y-axis.
#' @param xlab Label for the x-axis.
#' @param ... Further arguments passed to \link{plot}.
plot_basesurv_tvcure <- function(basesurv_object, ylab = "Baseline Survival Function", xlab = "Time", ...) {
  pdsort <- pred[order(pred[, "Time"]), ]
  plot(pdsort[, "Time"], pdsort[, 1], type = "S", xlab = xlab, ylab = ylab, ...)
}
