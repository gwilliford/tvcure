#' Calculate baseline survival function
#'
#' @param object An object returned by the tvcure function.

basesurv_tvcure <- function(object) {
  if (!inherits(object, "tvcure"))
    stop("Object must be results of tvcure")
  call <- match.call()
  s0 = as.matrix(object$s, ncol = 1)
  n = nrow(s0)
  structure(list(call = call, s0 = s0, n = n))
}

