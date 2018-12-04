residuals.tvcure <- function(model) {
  if (!inherits(model, "tvcure"))
    stop("Model must be a tvcure object")
  w <- as.vector(model$Status + (1 - model$Status) * (model$uncureprob * model$Survival)/((1 -
        model$uncureprob) + model$uncureprob * model$Survival))
  H <- model$BaseHaz
  expxb <- as.numeric(exp(model$beta %*% t(model$X[, -1])))
  martingale <- model$Status - w * H * expxb
  score <- rowSums(model$X[, -1] * martingale)
  structure(list(martingale = martingale, score = score))
}
