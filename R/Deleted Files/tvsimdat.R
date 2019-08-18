tvsimx <- function(model, variable, values) {
  # browser()
  # a <- colnames(model$X %in% model$bnames)
  newX <- apply(model$X[, 2:ncol(model$X)], 2, median)
  newX <- matrix(rep(newX, length(values)), ncol = length(newX), byrow = T)
  colnames(newX) <- model$bnames
  if (variable %in% model$bnames) newX[, variable] <- values
  newX
}

tvsimz <- function(model, variable, values) {
  newZ <- apply(model$Z, 2, median)
  newZ <- matrix(rep(newZ, length(values)), ncol = length(newZ), byrow = T)
  colnames(newZ) <- model$gnames
  if (variable %in% model$gnames) newZ[, variable] <- values
  newZ
}

# tvZsim <- function(model, variable, values) {
#   newZ <- apply(model$Z, 2, median)
#   newZ <- matrix(rep(newZ, length(values)), ncol = length(newZ), byrow = T)
#   colnames(newZ) <- model$gnames
#   newZ[, variable] <- values
#   newZ
# }

# Todo
# Update to use more than just the median
