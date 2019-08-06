tvXsim <- function(model, variable = "TRT", values) {
  newX <- apply(model$X[, 2:ncol(model$X)], 2, median)
  newX <- matrix(rep(newX, length(values)), ncol = length(newX), byrow = T)
  colnames(newX) <- model$bnames
  newX[, variable] <- values
  newX
}

tvZsim <- function(model, variable = "TRT", values) {
  newZ <- apply(model$Z, 2, median)
  newZ <- matrix(rep(newZ, length(values)), ncol = length(newZ), byrow = T)
  colnames(newZ) <- model$gnames
  newZ[, variable] <- values
  newZ
}
