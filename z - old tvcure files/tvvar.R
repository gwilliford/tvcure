# # Todo - fix all the cases where X = 1
# tvvar <- function(X, Z, beta, gamma, nbeta, ngamma, Time, Status, Basehaz, nobs) {
#   X <- X[, -1]
#   xb <- as.matrix(X) %*% beta
#   zg <- as.matrix(Z) %*% gamma  #expxb <- exp(xb)
#   expxb <- exp(xb)
#   expzg <- exp(zg)
#   phi <- (1 - Status) * exp(-Basehaz * expxb + xb + zg) / (1 + exp(-Basehaz * expxb + zg)^2)
#   psi <- (Status + exp(-Basehaz * expxb + zg)) / (1 + exp(-Basehaz * expxb + zg))
#
# # 	deltaH  <- vector(length = nobs)
# # 	for (k in 1:nobs) {
# # 		for (j in 1:nobs) {
# # 		  deltaH[j]  <- Basehaz[j] - Basehaz[j - 1]
# # 		}
# #       #a1[k, j] <- (as.numeric(k == j)/nobs) * sum(a1temp1) - (deltaHj/nobs) * sum(a1temp2)
# # 	}
#   # a1
#   dftemp <- cbind(Basehaz, Time)
#   dftemp <- dftemp[order(Time), ]
#   ordBasehaz <- dftemp[, 1]
#
#   a1      <- matrix(nrow = nobs, ncol = nobs)
#   for (k in 1:nobs){
#     for (j in 1:nobs) {
#       if (j == 1) {
#         deltaH <- ordBasehaz[j]
#       } else {
#         deltaH <- ordBasehaz[j] - ordBasehaz[j - 1]
#       }
#       a1[k, j] <- (as.numeric(k == j) /nobs) * sum(psi * expxb * as.numeric(Time >= Time[k])) -
#         deltaH * sum(phi * expxb * as.numeric(Time >= Time[j] & Time >= Time[k]))
#     }
#   }
#
# 	# a2
# 	a2 <- matrix(nrow = nobs, ncol = nbeta)
# 	for (k in 1:nobs) {
# 		for (j in 1:nbeta) {
# 		  if (nbeta == 1) {
# 		    a2[k, j] <- (1/nobs) * sum((phi * Basehaz - psi) * expxb * as.numeric(Time > Time[k]) * X)
# 		  } else {
# 				a2[k, j] <- (1/nobs) * sum((phi * Basehaz - psi) * expxb * as.numeric(Time > Time[k]) * as.matrix(X[, j]))
# 		  }
# 	  }
# 	}
#
# 	# a3
# 	a3 <- matrix(nrow = nobs, ncol = ngamma)
# 	for (k in 1:nobs) {
# 		for (j in 1:ngamma) {
# 				a3[k, j] <- (-1/nobs) * sum(psi * as.numeric(Time >= Time[k]) * Z[, j])
# 		}
# 	}
#
#   # b1
# 	b1 <- matrix(nrow = ngamma, ncol = ngamma)
# 	for (k in 1:ngamma) {
# 		for (j in 1:ngamma) {
# 			b1[k, j] <- (1/nobs) * sum(((expzg / (1 + expzg)) - psi)^2 * Z[, k] * Z[, j])
# 		}
# 	}
#
# 	# b2
# 	b2 <- matrix(nrow = nbeta, ncol = nbeta)
# 	for (k in 1:nbeta) {
# 	  for (j in 1:nbeta) {
# 	    if (nbeta == 1) {
# 	      b2[k, j] <- (1/nobs) * sum((psi - phi * Basehaz) * Basehaz * expxb * X * X)
# 	    } else {
# 	      b2[k, j] <- (1/nobs) * sum((psi - phi * Basehaz) * Basehaz * expxb * as.matrix(X[, k]) * as.matrix(X[, j]))
# 	    }
#   	}
# 	}
#
# 	# b3
# 	b3 <- matrix(nrow = nbeta, ncol = ngamma)
# 	for (k in 1:nbeta) {
# 	  for (j in 1:ngamma) {
# 	    if (nbeta == 1) {
# 	      b3[k, j] <- (1/nobs) * sum(phi * Basehaz * X * Z[, j])
# 	    } else {
# 	      b3[k, j] <- (1/nobs) * sum(phi * Basehaz * as.matrix(X[, k]) * Z[, j])
# 	    }
# 	  }
# 	}
#
# 	# d
#   dvec <- numeric()
#   for (i in 1:nobs){
#     if (i == 1) {
#       dvec[i] <- ordBasehaz[i]
#     } else {
#       dvec[i] <- ordBasehaz[i] - ordBasehaz[i - 1]
#     }
#   }
#   D <- diag(dvec)
#
#   #browser()
#   # Sigma matrices
#   Sigmab <- solve((b2 - t(a2) %*% D %*% solve(a1) %*% a2) -
#     (b3 - t(a2) %*% D %*% solve(a1) %*% a3) %*%
#     solve(b1 - t(a3) %*% D %*% solve(a1) %*% a3) %*%
#     (t(b3) - t(a3) %*% D %*% solve(a1) %*% a2))
#
#   Sigmag <- solve((b1 - t(a3) %*% D %*% solve(a1) %*% a3) -
#     (t(b3) - t(a3) %*% D %*% solve(a1) %*% a2) %*%
#     solve(b2 - t(a2) %*% D %*% solve(a1) %*% a2) %*%
#     (b3 - t(a2) %*% D %*% solve(a1) %*% a3))
#
# #   for (i in 1:nobs) {
# # 	  a1temp1[i] <- 2 + 2
# #
# # 	  	  psi[i] * expxb[i] *
# # 		}
# # 	}
#   list(ordBasehaz = ordBasehaz)
#   #browser()
# }
