library(spduration)
function (x, t = NULL, ci = TRUE, n = 1000, xvals = NULL, zvals = NULL,
    ...)
{
    if (is.null(t)) {
        max_t <- round(max(x$Y[x$Y[, "last"] == 1, "duration"]) *
            1.2)
        t <- seq(1, max_t, length.out = 100)
    }
    dur.dat <- x$mf.dur
    risk.dat <- x$mf.risk
    X <- model.matrix(attr(x$mf.dur, "terms"), data = x$mf.dur)
    Z <- model.matrix(attr(x$mf.risk, "terms"), data = x$mf.risk)
    beta <- coef(x, model = "duration")
    gamma <- coef(x, model = "risk")
    alpha <- coef(x, model = "distr")
    alpha <- exp(-alpha)
    beta_vcv <- vcov(x, "duration")
    gamma_vcv <- vcov(x, "risk")
    alpha_vcv <- vcov(x, "distr")
    if (is.null(xvals)) {
        X_vals <- apply(X, 2, mean)
    }
    else if (!length(xvals) == ncol(X) && length(xvals) - ncol(X) ==
        -1) {
        stop("Incorrect length for xvals, did you forget 1 for intercept term?")
    }
    else if (!length(xvals) == ncol(X)) {
        stop("Incorrect length for xvals")
    }
    else {
        X_vals <- xvals
    }
    if (is.null(zvals)) {
        Z_vals <- apply(Z, 2, mean)
    }
    else if (!length(zvals) == ncol(Z) && length(zvals) - ncol(Z) ==
        -1) {
        stop("Incorrect length for zvals, did you forget 1 for intercept term?")
    }
    else if (!length(zvals) == ncol(Z)) {
        stop("Incorrect length for zvals")
    }
    else {
        Z_vals <- zvals
    }
    lambda <- exp(-X_vals %*% beta)
    cure <- 1 - plogis(Z_vals %*% gamma)
    ht <- hazard(ti = t, lambda = lambda, cure = cure, alpha = alpha,
        out = NULL, dist = x$distr)
    if (ci == TRUE) {
        Coef_smpl <- MASS::mvrnorm(n = n, mu = coef(x, "full"),
            Sigma = vcov(x, "full"))
        b_idx <- 1:x$n.terms$duration
        g_idx <- (max(b_idx) + 1):(max(b_idx) + x$n.terms$risk)
        a_idx <- (max(g_idx) + 1):ncol(Coef_smpl)
        Beta <- Coef_smpl[, b_idx]
        Gamma <- Coef_smpl[, g_idx]
        A <- Coef_smpl[, a_idx]
        Alpha <- exp(-A)
        lambda <- exp(-tcrossprod(X_vals, Beta))
        cure <- 1 - plogis(tcrossprod(Z_vals, Gamma))
        sims <- matrix(nrow = length(t), ncol = n)
        hmat <- matrix(nrow = length(t), ncol = 3)
        for (i in 1:n) {
            sims[, i] <- hazard(ti = t, lambda = lambda[i], cure = cure[i],
                alpha = Alpha[i], out = NULL, dist = x$distr)
        }
        hmat[, 1] <- ht
        hmat[, 2] <- apply(sims, 1, quantile, probs = 0.05)
        hmat[, 3] <- apply(sims, 1, quantile, probs = 0.95)
        plot(t, hmat[, 1], type = "l", xlab = "Time",
            ylab = "Conditional Hazard", ylim = c(0, max(hmat[,
                3])), ...)
        lines(t, hmat[, 2], lty = 2)
        lines(t, hmat[, 3], lty = 2)
    }
    else {
        plot(t, ht, type = "l", xlab = "Time", ylab = "Conditional hazard",
            ylim = c(0, 1.2 * max(ht)), ...)
    }
    invisible(NULL)
}
<bytecode: 0x000001f9063faa00>
<environment: namespace:spduration>


  # Get model estimates
data(model.coups)

# Plot
plot_hazard(model.coups, ci = FALSE)
plot_hazard(model.coups, ci = TRUE)

