#' @title Prediction method for tvcure objects
#' @description Computes and plots predicted survival curves and cure probabilities at specified values of independent variables
#' @details Values to compute survival curves and cure probabilities for may be specified in one of two ways. First, specifying the variable and values arguments will compute a separate prediction for all values of the specified variable, with all other variables set to their median. Second, for more complex predictions, matrices containing new values of the hazard and cure variables may be passed using newX and newZ. Values contained in newX and newZ will override any specified variables and values if both are included. By default, the predict function returns a plot of the desired quantity. To access the underlying quantities estimated, set internals = TRUE.
#' @param model A model of class tvcure
#' @param type The desired quantity to compute. "basesurv" returns the baseline survivor function based on the observed values of the parameters. "suncure" returns the conditional baseline survival function, while "spop" returns the population (i.e., unconditional) survival function. "Uncureprob" returns the probability that an observation is susceptible to failure (i.e., is not cured.)
#' @param variable A string containing the name of the variable to plot predicted values for
#' @param values A vector of values of variable at which survival curves should be plotted
#' @param newX A matrix containing values of hazard variables to calculate predictions for
#' @param newZ A matrix containing values of cure variables to calcluate predictions for
#' @param Logical value indicating whether graphs should be printed in black and white.
#' @param xlab A label for the x-axis
#' @param ylab A label for the y-axis
#' @param legendtitle Title for the plot legend
#' @param internals A logical value indicating whether the predictions should be returned. If FALSE, only the graph will be returned.
#' @export

predict.tvcure <- function(model, type = c("basesurv", "suncure", "spop", "uncureprob"),
                           variable = NULL, values = NULL, newX = NULL, newZ = NULL,
                           xlab = "Time", ylab = NULL,
                           legendtitle = NULL, internals = F) {

  # Setup -------------------------------------------------------------------

  require(ggplot2)
  call <- match.call()
  if (!inherits(model, "tvcure")) stop("Model must be a tvcure object")
  s0 = as.matrix(model$Survival, ncol = 1)
  nobs = nrow(s0)
  beta <- model$beta
  gamma <- model$gamma
  bnames <- model$bnames
  gnames <- model$gnames
  link <- model$link
  Status <- model$Status
  Time <- model$Time[order(model$Time)]
  X <- model$X
  Z <- model$Z
  if (is.null(legendtitle)) {
    legendtitle <- variable
  }
  type = match.arg(type)


  # Create dataset for predictions -----------------------------------------------------------
  if (is.null(newX)) {
    newX <- apply(X, 2, median)
    newX <- matrix(rep(newX, length(values)), ncol = length(newX), byrow = T)
    colnames(newX) <- bnames
    if (variable %in% bnames) newX[, variable] <- values
  }
  newX <- as.matrix(newX)
  nx <- nrow(newX)

  if (is.null(newZ)) {
    newZ <- apply(Z, 2, median)
    newZ <- matrix(rep(newZ, length(values)), ncol = length(newZ), byrow = T)
    colnames(newZ) <- gnames
    if (variable %in% gnames) newZ[, variable] <- values
  }
  newZ <- as.matrix(newZ)
  nz <- nrow(newZ)



  # Create predictions ---------------------------------------------------------
  if (link == "logit") {
    uncureprob <- exp(gamma %*% t(newZ)) / (1 + exp(gamma %*% t(newZ)))
  }
  if (link == "probit") {
    uncureprob <- pnorm(gamma %*% t(newZ))
  }

  suncure = array(0, dim = c(nobs, nx))
  ebetaX = exp(model$beta %*% t(newX))
  for (i in 1:nx) {
    suncure[, i] = s0^ebetaX[i]
  }

  spop = array(0, dim = c(nobs, nrow(newX)))
  for (i in 1:nobs) {
    for (j in 1:nrow(newX)) {
      spop[i, j] = uncureprob[j] * suncure[i, j] + (1 - uncureprob[j])
    }
  }
  s0      <- s0[order(s0, decreasing = T)]
  suncure <- suncure[order(suncure[, 1], decreasing = T), ]
  spop    <- spop[order(spop[, 1], decreasing = T), ]



  # Plot Setup --------------------------------------------------------------
  splot <- ggplot() + theme(panel.border = element_rect(colour = "black", fill = NA))
  if (is.null(ylab)) {
    if (type == "uncureprob") {
      ylab = "Probability Subject is Susceptible"
    } else {
      ylab = "Probability of Survival"
    }
  }

  # Uncureprob Plot ---------------------------------------------------------
  if (type == "uncureprob") {
    splot <- ggplot(mapping = aes(x = values, y = as.vector(uncureprob))) + geom_point()
    splot <- splot + scale_x_continuous(breaks = values) +
      ylab(ylab) + xlab(xlab) + theme(legend.position = "none") + theme_bw()
  }

  # Basesurv Plot -----------------------------------------------------------

  if (type == "basesurv") {
    splot <- splot + geom_step(mapping = aes(Time, s0), size = 1.25)
    splot = splot + ylab(ylab) + xlab(xlab)
  }

  # Suncure Plot ------------------------------------------------------------
  if (type == "suncure") {

    scm  <- split(suncure, rep(1:ncol(suncure), each = nrow(suncure)))
    for (i in 1:length(scm)) {
      scm[[i]] <- cbind(scm[[i]], Time, num = i)
    }
    scf <- as.data.frame(do.call(rbind, scm))
    colnames(scf) <- c("scm", "Time", "num")

    splot = splot + geom_step(scf,
                              mapping = aes(Time, scm, col = as.factor(num),
                                                 linetype = as.factor(num)),
                              size = 1.25) +
      labs(linetype = legendtitle, col = legendtitle)
    splot = splot + ylab(ylab) + xlab(xlab)
  }

  # Spop Plot ---------------------------------------------------------------
  if (type == "spop") {

    spm  <- split(spop, rep(1:ncol(spop), each = nrow(spop)))
    for (i in 1:length(spm)) {
      spm[[i]] <- cbind(spm[[i]], Time, num = i)
    }
    spf <- as.data.frame(do.call(rbind, spm))
    colnames(spf) <- c("spm", "Time", "num")

    splot = splot +
      geom_step(spf, mapping = aes(Time, spm, col = as.factor(num),
                                   linetype = as.factor(num)), size = 1) +
      labs(linetype = legendtitle, col = legendtitle, fill = legendtitle)
    splot
  }

  # Output ------------------------------------------------------------------
  if (internals == F) {
    return(splot)
  } else {
    structure(list(uncureprob = uncureprob,
                   s0 = s0, suncure = suncure, spop = spop,
                   Survival = model$Survival,
                   link = link, Time = Time,
                   newX = newX, newZ = newZ, variable = variable, splot = splot))
  }
}
