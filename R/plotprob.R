plotprob = function(object, plottype = c("points", "line"),
                    values = NULL,
                    legendtitle = NULL, ylab = "Probability of Failure", xlab = "Time") {

  # Setup -------------------------------------------------------------------

  require(ggplot2)
  plottype = match.arg(plottype)
  failtime = object$failtime
  if (is.null(values)) values = 1
  CI = F

  # Plot Setup --------------------------------------------------------------
  splot = ggplot() + theme_bw()
  uncureprob = object$uncureprob

  # Uncureprob Plot ---------------------------------------------------------
    # if (CI == F) {
    splot = ggplot(mapping = aes(x = values, y = as.vector(uncureprob)))
    if (plottype == "points") splot = splot + geom_point()
    if (plottype == "line")   splot = splot + geom_line()
    # } else {
    #   splot = ggplot(mapping = aes(x = values, y = as.vector(uncuremean))) + geom_point() +
    #     geom_errorbar(width = (length(values)/10),
    #                   mapping = aes(x = values, ymin = uncurelo, ymax = uncurehi),
    #                   colour = "black")
    # }
    splot = splot + scale_x_continuous(breaks = values) + theme(legend.position = "none")
    splot = splot + ylab(ylab) + xlab(xlab)
    splot
}


