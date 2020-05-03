tvtable <- function(..., format = c("wide", "long"),
                           qi = c("se", "pvalue", "zscore"), stars = T, digits = 3,
                           modnum = F, varlist = NULL, footnote = NULL)
{
  if (format != "long") stop("Only one model may be specified if format is set to wide. /n")
  browser()
  models <- list(...)
  tables <- list(NULL)
  tabnames <- vector(length = length(models))
  for (i in 1:length(models)) {
    model <- models[[i]]
    if (class(model) == "coxph") {
      assign(paste0("table", i), tvtable.coxph(model, format, qi, stars, digits, varlist))
    } else if (class(model) == "tvcure") {
      assign(paste0("table", i), tvtable.tvcure(model, format, qi, stars, digits, varlist))
    } else {
      stop("Models must be of class coxph or tvcure.")
    }
    # table[i] <- as.data.frame(eval(parse(text = paste0("table", i))))
    # tables <- c(tables, eval(parse(text = paste0("table", i))))
    tables[[i]] <- eval(parse(text = paste0("table", i)))
    tabnames <- paste0("table", i)
  }
  browser()

  if (length(models > 1)) tabout <- tvtable_combine(tabnames, modnum, varlist, footnote)
  browser()
  # else tabout <- tables[[1]]
  tabout
  #
  #   # tables <- list()
  # tabout <- tvtable_combine(as.character(tables), modnum, varlist, footnote)
}

