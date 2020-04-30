tvtable <- function(..., format = c("wide", "long"),
                           qi = c("se", "pvalue", "zscore"), stars = T, digits = 3,
                           modnum = F, varlist = NULL, footnote = NULL) 
{
  models <- list(...)
  for (i in 1:length(models)) {
    browser()
    model <- models[[i]]
    if (class(model) == "coxph") {
      assign(paste0("table", i), as.data.frame(tvtable.coxph(model, format, qi, stars, digits, varlist)))
    } else if (class(model) == "tvcure") {
      assign(paste0("table", i), as.data.frame(tvtable.tvcure(model, format, qi, stars, digits, varlist)))
    } else { 
      stop("Models must be of class coxph or tvcure.")
    }
    table[i] <- as.data.frame(eval(parse(text = paste0("table", i)))
    # tables <- c(tables, eval(parse(text = paste0("table", i))))
  }
  tabout <- tvtable_combine(tables, modnum, varlist, footnote)
  if (length(models > 1)) 
    tables <- list()
  tabout <- tvtable_combine(as.character(tables), modnum, varlist, footnote)
  else tabout <- tabout1
}

