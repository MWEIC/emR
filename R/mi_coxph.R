#' Coxph with multiple imputed values
#'
#' This code generates a coxph model with multiple imputed values for missing data. Imputation is conducted with the [mice::mice()] function.
#' Pooled results obtained with [mice::pool()] and respective pvalues and 95% CIs are presented as results.
#' @param data data.frame or data.table containing survival data.
#' @param time the time interval from start of observation until date of event (e.g. disease progression or death)
#' or censoring.
#' @param status variable specifying if event occured or data has been censored.
#' @param vars variables tested for Influence on outcome.
#' @inheritParams mice::mice
#' @param prop.var variable for which propensity scores should be calculated. If no value is provided (prop.var = NULL), no weights are used in coxph. Default is NULL.
#' @param ... additional arguments to be passed on to coxph function
#' @export

mi_coxph <- function(data, time, status, vars, prop.var = NULL,  m = 5, ...){
  weights_ate <- NULL

  dat <- as.data.frame(data)
  dat <- dat[,c(vars,time,status)]
  vars_input <- paste(vars, collapse = " + ")
  set.seed(9)
  imp <- mice(dat, m = m)
  imp_comp <- complete(imp, "long")
  fitm <- lapply(1:m, function(x){
    tmp <- imp_comp[imp_comp$.imp == x,]
    if(!is.null(prop.var)) weights_ate <- ate_weights(tmp, vars, prop.var = prop.var)
    coxph(as.formula(paste("Surv(",time, ", ", status,") ~ ", vars_input, sep = "")), weights = weights_ate, data = tmp, ...)
  })
  res <- summary(pool(fitm), conf.int = TRUE)
  res$df <- NULL
  colnames(res) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
  if(!is.null(prop.var)) message("Inverse propensity score weighting was used within cox regression.")
  return(res)
}



