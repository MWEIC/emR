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
#' @param ... additional arguments to be passed on to coxph function
#' @export



mi_coxph <- function(data, time, status, vars,  m = 5, ...){
  dat <- as.data.frame(data)
  dat <- dat[,c(vars,time,status)]
  vars_input <- paste(vars, collapse = " + ")
  set.seed(9)
  imp <- mice(dat, m = m)
  fitm <- with(imp, coxph(as.formula(paste("Surv(",time, ", ", status,") ~ ", vars_input, sep = "")), ...))
  res <- summary(pool(fitm), conf.int = TRUE)
  res$df <- NULL
  colnames(res) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
  return(res)
}



     