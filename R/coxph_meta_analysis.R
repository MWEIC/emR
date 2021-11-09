#' Meta analysis forest plot
#'
#' This code generates a forestplot from a meta analysis coxph model.
#' @param data data
#' @param time the time interval from start of observation until date of event (e.g. disease progression or death)
#' or censoring.
#' @param status variable specifying if event occured or data has been censored.
#' @param vars variables tested for Influence on outcome.
#' @param var grouping variable
#' @param meta.group variable for which meta analysis should be conducted. Usually the outcome of interest (e.g. treatment).
#' @param univariate Logical value. If TRUE output of univariate cox regression is printed. Else output of multivariate
#' cox regression is printed. Default is FALSE.
#' @param ... additional arguments passed on to coxph
#' @export

coxph_meta_analysis <- function(data, time, status,vars, var, meta.group, univariate = FALSE, ...){
  res <- lapply(1:length(levels(data[[var]])), function(x){

    if(univariate == FALSE){
      vars_coxph <- c(vars[vars != var], meta.group)
      vars_input <- paste(vars_coxph, collapse = " + ")
    } else {
      vars_input <- meta.group
    }

    dat <- data[eval(parse(text = var)) == levels(data[[var]])[x]]
    fit <- coxph(as.formula(paste("Surv(", time,", ", status,") ~ ", vars_input, sep = "")), data = dat, ...)
    df <- as.data.frame(broom::tidy(fit, conf.int = TRUE))
    df$var <- var
    df$level <- levels(data[[var]])[x]
    df$N <- dim(dat)[1]
    df

  })
  return(res)
}






