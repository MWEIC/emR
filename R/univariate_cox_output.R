#' Calculate univariate cox regression and extract Hazard Ratio and pvalues
#'
#' This function calculates a univaraite cox regression with the \code{coxph} function of the \code{survival}
#' package. Hazard ratios and pvalues are extracted and shown in a table format. If a variable is divided in more
#' than 2 groups the wald test statistic for overall significance is also shown.
#' @param data data.frame or data.table containing survival data.
#' @param time The time interval from start of observation until date of event (e.g. disease progression or death)
#' or censoring.
#' @param status Variable specifying if event occured or data has been censored.
#' @param var Variable tested for Influence on outcome.
#' @export

univariate_cox_output <- function(data, time, status, var){

  formula <- paste("Surv(",time,", ", status,") ~ ", var, sep = "")
  fit_cox <- coxph(as.formula(formula), data = data)
  res_cox <- summary(fit_cox)
  res_ci <- res_cox$conf.int
  names <- rownames(res_ci)

  HR <- paste(round(res_ci[,1],2)," (", round(res_ci[,3],2), "-", round(res_ci[,4],2), ")", sep = "")
  pval.coef <- ifelse(res_cox$coefficients[,5] < 0.001,"<0.001",round(res_cox$coefficients[,5],3))
  pval.wald <- ifelse(res_cox$waldtest[3] < 0.001, "<0.001",round(res_cox$waldtest[3],3))

  out <- data.frame(HR = HR, p = as.character(pval.coef))
  out <- rbind(c(NA,as.character(pval.wald)), out)
  rownames(out) <- c(var,names)
  colnames(out) <- c("HR (95% CI)", "pvalue")

  return(out)
}
