

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