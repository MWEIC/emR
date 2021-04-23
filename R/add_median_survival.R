add_median_survival <- function(data, time, status, var){
  fit <- surv_fit(Surv(eval(parse(text = time)), eval(parse(text = status))) ~ eval(parse(text = var)), data = data)
  surv_med <- surv_median(fit)
  tbl <- data.frame(sapply(1:length(surv_med$median),function(x){
    paste(surv_med$median[x], " (", surv_med$lower[x],"-", surv_med$upper[x],")", sep = "")
  }))
  
  fit <- surv_fit(Surv(eval(parse(text = time)), eval(parse(text = status))) ~ 1, data = data)
  surv_med <- surv_median(fit)
  tmp <- data.frame(sapply(1:length(surv_med$median),function(x){
    paste(surv_med$median[x], " (", surv_med$lower[x],"-", surv_med$upper[x],")", sep = "")
  }))
  
  res <- rbind(tbl, tmp)
  rownames(res) <- c(sort(as.character(unique(data[[var]]))), "Total")
  colnames(res) <- "Median (95% CI)"
  return(res)
}



