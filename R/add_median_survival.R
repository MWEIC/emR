#' Calculate median survival time
#'
#' Fitting a survival fit for each subgroup defined by var. Additionally the total survival time for the 
#' whole sample is calculated. 
#' @param data data.frame or data.table containing survival data.
#' @param time The time interval from start of observation until date of event (e.g. disease progression or death)
#' or censoring. 
#' @param status Variable specifying if event occured or data has been censored. Default behaviour inherited from the \code{surv_fit} function in the \code{survival} package, with 0 
#' indicating censored data and 1 indicating event.
#' @param var Variable tested for Influence on outcome.
#' @export

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



