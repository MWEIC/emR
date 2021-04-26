#' Transform median survival data to htmltable format
#'
#' Wrapper around \code{add_median_survival} to transform the data to htmltable format in order to be used in display
#' with \code{htmlTable::htmlTable}. If time and status of 
#' @param data data.frame or data.table 
#' @param timePFS Time for PFS
#' @param timeOS Time for OS
#' @param statusPFS censor variable for PFS
#' @param statusOS censor variable for OS
#' @param var Variable tested for Influence on outcome.
#' @export

median_survival_htmltable <- function(data, timePFS = NULL, timeOS = NULL, statusPFS=NULL, statusOS=NULL,  var){
  
  if (!is.null(timePFS) & !is.null(statusPFS)) {
    PFS_surv <- add_median_survival(data, time = timePFS, status = statusPFS, var = var)
  } else {
    PFS_surv <- NULL
  }
  if (!is.null(timeOS) & !is.null(statusOS)) {
    OS_surv <- add_median_survival(data, time = timeOS, status = statusOS, var = var)
  } else {
    OS_surv <- NULL
  }
  if (!is.null(PFS_surv) & !is.null(OS_surv)) {
    med_surv <- cbind(PFS_surv, OS_surv)
    med_surv <- t(med_surv)
    rownames(med_surv) <- c("Median PFS (95% CI)", "Median OS (95% CI)")
  } else if (!is.null(PFS_surv)) {
    med_surv <- t(PFS_surv)
    rownames(med_surv) <- c("Median PFS (95% CI)")
    message("Only PFS is calculated and shown")
  } else if (!is.null(OS_surv)) {
    med_surv <- t(OS_surv)
    rownames(med_surv) <- c("Median OS (95% CI)")
    message("Only OS is calculated and shown")
  } else {
    warning("At least one type of survival should be specified")
  }
  return(med_surv)
  
}



