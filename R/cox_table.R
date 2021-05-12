#' Print results from univariate or multivariate cox regression
#'
#' This function combines the results from multiple calls of \code{cox_output} and prints the output
#' as an htmlTable generated with the \code{htmlTable} package or the output of multivariate cox regression.
#' @param data data.frame or data.table containing survival data.
#' @param time the time interval from start of observation until date of event (e.g. disease progression or death)
#' or censoring.
#' @param status variable specifying if event occured or data has been censored.
#' @param vars one or more variables defined as character strings to be included in the table
#' @param footnote character string passed on to \code{tfoot} argument of \code{htmlTable}
#' @param printHTML Logical value. If TRUE output is printed as htmlTable. Default is TRUE.
#' @param univariate Logical value. If TRUE output of univariate cox regression is printed. Else output of multivariate
#' cox regression is printed. Default is TRUE.
#' @param ... additional arguments to be passed on to \code{cox_output}
#' @export

cox_table <- function(data, time, status, vars, footnote = NULL, printHTML = TRUE, univariate = TRUE,...){
  if (univariate == TRUE) {
    tmp <- lapply(vars, cox_output, data = data, time = time, status = status, ...)
    tmp <- dplyr::bind_rows(tmp)
  } else {
    tmp <- cox_output(data = data, time = time, status = status, vars = vars,...)
  }
  rownames(tmp) <- gsub(">=", "&#8805", rownames(tmp))
  if (printHTML == TRUE){
    htmlTable::htmlTable(tmp, tfoot = footnote)
  } else{
    return(tmp)
  }
}
