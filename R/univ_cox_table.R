#' Combine results from multiple univariate cox regressions into one table
#'
#' This function combines the results from multiple calls of \code{univariate_cox_output} and prints the output
#' as an htmlTable generated with the \code{htmlTable} package.
#' @param data data.frame or data.table containing survival data.
#' @param time the time interval from start of observation until date of event (e.g. disease progression or death)
#' or censoring.
#' @param status variable specifying if event occured or data has been censored.
#' @param vars one or more variables defined as character strings to be included in the table
#' @param footnote character string passed on to \code{tfoot} argument of \code{htmlTable}
#' @export

univ_cox_table <- function(data, time, status, vars, footnote = NULL){
  tmp <- lapply(vars, univariate_cox_output, data = data, time = time, status = status)
  tmp <- dplyr::bind_rows(tmp)
  rownames(tmp) <- gsub(">=", "&#8805", rownames(tmp))
  htmlTable::htmlTable(tmp, tfoot = footnote)
}




