#' Print table with univariate, full mutlivariable and reduced (backwards selection) multivariable cox regression
#'
#' This function combines the results of a univariable, full multivariable model and a backwards selection model.
#' @param data data.frame or data.table containing survival data.
#' @param time the time interval from start of observation until date of event (e.g. disease progression or death)
#' or censoring.
#' @param status variable specifying if event occured or data has been censored.
#' @param vars one or more variables defined as character strings to be included in the table
#' @param rgroup a vector of character strings containing headings for row groups.
#' @param p.thres pvalue threshold for backwards selection model.
#' @param footnote character string passed on to \code{tfoot} argument of \code{htmlTable}
#' @param fixed.var specifies fixed variables to be included in the cox model.
#' @param ... additional arguments to be passed on to \code{cox_table_combined}
#' @export

cox_table_combined <- function(data, time, status, vars, fixed.var = NULL, rgroup = NULL, p.thres = 0.1, footnote = NULL, ...){

  univ <- cox_table(data = data, time = time, status = status,
                    vars = vars, footnote = NULL, printHTML = FALSE, univariate = TRUE)
  fullmodel <- cox_table(data = data, time = time, status = status,
                         vars = vars, footnote = NULL, printHTML = FALSE, univariate = FALSE)
  backwardsmodel <- cox_table(data = data, time = time, status = status,  vars = vars, footnote = NULL, fixed.var = fixed.var,
                              printHTML = FALSE, univariate = FALSE, modeltype = "backwards", p.thres = p.thres)
  tmp <- dplyr::left_join(tibble::rownames_to_column(univ$res), tibble::rownames_to_column(fullmodel$res), by = "rowname")
  tmp <- textshape::column_to_rownames(tmp, "rowname")
  out <- dplyr::left_join(tibble::rownames_to_column(tmp), tibble::rownames_to_column(backwardsmodel$res), by = "rowname")
  out <- textshape::column_to_rownames(out, "rowname")
  if(is.null(rgroup)){
    rgroup <- fullmodel$rgroup
  }
  htmlTable(out, rgroup = rgroup, n.rgroup = fullmodel$n.group, cgroup = c("Univariate cox regression", "Multivariate cox regression <br />(full model)",
                                                                           "Multivariate cox regression <br />(backwards selection&#42;)"), n.cgroup = c(2,2,2),
            tfoot = paste(footnote, " &#42;: Threshold for backwards selection was p<",p.thres, ".", sep = ""), ...)
}
