
univ_cox_table <- function(data, time, status, vars, footnote = NULL){
  tmp <- lapply(vars, univariate_cox_output, data = data, time = time, status = status)
  tmp <- dplyr::bind_rows(tmp)
  rownames(tmp) <- gsub(">=", "&#8805", rownames(tmp))
  htmlTable::htmlTable(tmp, tfoot = footnote)
}


# To do: update package with 2 functions, add documentation for functions, 