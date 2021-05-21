#' Get description stats for survival analysis
#'
#' Calculate descriptive statistics grouped by strata defined by the variable \code{strata} for
#' different outcome variables.
#' @param data data.frame or data.table
#' @param strat variable used for stratification.
#' @param outcome outcome variable to be summarized.
#' @param digits the number of decimals used.
#' @param ... additional arguments passed on to \code{getDescriptionStatsBy} function from \code{Gmisc} package
#' @inheritParams Gmisc::getDescriptionStatsBy
#' @export

get_stats <- function(data,strat, outcome, digits=1,add_total_col="last",show_all_values=TRUE,
                      hrzl_prop=FALSE,statistics=FALSE,html=TRUE,header_count = TRUE,
                      continuous_fn=Gmisc::describeMedian,...){
  strat <- paste(deparse(substitute(data)),"$", strat,sep = "")
  data <- as.data.frame(data)
  Gmisc::getDescriptionStatsBy(data[, outcome],
                               eval(parse(text = strat)),
                               add_total_col=add_total_col,
                               show_all_values=show_all_values,
                               hrzl_prop=hrzl_prop,
                               statistics=statistics,
                               html=html,
                               header_count = header_count,
                               digits=digits,
                               continuous_fn=continuous_fn,...)
}




