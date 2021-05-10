#' Get description stats for survival analysis
#'
#' Fitting a survival fit for each subgroup defined by var. Additionally the total survival time for the
#' whole sample is calculated.
#' @param data data.frame or data.table
#' @param strat Variable used for stratification.
#' @param outcome Outcome variable to be summarized.
#' @param digits The number of decimals used.
#' @param ... Additional arguments passed on to \code{getDescriptionStatsBy} function from \code{Gmisc} package
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




