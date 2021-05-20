#' Calculate overall response rate
#'
#' This function takes the output of \code{convert_response} and assigns 1 if response is complete response or partial
#' response, otherwise assigns 0.
#' @param x vector containing response data.
#' @export

calc_ORR <- function(x){
  ifelse(x == "CR"| x == "PR", 1,0)
}