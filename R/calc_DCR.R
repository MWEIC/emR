#' Calculate disease control rate
#'
#' This function takes the output of \code{convert_response} and assigns 1 if response is complete response, partial
#' response or stable disease and 0 if response is progressive disease.
#' @param x vector containing response data.
#' @export

# calculate DCR
calc_DCR <- function(x){
  ifelse(x == "CR"| x == "PR" | x == "SD" , 1,0)
}