#' Convert best clinical response to standardized format
#'
#' Non-assessible response is replaced by NA. NC is transformed to stable disease. NED is transformed to complete response.
#' Mixed response is documented as stable disease if \code{trtdur} > 90, otherwise it is documented as progressive disease.
#' @param x vector containing response data.
#' @param trtdur vector containing information about the treatment duration 
#' @export
 
convert_response <- function(x, trtdur){
  x <- ifelse(x == "", NA, x)
  x <- gsub("NB", NA, x=x)
  x <- gsub("NC", "SD", x=x)
  x <- gsub("NED", "CR", x=x)
  x <- ifelse(x == "MR" & trtdur > 90, "SD", x)
  x <- ifelse(x == "MR" & trtdur <= 90, "PD", x)
  return(x)
}