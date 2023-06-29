#' Print r or R² value
#' 
#' Takes a number and returns a string with no leading zero and rounded to digits decimal places.
#'  
#' @param r a number, like from a correlation or model R2
#' @param digits digits to round to
#' 
#' @export
print_r <- function(r, digits = 2){
  sub(pattern = "^0", replacement = "", x=as.character(round(r, digits)))
}
