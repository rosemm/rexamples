#' Print r or R-squared value
#' 
#' Takes a number and returns a string with no leading zero and rounded to digits decimal places.
#'  
#' @param r a number, like from a correlation or model R-squared
#' @param digits digits to round to
#' 
print_r <- function(r, digits = 2){
  sub(pattern = "^0", replacement = "", x=as.character(round(r, digits)))
}
