#' Print p value
#' 
#' Takes a number and returns a string with "p = " and the number
#' printed with no leading zero and rounded to digits decimal places. 
#' If the number is < .001, the just returns "p < .001".
#'  
#' @param p a number, like from a significance test
#' @param digits digits to round to
#' 
#' @export
print_p <- function(p, digits = 3){
  if (p < .001){
    p_statement <- "p < .001"
  } else {
    p_statement <- paste0("p = ", sub(pattern = "^0", replacement = "", x=as.character(round(p, digits))))
  }
}
