#' Randomly shuffle a vector
#'
#' Randomly re-order the elements in a vector. 
#' This is just a convenience function for \code{\link[base]{sample}} where \code{size} is the length of the vector.
#'
#' @param x a vector
#'
#' @examples
#' x <- 1:10
#' shuffle(x)
#' 
#' @seealso \code{\link[base]{sample}}
#'
#' @export
shuffle <- function(x){
  y <- base::sample(x, size=length(x), replace = FALSE)
  return(y)
}