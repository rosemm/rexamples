#' Calculate average of correlations
#' 
#' Computes the average of the estimates in vector x, using Fisher's r-to-z transformation before computing the mean and reversing the transformation afterward.
#' \code{ave_Rsq} takes the squareroot of all of the elements first, then applies the transformation, 
#' computes the mean, reverses the transformation, and squares the result to return the units to a squared correlation.
#' 
#' @param x a numeric vector
#' 
#' @seealso \code{\link[base]{atanh}} for computing 
#' \href{https://en.wikipedia.org/wiki/Fisher_transformation}{Fisher's r-to-z transformation}, 
#' and \code{\link[base]{mean}}
#' 
#' @export
ave_r <- function(x, na.rm=TRUE){
  # using Fishers r to z transformation, and reversing it
  tanh(mean(atanh(x), na.rm=na.rm))
}

#' @describeIn ave_r Calculate average of squared correlations
#' @export
ave_Rsq <- function(x, na.rm=TRUE){
  # using Fishers r to z transformation, and reversing it
  tanh(mean(atanh(sqrt(x)), na.rm=na.rm))^2
}


