#' Tests of canonical dimensions
#'
#' Based closely on the code from \url{http://www.ats.ucla.edu/stat/r/dae/canonical.htm}, with slight modifications to make it work more easily as a function.
#' 
#' @param cc the canonical correlations to test, output from from \code{\link[CCA]{cc}}
#' 
#' @return A data frame where each row is one dimension from the cannonical correlation (in order from largest to smallest), with columns for R-squared, Wilk's Lambda, F statistic, df for the F test, and p value for the F test.
#'
#' @examples
#' cc1 <- CCA::cc(iris[ , 1:2], iris[ , 3:4])
#' test_cc(cc1)
#' 
#' @seealso \code{\link[CCA]{cc}}
#' 
#' @references \url{http://www.ats.ucla.edu/stat/r/dae/canonical.htm}
#' @references Rencher, A. C. (2002). Methods of multivariate analysis, 2nd ed. John Wiley & Sons. p.367-p.370
#'
#' @export
test_cc <- function(cc){
  r_sq <- cc$cor^2 # squared correlations
  eigen_vals <- (1 - cc$cor^2)
  
  n <- length(cc$names$ind.names)
  p <- length(cc$names$Xnames)
  q <- length(cc$names$Ynames)
  k <- min(p, q)
  w <- n - 3/2 - (p + q)/2
  
  w_lambda <- rev(cumprod(rev(eigen_vals)))
  
  # initialize
  d1 <- d2 <- f <- vector("numeric", k)
  
  for (i in 1:k) {
    t <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    ti <- 1/t
    d1[i] <- p * q
    d2[i] <- w * t - p * q/2 + 1
    r <- (1 - w_lambda[i]^ti)/w_lambda[i]^ti
    f[i] <- r * d2[i]/d1[i]
    p <- p - 1 # test remaining dimensions
    q <- q - 1 # test remaining dimensions
  }
  
  pv <- pf(f, d1, d2, lower.tail = FALSE)
  dmat <- data.frame(R_sq=r_sq, WilksL = w_lambda, approx_F = f, df1 = d1, df2 = d2, p = pv)
  return(dmat)
}
