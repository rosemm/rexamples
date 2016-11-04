#' Calculation of Cohen's f for ANOVA effect size
#'
#' Calculates Cohen's f from group means, sample size, and pooled standard deviation for a one-way ANOVA.
#' Note that this only works for balanced designs (same n in each group).
#'
#' @param means a vector of group means
#' @param n sample size in each group
#' @param sd standard deviation in each group (also assumed to be the population standard deviation)
#'
#' @return f
#'
#' @examples
#' coehn.f(means = c(4,6,8), n = 10, sd = 4)
#' 
#' library(pwr)
#' f <- coehn.f(means = c(4,6,8), n = 10, sd = 4)
#' pwr.anova.test(k = 3, n = 10, f = f)
#' 
#' @references Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). New York:Academic Press.
#' 
#' @seealso \code{\link[pwr]{pwr.anova.test}}, \code{\link[effsize]{cohen.d}}
#'
#' @export
coehn.f <- function(means = NULL, n = NULL, sd = NULL){
  gm <- mean(means) # grand mean
  N <- n*length(means) # total N
  f <- sqrt(n/N*sum((means-gm)^2)/sd^2)
return(f) 
}