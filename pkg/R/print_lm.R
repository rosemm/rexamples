#' Print regression summary inline 
#' 
#' Prints a string that summarizes lm model result in APA format
#'  
#' @param model a model object resulting from lm
#' @param digits digits to round R² and F statistic to
#' @param digits_p digits to round the p value to
#' 
#' @return A string with the model results in APA format, like "R<sup>2</sup> = .07, F(1, 18) = 1.42, p = .249".
#' 
#' @examples
#' print_lm(lm(mpg ~ wt, data = mtcars))
#' 
#' @seealso \code{\link[stats]{lm}} for creating the model object
#' and \code{\link[stats]{summary.lm}}
#' 
#' @export
print_lm <- function(model, digits = 2, digits_p = 3){
  rsq <- summary(model)$r.squared
  f <- summary(model)$fstatistic["value"]
  df1 <- summary(model)$fstatistic["numdf"]
  df2 <- summary(model)$fstatistic["dendf"]
  p <- pf(f,df1,df2, lower.tail = FALSE)
  paste0("R² = ", print_r(rsq, digits), ", F(", df1, ", ", df2, ") = ", round(f, digits), ", ", print_p(p, digits_p) )
}
