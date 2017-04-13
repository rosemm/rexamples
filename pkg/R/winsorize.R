#' @export
winsorize <- function (x, multiple=3)
{
  if(length(multiple) != 1 || multiple <= 0) {
    stop("bad value for 'multiple'")
  }
  # med <- median(x)
  med <- mean(x)
  y <- x - med
  # sc <- mad(y, center=0) * multiple
  sc <- sd(y) * multiple
  y[ y > sc ] <- sc
  y[ y < -sc ] <- -sc
  return(y + med)
}
