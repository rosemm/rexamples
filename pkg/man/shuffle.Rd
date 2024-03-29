% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shuffle.R
\name{shuffle}
\alias{shuffle}
\title{Randomly shuffle a vector}
\usage{
shuffle(x)
}
\arguments{
\item{x}{a vector}
}
\description{
Randomly re-order the elements in a vector. 
This is just a convenience function for \code{\link[base]{sample}} where \code{size} is the length of the vector.
}
\examples{
x <- 1:10
shuffle(x)

}
\seealso{
\code{\link[base]{sample}}
}
