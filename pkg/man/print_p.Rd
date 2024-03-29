% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_p.R
\name{print_p}
\alias{print_p}
\title{Print p value}
\usage{
print_p(p, digits = 3)
}
\arguments{
\item{p}{a number, like from a significance test}

\item{digits}{digits to round to}
}
\description{
Takes a number and returns a string with "p = " and the number
printed with no leading zero and rounded to digits decimal places. 
If the number is < .001, the just returns "p < .001".
}
