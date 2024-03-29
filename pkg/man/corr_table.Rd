% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descriptives_table.R
\name{corr_table}
\alias{corr_table}
\title{Correlations table, with optional descriptives}
\usage{
corr_table(
  vars,
  var.names = NULL,
  caption = NULL,
  plot = FALSE,
  show.means = FALSE,
  digits = 2,
  stars = FALSE,
  ...
)
}
\arguments{
\item{vars}{a dataframe of the variables to include in the table}

\item{var.names}{an (optional) vector of strings the variable names (will use the column names in vars if none are provided here). 
If var.names is simply "labels" then it will attempt to use variable labels that may have been read in when the dataframe was created (see \code{\link{use_var_labels}}).}

\item{caption}{an (optional) caption to add to the table}

\item{...}{additional arguments passed to \code{\link[htmlTable]{htmlTable}} or \code{\link[pander]{pander}}}
}
\description{
Uses \code{\link[corrr]{fashion}} and \code{\link[htmlTable]{htmlTable}} or \code{\link[pander]{pander}} to produce nicely formated correlation table, 
optionally including means, SD, min and max for each variable below the correlation table. If plot=TRUE, will also produce a visual of the correlations.
}
