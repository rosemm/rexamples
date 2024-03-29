% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mi_cor.R
\name{mi_cor}
\alias{mi_cor}
\title{Correlations table from multiply imputed datasets}
\usage{
mi_cor(
  data,
  vars = NULL,
  format = c("Amelia", "SPSS"),
  means = TRUE,
  caption = "Correlations",
  method = "pearson"
)
}
\arguments{
\item{data}{Either the output from \code{\link[Amelia]{amelia}} or a dataframe with a column \code{Imputation_} to mark each imputed dataset, as is produced by the imputation procedure in SPSS}

\item{vars}{An optional character vector of the columns to include in the correlations table. If omited, all columns will be used (except the \code{Imputation_} column when \code{format}  = "SPSS)}

\item{format}{Either "Amelia" or "SPSS", indicating which format \code{data} is in.}

\item{means}{A logical indicating whether or not to append means and SEs to the bottom of the correlations table. Default is TRUE.}

\item{caption}{A string to be passed to \code{\link[htmlTable]{htmlTable}} with the caption for the table.}

\item{method}{A string indicating the method for calculating correlations, to be passed to \code{\link[stats]{cor}}, either "pearson", "kendall", or "spearman". Default is "pearson".}
}
\description{
Correlations table from multiply imputed datasets
}
