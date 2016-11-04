#' Executes all of the scripts in a directory
#' 
#' Uses \code{\link[base]{source}} to run all of the R scripts in a directory. 
#' A handy way to load a library of functions for a project, for example. 
#'
#' @param path directory to source
#' @param trace print names of files as they're sourced? default=TRUE
#' @param ... other arguments to pass to \code{\link[base]{source}}
#' 
#' @return None
#'
#' @examples
#' source_dir("lib")
#' 
#' @seealso \code{\link[base]{source}}
#'
#' @export
source_dir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
