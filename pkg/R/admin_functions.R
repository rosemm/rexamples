check_for_pkg <- function(pkgs){
  error <- FALSE
  for(pkg in pkgs){
    if (!requireNamespace(pkg, quietly = TRUE)) {
      error <- TRUE # if any of the packages are missing, set error to TRUE
      message("Package \"",pkg, "\" needed for this function to work. Please install it by running:\n install.packages(\"",pkg,"\")")
    }
  }
  if(error) {
    stop("This function cannot run.", call. = FALSE)
  }
}
  
  