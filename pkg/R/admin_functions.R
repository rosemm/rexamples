source_rmd_url <- function (url, ..., sha1 = NULL) {
  # based on devtools::source_url()
    stopifnot(is.character(url), length(url) == 1)
    temp_file <- tempfile()
    on.exit(unlink(temp_file))
    request <- httr::GET(url)
    httr::stop_for_status(request)
    writeBin(httr::content(request, type = "raw"), temp_file)
    file_sha1 <- digest::digest(file = temp_file, algo = "sha1")
    if (is.null(sha1)) {
      message("SHA-1 hash of file is ", file_sha1)
    } else {
      if (nchar(sha1) < 6) {
        stop("Supplied SHA-1 hash is too short (must be at least 6 characters)")
      }
      file_sha1 <- substr(file_sha1, 1, nchar(sha1))
      if (!identical(file_sha1, sha1)) {
        stop("SHA-1 hash of downloaded file (", file_sha1, 
             ")\n  does not match expected value (", sha1, 
             ")", call. = FALSE)
      }
    }
    knitr::knit(temp_file, envir=globalenv())
}
