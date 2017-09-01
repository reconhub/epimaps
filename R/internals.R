
## These are internal functions, none of which are exported.


stop0 <- function(x, ...) {
  stop(x, ..., call. = FALSE)
}


## This one checks and input of spatial coordinates and returns a 2-columns
## numeric matrix.

## Author: Thibaut Jombart

check_clean_lonlat <- function(x) {
  x <- as.matrix(x)

  if (!is.numeric(x)) {
    msg <- "spatial coordinates should be numeric"
    stop0(msg)
  }

  if (ncol(x) != 2L) {
    msg <- "spatial coordinates should be a 2-columns matrix or data.frame"
    stop0(msg)
  }

  if (any(!is.finite(x))) {
    msg <- "NA / non-finite values detected in spatial coordinates"
    stop0(msg)
  }

  colnames(x) <- c("lon", "lat")
  return(x)
}
