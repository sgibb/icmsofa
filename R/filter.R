#' Filter IBP values
#'
#' Replace invalid IBP values with `NA`
#'
#' @param x `double`, IPB values
#' @param range `double, allowed IBP rane
#' @return `double`
#' @noRd
.filterIbp <- function(x, range=c(20, 150)) {
    stopifnot(is.double(x))
    stopifnot(is.numeric(range) && length(range) == 2L)
    x[x < range[1L] | x > range[2L]] <- NA_real_
    x
}
