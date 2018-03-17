#' Filter PAO2 values
#'
#' Replace invalid PAO2 values with `NA`
#'
#' @param x `double`, PAO2 values
#' @param range `double, allowed PAO2 range
#' @param verbose `logical`, verbose output?
#' @return `double`
#' @noRd
.filterPaO2 <- function(x, range=c(10, 600), verbose=interactive()) {
    .filterRange(x, range, verbose)
}

#' Filter FIO2 values
#'
#' Replace invalid FIO2 values with `NA`
#'
#' @param x `double`, FIO2 values
#' @param range `double, allowed FIO2 range
#' @param verbose `logical`, verbose output?
#' @return `double`
#' @noRd
.filterFiO2 <- function(x, range=c(0.21, 1.0), verbose=interactive()) {
    .filterRange(x, range, verbose)
}

#' Filter IBP values
#'
#' Replace invalid IBP values with `NA`
#'
#' @param x `double`, IPB values
#' @param range `double, allowed IBP range
#' @param verbose `logical`, verbose output?
#' @return `double`
#' @noRd
.filterIbp <- function(x, range=c(20, 150), verbose=interactive()) {
    .filterRange(x, range, verbose)
}

#' Filter by range
#'
#' @param x `numeric`
#' @param range `numeric`, allowed range
#' @param verbose `logical`, verbose output?
#' @return `numeric`
#' @noRd
.filterRange <- function(x, range, verbose=interactive()) {
    stopifnot(is.numeric(x))
    stopifnot(is.numeric(range) && length(range) == 2L)
    isOOR <- x < range[1L] | x > range[2L]
    if (verbose && any(isOOR)) {
        message(
            sum(isOOR), " values removed because they are ",
            "out of range [", range[1L], ";", range[2L], "]."
        )
    }
    x[isOOR] <- NA_real_
    x
}
