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
    isOOR <- x < range[1L] | x > range[2L] | is.na(x)
    if (verbose && any(isOOR)) {
        message(
            sum(isOOR), " values removed because they are ",
            "out of range [", range[1L], ";", range[2L], "]."
        )
    }
    x[isOOR] <- NA_real_
    x
}

#' Keep just art/ven BGA
#'
#' @param x `data.frame`
#' @param keep `character`, which BGA to keep
#' @param verbose `logical`, verbose output?
#' @return `data.frame`
#' @noRd
.filterBga <- function(x, keep=c("arterial", "venous", "misc"),
                       verbose=interactive()) {
    keep <- match.arg(keep, several.ok=TRUE)
    mapping <- c(arterial=1L, venous=2L, misc=9L)
    isBga <- x$Type == "BGA" & !is.na(x$Type)
    d <- x$Date[isBga & !x$Value %in% mapping[keep]]
    toRemove <- x$Type == "PAO2" & !is.na(x$Type) & x$Date %in% d
    if (verbose && any(toRemove)) {
        message(
            sum(toRemove), " paO2 values removed because they are not ",
            paste(keep, collapse=" or "), "."
        )
    }
    x$Value[toRemove] <- NA_real_
    x
}
