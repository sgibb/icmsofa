#' Convert to POSIXct
#'
#' as.POSIXct with own defaults
#'
#' @param x `character`, date
#' @param format `character`, format
#' @return `POSIXct`
#' @noRd
.asPosixCt <- function(x, format="%d.%m.%y %H:%M:%S") {
    as.POSIXct(x, format=format, origin="1970-01-01 00:00:00", tz="UTC")
}

#' Daily dates
#'
#' daily dates, limits rounded to the previous/next day
#'
#' @param x `POSIXct`
#' @return `POSIXct`
#' @noRd
.daily <- function(x) {
    r <- trunc(range(x), "days") + c(0L, 86400L)
    seq(r[1L], r[2L], by=86400L)
}

#' Replace NA with last vaild value
#'
#' @param x vector
#' @return vector
#' @noRd
.fillNa <- function(x) {
    na <- is.na(x)
    idx <- cumsum(!na) + 1L
    c(NA, x[!na])[idx]
}

#' Hourly dates
#'
#' hourly dates, limits rounded to the previous/next hour
#'
#' @param x `POSIXct`
#' @return `POSIXct`
#' @noRd
.hourly <- function(x) {
    r <- trunc(range(x), "hour") + c(0L, 3600L)
    seq(r[1L], r[2L], by=3600L)
}

#' Is value in range
#'
#' @param x `numeric`
#' @param lower `numeric`
#' @param upper `numeric`
#' @param includeBoundaries `logical`
#' @return logical
#' @noRd
.inRange <- function(x, lower, upper, includeBoundaries=TRUE) {
    if (includeBoundaries) {
        lower <= x & x <= upper
    } else {
        lower < x & x < upper
    }
}
"%range%" <-
    function(x, range) .inRange(x, range[[1L]], range[[2L]],
                                includeBoundaries=TRUE)
"%inside%" <-
    function(x, range) .inRange(x, range[[1L]], range[[2L]],
                                includeBoundaries=FALSE)

#' Calculate maximum
#'
#' Calculate `max` and ignore `NA`, except everything is `NA`
#'
#' @param x `double`
#' @return `double`
#' @noRd
.maxNa <- function(x) {
    m <- suppressWarnings(max(x, na.rm=TRUE))
    if (is.infinite(m)) {
        NA_integer_
    } else {
        m
    }
}

#' Previous 24 hours
#'
#' Return ids of last 24 hours
#'
#' @param x `POSIXct`, dates
#' @param ref `POSIXct`, reference date
#' @param lag `numeric`, lag seconds added to reference date and extend the
#' range to 24 h + lag seconds (e.g. laboratory values take some time)
#' @return `logical`
#' @noRd
.prev24h <- function(x, ref, lag=0) {
    stopifnot(inherits(x, "POSIXct"))
    stopifnot(inherits(ref, "POSIXct"))
    stopifnot(is.numeric(lag))
    x <- as.numeric(x)
    ref <- as.numeric(ref)
    x %range% c(ref - 24L * 3600, ref + lag)
}

#' Find index of last non-NA
#'
#' @param x `double`
#' @return `double`
.whichLastNotNa <- function(x) {
    Position(function(y)!is.na(y), x, right=TRUE)
}
