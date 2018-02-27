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
#' seq dates
#'
#' @param x `POSIXct`
#' @return `POSIXct`
#' @noRd
.hourly <- function(x) {
    r <- .limDates(x)
    seq(r[1L], r[2L], by=3600L)
}

#' limits dates
#'
#' Date limits, rounded to the previous/next hour
#'
#' @param x `POSIXct`
#' @return `POSIXct`
#' @noRd
.limDates <- function(x) {
    trunc(range(x), "hour") + c(0L, 3600L)
}

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

#' SOFA id
#'
#' Turn different `Type` values into an ID
#'
#' @param x `character`
#' @return `integer`
#' @noRd
.sofaTypeId <- function(x) {
    stopifnot(is.character(x))
    id <- c(
        "HOR"=1L, "DOB"=2L, "NOR"=2L, "IBP"=2L, "BIL"=3L, "PLT"=4L, "CRE"=5L
    )
    unname(id[x])
}
