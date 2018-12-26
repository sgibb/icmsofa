#' Filter icm import
#'
#' @param tbl `data.frame`, icm data
#' @param verbose `logical`, verbose output?
#' @return `data.frame`
#' @noRd
.filter <- function(tbl, verbose=interactive()) {
    isNa <- is.na(tbl$Type)
    tbl$Valid <- !isNa
    if (verbose) {
        message(
            sum(isNa),
            " entries removed because their treatment type is missing/unkown."
        )
    }
    isNa <- is.na(tbl$Value)
    tbl$Valid <- tbl$Valid & !isNa
    if (verbose) {
        message(
            sum(isNa),
            " entries removed because their value is missing/unkown."
        )
    }
    if (verbose) {
        message("Inspect BGA values ...")
    }
    tbl <- .filterBga(tbl, keep="arterial", verbose=verbose)

    flt <- data.frame(
        type=c("PAO2", "FIO2", "O2INS", "SPO2", "MAP"),
        value=c("Value", "Value", "Dose", "Value", "Value"),
        lower=c(10, 21, 0, 60, 40),
        upper=c(600, 100, 15, 100, 150),
        stringsAsFactors=FALSE
    )

    for (i in seq_len(nrow(flt))) {
        if (verbose) {
            message("Inspect ", flt$type[i], " values ...")
        }

        isType <- grepl(paste0("^", flt$type[i], "$"), tbl$Type)
        tbl$Valid[isType] <-
            tbl[isType, flt$value[i]] %range% c(flt$lower[i], flt$upper[i]) &
            !is.na(tbl[isType, flt$value[i]]) & tbl$Valid[isType]

        if (verbose) {
            message(
                sum(!tbl$Valid[isType]), " ", flt$type[i],
                " removed, because they are not in range ",
                "[", flt$lower[i], ";", flt$upper[i], "].")
        }

    }
    tbl
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
    mapping <- c(arterial=1L, venous=2L, misc=999L)
    isBga <- x$Type == "BGA" & x$Valid
    toRemoveBga <- isBga & !(x$Value %in% mapping[keep])
    d <- unique(x$Date[toRemoveBga])
    toRemove <- x$Type == "PAO2" & x$Valid & x$Date %in% d
    if (verbose && any(toRemove)) {
        message(
            sum(toRemove), " PAO2 values removed because they are not ",
            paste(keep, collapse=" or "), "."
        )
    }
    x$Valid[toRemoveBga | toRemove] <- FALSE
    x
}
