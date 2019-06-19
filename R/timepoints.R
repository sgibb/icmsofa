#' Get SOFA for specific timepoint
#'
#' @param icm `data.frame`, ICM data
#' @param tp `data.frme`, timepoint data, first column Id, other columns
#' timepoints
#' @param lag `numeric`, lag seconds added to reference date and extend the
#' range to 24 h + lag seconds (e.g. laboratory values take some time)
#' @param lagOnlyLaboratory `logical` add lag seconds only to the laboratory
#' values?
#' @param verbose `logical`, verbose output?
#' @return `data.frame`
#' @export
sofaForTimepoints <- function(icm, tp, lag=0L, lagOnlyLaboratory=TRUE,
                              verbose=interactive()) {
    stopifnot(
        is.data.frame(icm),
        is.data.frame(tp) || is.matrix(tp),
        is.numeric(lag) || length(lag) != 1L
    )

    nc1 <- ncol(tp) - 1L
    nr <- nrow(tp)

    sofa <- matrix(
        NA_integer_, nrow=nr, ncol=nc1 * 6L,
        dimnames=list(
            c(),
            paste(
                rep(c(.sofaItems, "SOFA"), nc1),
                rep(colnames(tp)[-1L], each=6L),
                sep="_"
            )
        )
    )
    if (verbose) {
        pb <- txtProgressBar(0L, nr * nc1, style=3L)
        on.exit(close(pb))
    }
    for (i in seq_len(nr)) {
        sb <- icm[icm$CaseId == tp[i, 1L],, drop=FALSE]
        if (nrow(sb)) {
            for (j in seq_len(nc1)) {
                if (verbose) {
                    setTxtProgressBar(pb, (i - 1L) * nc1 + j)
                }
                sofa[i, (j - 1L) * 6L + 1L:6L] <-
                    .sofaAt(sb,
                            tp[i, j + 1L],
                            lag=lag,
                            lagOnlyLaboratory=lagOnlyLaboratory
                    )
            }
        }
    }
    cbind(tp, sofa)
}

#' Get value for specific timepoint
#'
#' @param icm `data.frame`, ICM data
#' @param tp `data.frme`, timepoint data, first column Id, other columns
#' timepoints
#' @param type `character`, type of value
#' @param lag `numeric`, lag seconds added to reference date and extend the
#' range to 24 h + lag seconds (e.g. laboratory values take some time)
#' @param prelag `numeric`, lag seconds added to reference date-24h and extend the
#' range to -24 h + lag seconds
#' @param fun `function`, to apply over the values
#' @param verbose `logical`, verbose output?
#' @return `data.frame`
#' @export
valueForTimepoints <- function(icm, tp, type, lag=0L, prelag=0L, fun=.maxNa,
                               verbose=interactive()) {
    stopifnot(
        is.data.frame(icm),
        is.data.frame(tp) || is.matrix(tp),
        is.numeric(lag) || length(lag) != 1L,
        is.character(type) || length(type) != 1L
    )
    fun <- match.fun(fun)

    nr <- nrow(tp)
    nc1 <- ncol(tp) - 1L

    m <- matrix(
        NA_real_, nrow=nr, ncol=nc1,
        dimnames=list( c(), paste(type, colnames(tp)[-1L], sep="_"))
    )

    if (verbose) {
        message("Looking for ", type, " ...")
        pb <- txtProgressBar(0L, nr * nc1, style=3L)
        on.exit(close(pb))
    }
    icm <- icm[icm$Type == type, , drop=FALSE]

    for (i in seq_len(nr)) {
        sb <- icm[icm$CaseId == tp[i, 1L],, drop=FALSE]
        if (nrow(sb)) {
            for (j in seq_len(nc1)) {
                if (verbose) {
                    setTxtProgressBar(pb, (i - 1L) * nc1 + j)
                }
                m[i, j] <- .valueAt(
                    sb,
                    tp=tp[i, j + 1L],
                    vcol="Value",
                    lag=lag,
                    prelag=prelag,
                    fun=fun
                )
            }
        }
    }
    cbind(tp, m)
}

#' Extract specific time range/frame.
#'
#' @param icm `data.frame`, ICM data
#' @param tp `data.frme`, timepoint data, first column Id, second column
#' timepoints
#' @param lag `numeric`, lag seconds added to reference date and extend the
#' range to 24 h + lag seconds (e.g. laboratory values take some time)
#' @return `data.frame`
#' @export
extractTimeFrame <- function(icm, tp, lag=0L) {
    icm <- icm[icm$CaseId %in% tp[, 1L],, drop=FALSE]
    icm <- icm[order(icm$CaseId, icm$Date), ]
    sel <- logical(nrow(icm))
    for (i in seq_len(nrow(tp))) {
        sel <- sel |
            (icm$CaseId == tp[i, 1L] &
             .prev24h(icm$Date, tp[i, 2L], lag) &
             !is.na(tp[i, 2L]))
    }
    icm[sel,, drop=FALSE]
}
