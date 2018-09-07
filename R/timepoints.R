#' Get SOFA for specific timepoint
#'
#' @param icm `data.frame`, ICM data
#' @param tp `data.frme`, timepoint data, first column Id, other columns
#' timepoints
#' @return `data.frame`
#' @export
sofaForTimepoints <- function(icm, tp) {
    sofa <- matrix(NA_integer_, nrow=nrow(tp), ncol=(ncol(tp) - 1L) * 6L,
                   dimnames=list(c(), paste(
                                        rep(
                                            c(.sofaItems, "SOFA"),
                                            ncol(tp) - 1L
                                        ),
                                        colnames(tp)[-1L],
                                        sep="_"
                                      )
                   )
    )
    for (i in seq_len(nrow(tp))) {
        sb <- icm[icm$CaseId == tp[i, 1L],]
        for (j in seq_len(ncol(tp) - 1L)) {
            sel <- sb$Date <= tp[i, j + 1L]
            m <- which.max(sb$Date[sel])
            if (length(m)) {
                sb <- sb[sel, c(.sofaItems, "SOFA"), drop=FALSE]
                sofa[i, (j - 1L) * 6L + 1L:6L] <- as.integer(sb[m[1L],, drop=FALSE])
            }
        }
    }
    cbind(tp, sofa)
}
