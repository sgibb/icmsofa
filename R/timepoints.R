#' Get SOFA for specific timepoint
#'
#' @param icm `data.frame`, ICM data
#' @param tp `data.frme`, timepoint data, first column Id, other columns
#' timepoints
#' @param lag `numeric`, lag seconds added to reference date and extend the
#' range to 24 h + lag seconds (e.g. laboratory values take some time)
#' @param lagOnlyLaboratory `logical` add lag seconds only to the laboratory
#' values?
#' @return `data.frame`
#' @export
sofaForTimepoints <- function(icm, tp, lag=0L, lagOnlyLaboratory=TRUE) {
    sofa <- matrix(NA_integer_, nrow=nrow(tp), ncol=(ncol(tp) - 1L) * 6L,
                   dimnames=list(c(), paste(
                                        rep(
                                            c(.sofaItems, "SOFA"),
                                            ncol(tp) - 1L
                                        ),
                                        rep(
                                            colnames(tp)[-1L],
                                            each=6L
                                        ),
                                        sep="_"
                                      )
                   )
    )
    for (i in seq_len(nrow(tp))) {
        sb <- icm[icm$CaseId == tp[i, 1L],,drop=FALSE]
        if (nrow(sb)) {
            for (j in seq_len(ncol(tp) - 1L)) {
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
