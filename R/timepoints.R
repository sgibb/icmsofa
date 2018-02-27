#' Get SOFA for specific timepoint
#'
#' @param icm `data.frame`, ICM data
#' @param tp `data.frme`, timepoint data, first column Id, other columns
#' timepoints
#' @return `data.frame`
#' @export
sofaForTimepoints <- function(icm, tp) {
    sofa <- matrix(NA_integer_, nrow=nrow(tp), ncol=ncol(tp) - 1L,
                   dimnames=list(c(), paste0("Sofa", colnames(tp)[-1L])))
    for (i in seq_len(nrow(tp))) {
        sb <- icm[icm$Id == tp[i, 1L],]
        for (j in seq_len(ncol(tp) - 1L)) {
            sel <- sb$Date <= tp[i, j + 1L]
            m <- which.max(sb$Date[sel])
            if (length(m)) {
                sofa[i, j] <- sb$Sofa[sel][m]
            }
        }
    }
    cbind(tp, sofa)
}
