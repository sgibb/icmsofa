#' Write SOFA data.frame
#'
#' Write SOFA `data.frame` to file
#'
#' @param x `data.frame`
#' @param path `character`, file path
#' @export
exportSofa <- function(x, path) {
    invisible(lapply(split(x, x$CaseId), function(sb) {
        write.csv(
            sb,
            file=file.path(path, paste(sb$CaseId[1L], "csv", sep=".")),
            row.names=FALSE
        )
    }))
}
