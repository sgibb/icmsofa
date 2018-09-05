#' Import ICM data
#'
#' Import ICM SQL-Export.
#'
#' @param file `character`, filename
#' @param columns `character`, column names
#' @param sep `character`, field separator
#' @param dec `character`, decimal sign
#' @param verbose `logical`, verbose output?
#' @return data.frame
#' @export
importIcm <- function(file,
                      columns=c(CaseId="HIS_CASEID", Date="ADMINDATE",
                                TreatmentId="TREATMENTID",
                                Description="TREATMENTNAME",
                                Value="NUMVALUE", Dose="DOSE",
                                Begin="BEGIN", End="END"),
                      sep="\t", dec=",",
                      verbose=interactive()) {
    tbl <- read.table(
        file,
        dec=dec, sep=sep, header=TRUE,
        stringsAsFactors=FALSE
    )
    tbl <- tbl[, columns, drop=FALSE]
    colnames(tbl) <- names(columns)

    tbl$Type <- .treatmentIdType(tbl$TreatmentId)

    tbl <- .filter(tbl, verbose)

    # We don't use dopamine!
    isDrug <- tbl$Type %in% c("DOB", "NOR")
    tbl$Value[isDrug] <- tbl$Dose[isDrug]

    isO2 <- tbl$Type == "O2INS" & tbl$Valid
    tbl$Value[isO2] <- .o2FlowRateToFiO2(tbl$Dose[isO2])

    isFiO2 <- tbl$Type == "FIO2" & tbl$Valid
    tbl$Value[isFiO2] <- tbl$Value[isFiO2] / 100L

    tbl$Date <- .asPosixCt(tbl$Date)
    tbl$Begin <- .asPosixCt(tbl$Begin)
    tbl$End <- .asPosixCt(tbl$End)

    tbl <- tbl[order(tbl$CaseId, tbl$Date),]
    rownames(tbl) <- NULL

    tbl <- .correctFiO2Times(tbl, threshold=3600)
    tbl
}

#' Import timepoint data.
#'
#' Import timepoint data.
#'
#' @param file `character`, filename
#' @return data.frame
#' @export
importTimepoints <- function(file) {
    tbl <- read_excel(file)
    tbl[-1L] <- lapply(tbl[-1L], .asPosixCt)
    as.data.frame(tbl, stringsAsFactors=FALSE)
}
