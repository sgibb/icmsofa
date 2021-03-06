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
                                Type="TREATMENTTYPE",
                                Value="NUMVALUE", Dose="DOSE",
                                Begin="BEGIN", End="END"),
                      sep="\t", dec=",",
                      verbose=interactive()) {

    tbl <- read.table(
        file,
        dec=dec, sep=sep, header=TRUE, strip.white=TRUE,
        stringsAsFactors=FALSE
    )
    if (verbose) {
        message("Drop useless columns")
    }
    tbl <- tbl[, columns, drop=FALSE]
    colnames(tbl) <- names(columns)

    tbl$Type <- toupper(tbl$Type)

    if (verbose) {
        message("Convert dates")
    }
    tbl$Date <- .asPosixCt(tbl$Date)
    tbl$Begin <- .asPosixCt(tbl$Begin)
    tbl$End <- .asPosixCt(tbl$End)

    tbl <- .filter(tbl, verbose)

    # We don't use dopamine!
    isDrug <- tbl$Type %in% c("DOB", "NOR")
    tbl$Value[isDrug] <- tbl$Dose[isDrug]

    isO2 <- tbl$Type == "O2INS" & tbl$Valid
    if (any(isO2)) {
        tbl$Value[isO2] <- .o2FlowRateToFiO2(tbl$Dose[isO2])
    }

    isFiO2 <- tbl$Type == "FIO2" & tbl$Valid
    tbl$Value[isFiO2] <- tbl$Value[isFiO2] / 100L

    tbl <- tbl[order(tbl$CaseId, tbl$Date),]

    if (verbose) {
        message("Correct FiO2 times")
    }
    tbl <- .correctFiO2Times(tbl, threshold=3600)
    tbl <- .convertSpO2intoPaO2(tbl)

    tbl <- tbl[order(tbl$CaseId, tbl$Date),]
    rownames(tbl) <- NULL
    tbl
}

#' Import timepoint data.
#'
#' @param file `character`, filename
#' @param sep `character`, field separator
#' @param dec `character`, decimal sign
#' @param nonTimeColumns `numeric`, columns without time information
#' @param format `character`, time format
#' @return data.frame
#' @export
importTimepoints <- function(file, sep="\t", dec=",", nonTimeColumns=1L,
                             format="%d.%m.%Y %H:%M") {
    tbl <- read.table(
        file,
        dec=dec, sep=sep, header=TRUE,
        stringsAsFactors=FALSE
    )
    tbl[-nonTimeColumns] <- lapply(
        tbl[-nonTimeColumns],
        .asPosixCt,
        format=format
    )
    tbl
}
