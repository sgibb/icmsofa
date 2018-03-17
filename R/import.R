#' Import ICM data
#'
#' Import ICM XLS-Export.
#'
#' @param file `character`, filename
#' @param sheets `character`, sheet names
#' @param columns `character`, column names
#' @param verbose `logical`, verbose output?
#' @return data.frame
#' @export
importXl <- function(file, sheets=c(PAO2="PO2",
                                    FIO2="FIO",
                                    NOR="Noradrenalin",
                                    DOB="Dobutamin",
                                    IBP="IBPm",
                                    BILI="Bilirubin",
                                    PLT="Thrombozyten",
                                    CREA="Kreatinin"),
                     columns=c(CaseId="fallnummer", Date="admindate",
                               Description="treatmentname", Value="num"),
                     verbose=interactive()) {
    tbl <- lapply(names(sheets), function(nms) {
        sheet <- sheets[nms]
        tbl <- read_excel(file, sheet)[, columns]
        colnames(tbl) <- names(columns)
        tbl <- tbl[!is.na(tbl$CaseId), ]
        tbl <- unique(tbl)
        tbl$Type <- nms
        as.data.frame(tbl, stringsAsFactors=FALSE)
    })
    tbl <- do.call(rbind, tbl)
    colnames(tbl) <- c(names(columns), "Type")

    .import(tbl, verbose)
}

#' Import ICM data
#'
#' Import ICM SQL-Export.
#'
#' @param file `character`, filename
#' @param columns `character`, column names
#' @param verbose `logical`, verbose output?
#' @return data.frame
#' @export
importIcm <- function(file,
                      columns=c(CaseId="FALLNUMMER", Date="ADMINDATE",
                                TreatmentId="TREATMENTID",
                                Description="TREATMENTNAME",
                                Value="NUMVALUE", Duration="DURATION"),
                      verbose=interactive()) {
    tbl <- read.table(
        file,
        dec=",", sep="\t", header=TRUE,
        stringsAsFactors=FALSE
    )
    tbl <- tbl[, columns, drop=FALSE]
    colnames(tbl) <- names(columns)

    tbl$Type <- .treatmentIdType(tbl$TreatmentId)

    .import(tbl, verbose)
}

#' Fix and Reorder ICM import
#'
#' @param tbl `data.frame`
#' @param verbose `logical`, verbose output?
#'
#' @noRd
.import <- function(tbl, verbose=interactive()) {
    if (verbose) {
        message("Inspect BGA values ...")
    }
    tbl <- .filterBga(tbl, keep="arterial", verbose=verbose)
    if (verbose) {
        message("Inspect paO2 values ...")
    }
    isPaO2 <- tbl$Type == "PAO2" & !is.na(tbl$Type)
    tbl$Value[isPaO2] <- .filterPaO2(tbl$Value[isPaO2], verbose=verbose)
    if (verbose) {
        message("Inspect PaO2 values ...")
    }
    isFiO2 <- tbl$Type == "FIO2" & !is.na(tbl$Type)
    tbl$Value[isFiO2] <- .filterFiO2(tbl$Value[isFiO2], verbose=verbose)
    if (verbose) {
        message("Inspect (N)IBP values ...")
    }
    isBp <- grepl("^N?IBP$", tbl$Type)
    tbl$Value[isBp] <- .filterIbp(tbl$Value[isBp], verbose=verbose)

    tbl <- tbl[order(tbl$CaseId, tbl$Date),]
    rownames(tbl) <- NULL
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
    tbl[-1L] <- lapply(
        tbl[-1L],
        as.POSIXct,
        format="%d.%m.%Y %H:%M", origin="1970-01-01 00:00:00", tz="UTC"
    )
    as.data.frame(tbl, stringsAsFactors=FALSE)
}
