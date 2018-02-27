#' Import ICM data
#'
#' Import ICM SQL-Export.
#'
#' @param file `character`, filename
#' @param sheets `character`, sheet names
#' @param columns `character`, colum names
#' @return data.frame
#' @export
importIcm <- function(file, sheets=c(PAO="PO2",
                                     FIO="FIO",
                                     NOR="Noradrenalin",
                                     DOB="Dobutamin",
                                     IBP="IBPm",
                                     BIL="Bilirubin",
                                     PLT="Thrombozyten",
                                     CRE="Kreatinin"),
                      columns=c(Id="fallnummer", Date="admindate",
                                Description="treatmentname", Value="num")) {
    tbl <- lapply(names(sheets), function(nms) {
        sheet <- sheets[nms]
        tbl <- read_excel(file, sheet)[, columns]
        colnames(tbl) <- names(columns)
        tbl <- tbl[!is.na(tbl$Id), ]
        tbl <- unique(tbl)
        tbl$Type <- nms
        as.data.frame(tbl, stringsAsFactors=FALSE)
    })
    tbl <- do.call(rbind, tbl)
    ## fix dates
    tbl$Date <- as.POSIXct(tbl$Date, origin="1970-01-01 00:00:00", tz="UTC")

    tbl$Value[tbl$Type == "IBP"] <- .filterIbp(tbl$Value[tbl$Type == "IBP"])

    tbl[order(tbl$Id, tbl$Date),]
}
