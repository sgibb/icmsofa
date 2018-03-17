#' Daily dates
#'
#' daily dates, limits rounded to the previous/next day
#'
#' @param x `POSIXct`
#' @return `POSIXct`
#' @noRd
.daily <- function(x) {
    r <- trunc(range(x), "days") + c(0L, 86400L)
    seq(r[1L], r[2L], by=86400L)
}

#' Replace NA with last vaild value
#'
#' @param x vector
#' @return vector
#' @noRd
.fillNa <- function(x) {
    na <- is.na(x)
    idx <- cumsum(!na) + 1L
    c(NA, x[!na])[idx]
}

#' Hourly dates
#'
#' hourly dates, limits rounded to the previous/next hour
#'
#' @param x `POSIXct`
#' @return `POSIXct`
#' @noRd
.hourly <- function(x) {
    r <- trunc(range(x), "hour") + c(0L, 3600L)
    seq(r[1L], r[2L], by=3600L)
}

#' Calculate maximum
#'
#' Calculate `max` and ignore `NA`, except everything is `NA`
#'
#' @param x `double`
#' @return `double`
#' @noRd
.maxNa <- function(x) {
    m <- suppressWarnings(max(x, na.rm=TRUE))
    if (is.infinite(m)) {
        NA_integer_
    } else {
        m
    }
}

#' SOFA id
#'
#' Turn different `Type` values into an ID
#'
#' @param x `character`
#' @return `integer`
#' @noRd
.sofaTypeId <- function(x) {
    stopifnot(is.character(x))
    id <- c(
        "HOR"=1L, "DOB"=2L, "NOR"=2L, "IBP"=2L, "BIL"=3L, "PLT"=4L, "CRE"=5L
    )
    unname(id[x])
}

#' Treatment id to type
#'
#' Turn different treatment id values into a `Type`.
#'
#' @param x `numeric`, treatment id
#' @return `character`
#' @noRd
.treatmentIdType <- function(x) {
    stopifnot(is.numeric(x))

    mapping <- as.data.frame(matrix(c(
        # PAO2
        "1073", "PO2", "PAO2",
        "2794", "PO2 (mmHg)", "PAO2",
        "2813", "PO2 (Temp.) (mmHg)", "PAO2",
        "3691", "PO2 (Temp.) (mmHg) LA", "PAO2",
        "5686", "PO2(art_kap) (Temp.) (mmHg) LA", "PAO2",
        # BGA
        "2796", "BGA (Quelle)", "BGA",
        "3676", "BGA (Quelle)", "BGA",
        # FIO2
        "675", "FiO2 (%) gemessen", "FIO2",
        "1995", "FiO2 (%)", "FIO2",
        "3733", "FiO2 (%) Einstellung", "FIO2",
        "4240", "FiO2 (%) Einstellung", "FIO2",
        "5052", "FiO2 (%) Einstellung", "FIO2",
        "5474", "Carina_FiO2 eingestellt", "FIO2",
        # IBP
        "1646", "IBP m (mmHg)", "IBP",
        "1653", "NIBP m (mmHg)", "NIBP",
        # DOB
        "275", "Dobutamin (250/50) Perf.", "DOB",
        "4967", "Dobutamin (250/50) Perf.", "DOB",
        "3493", "Dobutamin (500/50) Perf.", "DOB",
        "4968", "Dobutamin (500/50) Perf.", "DOB",
        # NOR
        "3466", "Noradrenalin ( 1/50) Perf.", "NOR",
        "4962", "Noradrenalin ( 1/50) Perf.", "NOR",
        "3467", "Noradrenalin ( 5/50) Perf.", "NOR",
        "4963", "Noradrenalin ( 5/50) Perf.", "NOR",
        "1132", "Noradrenalin (10/50) Perf.", "NOR",
        "4964", "Noradrenalin (10/50) Perf.", "NOR",
        "3468", "Noradrenalin (25/50) Perf.", "NOR",
        "4965", "Noradrenalin (25/50) Perf.", "NOR",
        "3469", "Noradrenalin (50/50) Perf.", "NOR",
        "4966", "Noradrenalin (50/50) Perf.", "NOR",
        # BILI
        "2059", "Bilirubin gesamt", "BILI",
        "2276", "Bilirubin direkt", "BILI",
        "3163", "Bilirubin (mmol/l)", "BILI",
        "3677", "Bilirubin (mmol/l) BGA", "BILI",
        # PLT
        "2033", "Thrombocyten", "PLT",
        "2040", "Thrombocyten Citratblut", "PLT",
        # CREA
        "2057", "Kreatinin", "CREA",
        # LAC
        "2778", "Lactat", "LAC",
        "3686", "Lactat (mmol/L) LA", "LAC",
        "2054", "Lactat (mmol/l) LA", "LAC"
        ), ncol=3L, byrow=TRUE,
        dimnames=list(NULL, c("Id", "Description", "Type"))
    ), stringsAsFactors=FALSE)
    mapping$Type[match(x, mapping$Id)]
}
