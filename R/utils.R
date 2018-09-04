#' Convert to POSIXct
#'
#' as.POSIXct with own defaults
#'
#' @param x `character`, date
#' @param format `character`, format
#' @return `POSIXct`
#' @noRd
.asPosixCt <- function(x, format="%d.%m.%y %H:%M:%S") {
    as.POSIXct(x, format=format, origin="1970-01-01 00:00:00", tz="UTC")
}

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

#' Is value in range
#'
#' @param x `numeric`
#' @param range `numeric`, allowed range
#' @return logical
#' @noRd
.inRange <- function(x, range) {
    stopifnot(is.numeric(x))
    stopifnot(is.numeric(range) && length(range) == 2L)
    if (range[1L] > range[2L]) {
        x >= range[2L] & x <= range[1L]
    } else {
        x >= range[1L] & x <= range[2L]
    }
}
"%inrange%" <- function(x, range) .inRange(x, range)

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

#' Previous 24 hours
#'
#' Return ids of last 24 hours
#'
#' @param x `POSIXct`, dates
#' @param ref `POSIXct`, reference date
#' @param lag `numeric`, lag seconds added to reference date and extend the
#' range to 24 h + lag seconds (e.g. laboratory values take some time)
#' @return `logical`
#' @noRd
.prev24h <- function(x, ref, lag=0) {
    stopifnot(inherits(x, "POSIXct"))
    stopifnot(inherits(ref, "POSIXct"))
    stopifnot(is.numeric(lag))
    x <- as.numeric(x)
    ref <- as.numeric(ref)
    x %inrange% c(ref - 24L * 3600, ref + lag)
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
        "HORV"=1L, "DOB"=2L, "NOR"=2L, "IBP"=2L, "BILI"=3L, "PLT"=4L, "CREA"=5L
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
        "2768", "PO2", "PAO2",
        "2794", "PO2 (mmHg)", "PAO2",
        "2813", "PO2 (Temp.) (mmHg)", "PAO2",
        "3626", "paO2 (mmHg)", "PAO2",
        "3691", "PO2 (Temp.) (mmHg) LA", "PAO2",
        "4397", "paO2 LA", "PAO2",
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
        "5492", "O2-Konzentration", "FIO2",
        # O2INS
        "1303", "O2-Insufflation", "O2INS",
        "2623", "Sauerstoff-Verabreichung", "O2INS",
        "4598", "O2-Insufflation", "O2INS",
        # IBP
        "1646", "IBP m (mmHg)", "IBP",
        "1653", "NIBP m (mmHg)", "IBP",
        # DOP
        "277",  "Dopamin (500/50) Perf.", "DOP",
        "3755", "Dopamin Perf", "DOP",
        # DOB
        "275", "Dobutamin (250/50) Perf.", "DOB",
        "4967", "Dobutamin (250/50) Perf.", "DOB",
        "3493", "Dobutamin (500/50) Perf.", "DOB",
        "4968", "Dobutamin (500/50) Perf.", "DOB",
        # NOR
        "1489", "Noradrenalin (Arterenol)", "NOR",
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
        "2061", "Kreatinin", "CREA",
        # LACT
        "2778", "Lactat", "LACT",
        "3686", "Lactat (mmol/L) LA", "LACT",
        "2054", "Lactat (mmol/l) LA", "LACT"
        ), ncol=3L, byrow=TRUE,
        dimnames=list(NULL, c("Id", "Description", "Type"))
    ), stringsAsFactors=FALSE)
    mapping$Type[match(x, mapping$Id)]
}
