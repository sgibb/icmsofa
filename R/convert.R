#' Correct Begin and End Time for FiO2 data
#'
#' In contrast to perfusor data or O2 data the BEGIN and END times reflect the
#' time when the order was initiated on the chart and not when the measurement
#' starts or ends.
#'
#' Here we set BEGIN to ADMINDATE and END to the BEGIN of the next FiO2 value
#' (or if it is above the threshold to BEGIN + threshold)
#'
#' @param x `data.frame`
#' @param threshold `numeric`, threshold time in seconds
#' @return `data.frame`
#' @noRd
.correctFiO2Times <- function(x, threshold=3600) {
    isFiO2 <- x$Type == "FIO2" & x$Valid
    sb <- x[isFiO2,]
    sb$Begin <- sb$Date
    nr <- nrow(sb)
    samePatient <- sb$CaseId[-1L] == sb$CaseId[-nr]
    sb$End[-nr][samePatient] <- sb$Begin[-1L][samePatient]

    d <- difftime(sb$End, sb$Begin, units="secs") > threshold
    sb$End[d] <- sb$Begin[d] + threshold
    x[isFiO2,] <- sb
    x
}

#' O2-Flow rate to FiO2
#'
#' based on http://www.cscc.imise.uni-leipzig.de/Studien/MEDUSA/CRF-Patient/Konversionstabelle-Oxygenierungsindex.pdf
#'
#' O2-Flow (l/min)  estimated FiO2
#' nasula canula
#' 1  0,24
#' 2  0,28
#' 3  0,32
#' 4  0,36
#' 5  0,40
#' 6  0,44
#' face mask
#' 5    0,40
#' 6-7  0,50
#' 7-8  0,60
#'
#' we ignore the application form:
#'
#' @param x double, O2 flow rate
#' @return double, estimated FiO2
#' @noRd
.o2FlowRateToFiO2 <- function(x) {
    stopifnot(is.numeric(x))
    c(0.21, 0.24, 0.28, 0.32, 0.36, 0.40, 0.5, 0.6)[
      .bincode(x, breaks=c(-Inf, 0:6, Inf))
    ]
}

#' SpO2 to PaO2
#'
#' based on http://www.cscc.imise.uni-leipzig.de/Studien/MEDUSA/CRF-Patient/Konversionstabelle-Oxygenierungsindex.pdf
#'
#' @param x double, SpO2
#' @return double, estimated PaO2
#' @noRd
.spo2ToPaO2 <- function(x) {
    stopifnot(is.numeric(x))
    c(44, 45, 46, 47, 49, 50, 52, 53, 55, 57, 60, 62, 65, 69, 73, 79, 86, 96,
        112, 145)[
        .bincode(x, breaks=c(-Inf, 80:98, Inf))
    ]
}
