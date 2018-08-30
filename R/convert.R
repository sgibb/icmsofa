#' O2-Flow rate to FiO2
#'
#' based on http://www.cscc.imise.uni-leipzig.de/Studien/MEDUSA/CRF-Patient/Konversionstabelle-Oxygenierungsindex.pdf
#'
#' O2-Flow  Geschätzte
#' (l/min)  FiO2
#' Nasensonde, Nasenbrille
#' 1  0,24
#' 2  0,28
#' 3  0,32
#' 4  0,36
#' 5  0,40
#' 6  0,44
#' Gesichtsmaske
#' 5    0,40
#' 6-7  0,50
#' 7-8  0,60
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
