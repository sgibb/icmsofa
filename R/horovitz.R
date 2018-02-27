#' Calculate Horovitz Index
#'
#' @param pao2 double, paO2
#' @param fio2 double, FiO2
#' @return double
#' @noRd
.horovitz <- function(pao2, fio2) {
    stopifnot(length(pao2) == length(fio2))
    stopifnot(all(10 <= pao2 & pao2 <= 600 | is.na(pao2)))
    stopifnot(all(0.21 <= fio2 & fio2 <= 1.0 | is.na(fio2)))
    pao2 / fio2
}
