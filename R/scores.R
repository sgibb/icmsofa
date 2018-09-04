#' Horovitz to SOFA
#'
#' < 400 	    1
#' < 300 	    2
#' < 200 + vent 3
#' < 100 + vent 4
#'
#' @param x double, Horovitz
#' @param ventilated logical, (default: TRUE)
#' @return integer, 0:4
#' @noRd
.horovitz2sofa <- function(x, ventilation=TRUE) {
    (x < 400) + (x < 300) + ventilation * ((x < 200) + (x < 100))
}

#' Circulation to SOFA
#'
#' Mean arterial pressure OR administration of vasopressors required 	SOFA score
#' MAP ≥ 70 mm/Hg = 0
#' MAP < 70 mm/Hg = 1
#' dopamine ≤ 5 µg/kg/min or dobutamine (any dose) = 2
#' dopamine > 5 µg/kg/min OR epinephrine ≤ 0.1 µg/kg/min OR norepinephrine ≤ 0.1 µg/kg/min = 3
#' dopamine > 15 µg/kg/min OR epinephrine > 0.1 µg/kg/min OR norepinephrine > 0.1 µg/kg/min = 4
#'
#' @param x double, value
#' @param type IBP/DOB/NOR
#' @return integer 0:4
#' @noRd
.circulation2sofa <- function(x, type) {
    ((type == "IBP") * (x < 70L)) +
    ((type == "DOB") * 2L) +
    ((type == "NOR") * ((x > 0.1) + 3L))
}

#' Bilirubin to SOFA
#'
#' Bilirubin (mg/dl) [μmol/L] 	SOFA score
#' < 1.2 [< 20] 	    0
#' 1.2–1.9 [20-32] 	    1
#' 2.0–5.9 [33-101] 	2
#' 6.0–11.9 [102-204] 	3
#' > 12.0 [> 204] 	    4
#'
#' @param x double, bilirubin (in µmol/l)
#' @return integer, 0:4
#' @noRd
.bilirubin2sofa <- function(x) {
    .bincode(x, c(0L, 20, 33, 102, 205, Inf), right=FALSE) - 1L
}

#' Platelets to SOFA
#'
#' Platelets×103/µl 	SOFA score
#' ≥ 150 	0
#' < 150 	1
#' < 100 	2
#' < 50 	3
#' < 20 	4
#'
#' @param x double, platelets
#' @return integer, 0:4
#' @noRd
.platelets2sofa <- function(x) {
    5L - .bincode(x, c(0L, 20, 50, 100, 150, Inf), right=FALSE)
}

#' Creatinine to SOFA
#'
#' mg/dl [µmol/l]
#' < 1.2 [< 110] 	0
#' 1.2–1.9 [110-170] 	1
#' 2.0–3.4 [171-299] 	2
#' 3.5–4.9 [300-440] (or < 500 ml/d) 	3
#' > 5.0 [> 440] (or < 200 ml/d) 	4
#'
#' @param x double, creatinine (in µmol/l)
#' @return integer, 0:4
#' @noRd
.creatinine2sofa <- function(x) {
    .bincode(x, c(0L, 110, 171, 300, 441, Inf), right=FALSE) - 1L
}
