#' Add SOFA scores.
#'
#' Add SOFA scores to `data.frame`
#'
#' @param x `data.frame`
#' @param na.rm `logical`, should missing values replaced by zero?
#' @return `data.frame`
#' @export
addSofa <- function(x, na.rm=FALSE) {
    x <- .addSubScores(x)
    x <- .addSofaScores(x, na.rm=na.rm)
    x
}

#' Add SOFA subscores.
#'
#' Add the subscores to the data.frame.
#'
#' @param x `data.frame`
#' @return `data.frame`
#' @noRd
.addSubScores <- function(x) {
    x[, .sofaItems] <- NA_integer_

    ## easy scores
    isBili <- x$Type == "BILI" & x$Valid
    isPlt <- x$Type == "PLT" & x$Valid
    isCrea <- x$Type == "CREA" & x$Valid
    x$BILI[isBili] <- .bilirubin2sofa(x$Value[isBili])
    x$PLT[isPlt] <- .platelets2sofa(x$Value[isPlt])
    x$CREA[isCrea] <- .creatinine2sofa(x$Value[isCrea])

    ## complicated scores
    x <- .addRespirationSubScore(x)
    x <- .addCirculationSubScore(x)
    x
}

#' Add Respiration Subscore.
#'
#' Add the subscores for the respiratory system.
#'
#' @param x `data.frame`
#' @param threshold `numeric`, max allowed minutes from last FiO2 from the
#' respirator
#' @return `data.frame`
#' @noRd
.addRespirationSubScore <- function(x, threshold=3600) {
    x <- x[order(x$CaseId, x$Date), ]
    sb <- x[x$Type %in% c("PAO2", "FIO2", "O2INS") & x$Valid,]

    isFiO2 <- sb$Type %in% c("FIO2", "O2INS")

    beCol <- c("FiO2Begin", "FiO2End")
    fioCols <- c("FiO2", "FiO2Type", "FiO2CaseId", beCol)
    sb[, fioCols] <- NA_real_
    sb[isFiO2, fioCols] <-
        sb[isFiO2, c("Value", "Type", "CaseId", "Begin", "End")]
    sb[, fioCols] <- lapply(sb[, fioCols], .fillNa)

    sb <- sb[sb$Type == "PAO2",]
    sb[, beCol] <- lapply(
        sb[, beCol],
        as.POSIXct, origin="1970-01-01 00:00:00", tz="UTC"
    )

    bedCol <- paste0(beCol, "Diff")
    sb[, bedCol] <- NA_real_
    sb[, bedCol] <- lapply(
        sb[, beCol],
        function(x)as.numeric(difftime(x, sb$Date, units="secs"))
    )

    isValid <- sb$Date %inside% sb[, beCol] &
        ((sb$FiO2Type == "FIO2" & sb$FiO2BeginDiff <= threshold) |
            sb$FiO2Type == "O2INS") & sb$CaseId == sb$FiO2CaseId

    ## If FiO2 time doesn't match, use 0.21 as default
    sb$FiO2[!isValid] <- 0.21
    sb$Value <- .horovitz(sb$Value, sb$FiO2)
    sb$RESP <- .horovitz2sofa(sb$Value, sb$FiO2Type != "O2INS")
    sb$Type <- "HORV"
    sb$Description <- "Horovitz"

    x <- rbind(x, sb[, colnames(x), drop=FALSE])
    x <- x[order(x$CaseId, x$Date), ]
    rownames(x) <- NULL
    x
}

#' Add Circulation Subscore.
#'
#' Add the subscores for the circulation system.
#'
#' @param x `data.frame`
#' @return `data.frame`
#' @noRd
.addCirculationSubScore <- function(x) {
    x <- x[order(x$CaseId, x$Date), ]

    ## the easy ones (score for each timestamp)
    isCirc <- x$Type %in% c("IBP", "DOB", "NOR") & x$Valid
    x$CIRC[isCirc] <- .circulation2sofa(x$Value[isCirc], x$Type[isCirc])

    ## the perfusor values spanning some longer time ranges
    beCols <- c("PerfBegin", "PerfEnd")
    cols <- c(beCols, "PerfCIRC", "PerfCaseId")

    for (type in c("DOB", "NOR")) {
        x[, cols] <- NA_real_
        isType <- x$Type == type & x$Valid
        x[isType, cols] <- x[isType, c("Begin", "End", "CIRC", "CaseId")]
        x[, cols] <- lapply(x[, cols], .fillNa)
        x[, beCols] <- lapply(
            x[, beCols],
            as.POSIXct, origin="1970-01-01 00:00:00", tz="UTC"
        )

        isValid <- x$Date %range% x[, beCols] &
            !is.na(x[, beCols[1L]]) &
            !is.na(x[, beCols[2L]]) &
            x$CaseId == x$PerfCaseId &
            ((x$PerfCIRC > x$CIRC & !is.na(x$CIRC)) |
             (is.na(x$CIRC) & !is.na(x$PerfCIRC)))
        x$CIRC[isValid] <- x$PerfCIRC[isValid]
        x[, cols] <- NULL
    }

    rownames(x) <- NULL
    x
}

#' SOFA Score
#'
#' Calculate 24 h SOFA-Score.
#'
#' @param x `data.frame`
#' @param na.rm `logical`, should missing values replaced by zero?
#' @return `data.frame`
#' @noRd
.calculateSofa <- function(x, na.rm=FALSE) {
    for (i in seq_len(nrow(x))) {
        sel <- .prev24h(x$Date, ref=x$Date[i])
        if (any(sel)) {
            sb <- x[sel, , drop=FALSE]
            x$Sofa[i] <-
                sum(
                    vapply(
                        1L:5L,
                        function(ti).maxNa(sb$SubScore[sb$TypeId == ti]),
                        NA_integer_
                    ),
                    na.rm=na.rm
                )
        }
    }
    x
}
