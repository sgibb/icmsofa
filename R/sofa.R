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
    x$SubScore <- NA_integer_

    ## easy scores
    isBili <- x$Type == "BILI" & x$Valid
    isPlt <- x$Type == "PLT" & x$Valid
    isCrea <- x$Type == "CREA" & x$Valid
    x$SubScore[isBili] <- .bilirubin2sofa(x$Value[isBili])
    x$SubScore[isPlt] <- .platelets2sofa(x$Value[isPlt])
    x$SubScore[isCrea] <- .creatinine2sofa(x$Value[isCrea])

    isCirc <- x$Type %in% c("DOP", "DOB", "IBP", "NOR") & x$Valid
    x$SubScore[isCirc] <- .circulation2sofa(x$Value[isCirc], x$Type[isCirc])

    ## complicated scores
    x <- do.call("rbind", lapply(split(x, x$CaseId), .addRespiratorySubScore))
    x <- x[order(x$CaseId, x$Date),]

    ## remove useless rownames
    rownames(x) <- NULL
    x
}

#' Add SOFA-scores.
#'
#' Add the sofa-scores to the data.frame.
#'
#' @param x `data.frame`
#' @param na.rm `logical`, should missing values replaced by zero?
#' @return `data.frame`
#' @noRd
.addSofaScores <- function(x, na.rm=FALSE) {
    x$Sofa <- ifelse(na.rm, 0L, NA_integer_)
    x$TypeId <- .sofaTypeId(x$Type)

    x <- do.call(
        "rbind",
        lapply(split(x, x$CaseId), .calculateSofa, na.rm=na.rm)
    )

    ## remove useless column and rownames
    x <- x[, colnames(x) != "TypeId", drop=FALSE]
    rownames(x) <- NULL
    x
}

#' Add Respiratory Subscore.
#'
#' Add the subscores for the respiratory system.
#'
#' @param x `data.frame`
#' @return `data.frame`
#' @noRd
.addRespiratorySubScore <- function(x) {
    sb <- x[x$Type %in% c("PAO2", "FIO2"),]

    if (!nrow(sb)) {
        return(x)
    }

    sb$FiO2Date <- sb$FiO2 <- sb$PaO2 <- NA_real_
    sb$FiO2[sb$Type == "FIO2"] <- sb$Value[sb$Type == "FIO2"]
    sb$PaO2[sb$Type == "PAO2"] <- sb$Value[sb$Type == "PAO2"]
    sb$FiO2Date[sb$Type == "FIO2"] <- sb$Date[sb$Type == "FIO2"]
    sb$FiO2 <- .fillNa(sb$FiO2)
    sb$FiO2Date <- .asPosixCt(.fillNa(sb$FiO2Date))

    ## If FiO2 is missing assume 0.21
    sb$FiO2[is.na(sb$FiO2)] <- 0.21
    sb$diff <- sb$Date - sb$FiO2Date
    ## If diff time larger than 3600 sec set FiO2 to 0.21 (in general FiO2 is
    ## sampled every 1800 sec).
    ## Could fail if the patient is in operation theater, ct or something else
    sb$FiO2[sb$diff > 3600] <- 0.21
    sb$Horovitz <- .horovitz(sb$PaO2, sb$FiO2)
    ## Assumen ventilation for FiO2 > 0.21
    sb$SubScore <- .horovitz2sofa(sb$Horovitz, ventilation=sb$FiO2 > 0.21)
    sb <- sb[sb$Type == "PAO2" & !is.na(sb$Horovitz), ]
    if (!nrow(sb)) {
        return(x)
    }
    sb$Type <- "HORV"
    sb$Value <- sb$Horovitz
    sb$Description <- "Horovitz"
    rbind(x, sb[, colnames(x), drop=FALSE])
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
