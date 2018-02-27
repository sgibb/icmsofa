#' Add SOFA scores.
#'
#' Add SOFA scores to `data.frame`
#'
#' @param x `data.frame`
#' @return `data.frame`
#' @export
addSofa <- function(x) {
    x <- .addSubScores(x)
    x <- .addSubScores(x)
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
    x$SubScore[x$Type == "BIL"] <- .bilirubin2sofa(x$Value[x$Type == "BIL"])
    x$SubScore[x$Type == "PLT"] <- .platelets2sofa(x$Value[x$Type == "PLT"])
    x$SubScore[x$Type == "CRE"] <- .creatinine2sofa(x$Value[x$Type == "CRE"])
    isCirc <- x$Type %in% c("DOB", "IBP", "NOR")
    x$SubScore[isCirc] <- .circulation2sofa(x$Value[isCirc], x$Type[isCirc])

    ## complicated scores
    x <- do.call("rbind", lapply(split(x, x$Id), .addRespiratorySubScore))
    x <- x[order(x$Id, x$Date),]

    ## remove useless rownames
    rownames(x) <- NULL
    x
}

#' Add SOFA-scores.
#'
#' Add the sofa-scores to the data.frame.
#'
#' @param x `data.frame`
#' @return `data.frame`
#' @noRd
.addSofaScores <- function(x) {
    x$Sofa <- NA_integer_
    x$TypeId <- .sofaTypeId(x$Type)

    ## complicated scores
    x <- do.call("rbind", lapply(split(x, x$Id), .calculateSofa))

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
    sb <- x[x$Type %in% c("PAO", "FIO"),]

    if (!nrow(sb)) {
        return(x)
    }

    sb$FiO2Date <- sb$FiO2 <- sb$PaO2 <- NA_real_
    sb$FiO2[sb$Type == "FIO"] <- sb$Value[sb$Type == "FIO"]
    sb$PaO2[sb$Type == "PAO"] <- sb$Value[sb$Type == "PAO"]
    sb$FiO2Date[sb$Type == "FIO"] <- sb$Date[sb$Type == "FIO"]
    sb$FiO2 <- .fillNa(sb$FiO2)
    sb$FiO2Date <- as.POSIXct(
        .fillNa(sb$FiO2Date),
        origin="1970-01-01 00:00:00",
        tz="UTC"
    )
    sb$diff <- sb$Date - sb$FiO2Date
    ## If diff time larger than 3600 sec set FiO2 to 0.21 (in general FiO2 is
    ## sampled every 1800 sec).
    ## Could fail if the patient is in operation theater, ct or something else
    sb$FiO2[sb$diff > 3600] <- 0.21
    sb$Horovitz <- .horovitz(sb$PaO2, sb$FiO2)
    ## Assumen ventilation for FiO2 > 0.21
    sb$SubScore <- .horovitz2sofa(sb$Horovitz, ventilation=sb$FiO2 > 0.21)
    sb <- sb[sb$Type == "PAO" & !is.na(sb$Horovitz), ]
    if (!nrow(sb)) {
        return(x)
    }
    sb$Type <- "HOR"
    sb$Value <- sb$Horovitz
    sb$Description <- "Horovitz"
    rbind(x, sb[, c("Id", "Date", "Description", "Value", "Type", "SubScore")])
}

#' SOFA Score
#'
#' Calculate 24 h SOFA-Score.
#'
#' @param x `data.frame`
#' @return `data.frame`
#' @noRd
.calculateSofa <- function(x) {
    for (i in seq_len(nrow(x))) {
        sel <- (x$Date[i] - 24L * 3600L) < x$Date[i] & x$Date < x$Date[i]
        if (any(sel)) {
            sb <- x[sel, , drop=FALSE]
            x$Sofa[i] <-
                sum(
                    vapply(
                        1L:5L,
                        function(ti).maxNa(sb$SubScore[sb$TypeId == ti]),
                        NA_integer_
                    )
                )
        }
    }
    x
}