#' Add SOFA scores to `data.frame`
#'
#' @param x `data.frame`
#' @param lag `numeric`, lag seconds added to reference date and extend the
#' range to 24 h + lag seconds (e.g. laboratory values take some time)
#' @param lagOnlyLaboratory `logical` add lag seconds only to the laboratory
#' values?
#' @param na.rm `logical`, should missing values replaced by zero?
#' @param estimatedRespirationParams `character`, strategy to handle estimated
#' respiratory parameters (Horovitz based on EPAO2/O2INS)
#' @param verbose `logical`, verbose output?
#' @return `data.frame`
#' @export
addSofa <- function(x, lag=0L, lagOnlyLaboratory=TRUE, na.rm=FALSE,
    estimatedRespirationParams=c("inferior", "ignore", "keep"),
    verbose=interactive()) {
    x <- .addSubScores(x, verbose=verbose)
    x <- .addSofaScores(
        x,
        lag=lag,
        lagOnlyLaboratory=lagOnlyLaboratory,
        na.rm=na.rm,
        estimatedRespirationParams=estimatedRespirationParams,
        verbose=verbose)
    x
}

#' Add SOFA-scores.
#'
#' Add the sofa-scores to the data.frame.
#'
#' @param x `data.frame`
#' @param lag `numeric`, lag seconds added to reference date and extend the
#' range to 24 h + lag seconds (e.g. laboratory values take some time)
#' @param lagOnlyLaboratory `logical` add lag seconds only to the laboratory
#' values?
#' @param na.rm `logical`, should missing values replaced by zero?
#' @param estimatedRespirationParams `character`, strategy to handle estimated
#' respiratory parameters (Horovitz based on EPAO2/O2INS)
#' @param verbose `logical`, verbose output?
#' @return `data.frame`
#' @noRd
.addSofaScores <- function(x, lag=0L, lagOnlyLaboratory=TRUE, na.rm=FALSE,
    estimatedRespirationParams=c("inferior", "ignore", "keep"),
    verbose=interactive()) {
    x <- x[order(x$CaseId, x$Date), ]

    if (na.rm) {
        x$SOFA <- 0L
    } else {
        x$SOFA <- NA_integer_
    }

    split(x, x$CaseId) <- lapply(
            split(x, x$CaseId),
            .calculateSofa,
            lag=lag,
            lagOnlyLaboratory=lagOnlyLaboratory,
            na.rm=na.rm,
            estimatedRespirationParams=match.arg(estimatedRespirationParams),
            verbose=verbose
    )

    ## remove useless rownames
    rownames(x) <- NULL
    x
}

#' Add SOFA subscores.
#'
#' Add the subscores to the data.frame.
#'
#' @param x `data.frame`
#' @param verbose `logical`, verbose output?
#' @return `data.frame`
#' @noRd
.addSubScores <- function(x, verbose=interactive()) {
    if (verbose) {
        message("Add subscores for BILI, PLT, CREA ...")
    }
    x[, .sofaItems] <- NA_integer_

    ## easy scores
    isBili <- x$Type == "BILI" & x$Valid
    isPlt <- x$Type == "PLT" & x$Valid
    isCrea <- x$Type == "CREA" & x$Valid
    x$BILI[isBili] <- .bilirubin2sofa(x$Value[isBili])
    x$PLT[isPlt] <- .platelets2sofa(x$Value[isPlt])
    x$CREA[isCrea] <- .creatinine2sofa(x$Value[isCrea])

    ## complicated scores
    if (verbose) {
        message("Add subscores for RESP ...")
    }
    x <- .addRespirationSubScore(x)
    if (verbose) {
        message("Add subscores for CIRC ...")
    }
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
    sb <- x[x$Type %in% c("EPAO2", "PAO2", "FIO2", "O2INS") & x$Valid,]

    isFiO2 <- sb$Type %in% c("FIO2", "O2INS")

    beCol <- c("FiO2Begin", "FiO2End")
    fioCols <- c("FiO2", "FiO2Type", "FiO2CaseId", beCol)
    sb[, fioCols] <- NA_real_
    sb[isFiO2, fioCols] <-
        sb[isFiO2, c("Value", "Type", "CaseId", "Begin", "End")]
    sb[, fioCols] <- lapply(sb[, fioCols], .fillNa)

    sb <- sb[sb$Type %in% c("EPAO2", "PAO2"),]
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
            sb$FiO2Type == "O2INS") & sb$CaseId == sb$FiO2CaseId &
            sb$Valid & !is.na(sb$FiO2)

    ## If FiO2 time doesn't match, use 0.21 as default
    sb$FiO2[!isValid] <- 0.21
    sb$FiO2Type[!isValid] <- "FiO2"
    sb$Value <- .horovitz(sb$Value, sb$FiO2)
    sb$RESP <- .horovitz2sofa(sb$Value, sb$FiO2Type != "O2INS")
    sb$Type <- ifelse(
        sb$Type == "EPAO2" | sb$FiO2Type == "O2INS", "EHORV", "HORV"
    )
    sb$Description <- ifelse(
        sb$Type == "EPAO2" | sb$FiO2Type == "O2INS",
        "estimated Horovitz", "Horovitz"
    )
    sb <- sb[
        !duplicated(sb[, c("CaseId", "Date", "Type", "Value"), drop=FALSE]),,
        drop=FALSE
    ]

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
    isCirc <- x$Type %in% c("MAP", "DOB", "NOR") & x$Valid
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
#' @param lag `numeric`, lag seconds added to reference date and extend the
#' range to 24 h + lag seconds (e.g. laboratory values take some time)
#' @param lagOnlyLaboratory `logical` add lag seconds only to the laboratory
#' values?
#' @param na.rm `logical`, should missing values replaced by zero?
#' @param estimatedRespirationParams `character`, strategy to handle estimated
#' respiratory parameters (Horovitz based on EPAO2/O2INS)
#' @param verbose `logical`, verbose output?
#' @return `data.frame`
#' @noRd
.calculateSofa <- function(x, lag=0L, lagOnlyLaboratory=TRUE, na.rm=FALSE,
    estimatedRespirationParams=c("inferior", "ignore", "keep"),
    verbose=interactive()) {
    estimatedRespirationParams <- match.arg(estimatedRespirationParams)
    if (verbose) {
        message(
            "Calculate SOFA for ", x$CaseId[1L],
            " (", nrow(x), " timepoints) ..."
        )
        pb <- txtProgressBar(min=0L, max=nrow(x), style=3L)
        on.exit(close(pb))
    }
    for (i in seq_len(nrow(x))) {
        x$SOFA[i] <- .sofaAt(
            x,
            x$Date[i],
            lag=lag,
            lagOnlyLaboratory=lagOnlyLaboratory,
            na.rm=na.rm,
            estimatedRespirationParams=estimatedRespirationParams
        )["SOFA"]
        if (verbose) {
            setTxtProgressBar(pb, i)
        }
    }
    x
}

#' SOFA score at timepoint
#'
#' @param x `data.frame`
#' @param tp `POSIXct`, timepoint
#' @param lag `numeric`, lag seconds added to reference date and extend the
#' range to 24 h + lag seconds (e.g. laboratory values take some time)
#' @param lagOnlyLaboratory `logical` add lag seconds only to the laboratory
#' values?
#' @param estimatedRespirationParams `character`, strategy to handle estimated
#' respiratory parameters (Horovitz based on EPAO2/O2INS)
#' @param na.rm `logical`, should missing values replaced by zero?
#' @return `data.frame`
#' @noRd
.sofaAt <- function(x, tp, lag=0L, lagOnlyLaboratory=TRUE,
                    estimatedRespirationParams=c("inferior", "ignore", "keep"),
                    na.rm=FALSE) {
    estimatedRespirationParams <- match.arg(estimatedRespirationParams)

    scores <- rep.int(NA_integer_, 6L)
    names(scores) <- c(.sofaItems, "SOFA")
    lag <- lag * as.integer(
        .sofaItems %in% c("BILI", "CREA", "PLT") | !lagOnlyLaboratory
    )
    names(lag) <- .sofaItems

    sb <- x[x$Type != "EHORV",, drop=FALSE]

    for (item in .sofaItems) {
        scores[item] <- .valueAt(
            sb,
            tp,
            vcol=item,
            lag=lag[item],
            fun=.maxNa
        )
    }

    scores["RESP"] <- .respScoreForEstimatedParams(
        x, tp=tp, resp=scores["RESP"],
        method=estimatedRespirationParams,
        lag=lag["RESP"]
    )

    scores["SOFA"] <- sum(scores[.sofaItems], na.rm=na.rm)
    scores
}

#' Recalculate estimated parameters (Horovitz based on EPAO2 or O2INS)
#'
#' @param x `data.frame`
#' @param tp `POSIXct`, timepoint
#' @param resp `numeric`, SOFA for RESP
#' @param method `character`, how to handle estimated parameters
#' @param lag `numeric`, lag seconds added to reference date and extend the
#' range to 24 h + lag seconds (e.g. laboratory values take some time)
#' @noRd
.respScoreForEstimatedParams <- function(x, tp, resp,
    method=c("inferior", "ignore", "keep"), lag=0L) {

    method <- match.arg(method)

    if (method == "keep" || (method == "inferior" && is.na(resp))) {
        score <- .valueAt(
            x[x$Type == "EHORV",, drop=FALSE],
            tp,
            vcol="RESP",
            lag=lag,
            fun=.maxNa
        )

        resp <- .maxNa(c(resp, score))
    }
    resp
}

#' Calculate value over timeperiod
#'
#' @param x `data.frame`
#' @param tp `POSIXct`, timepoint
#' @param vcol `character`, column that contains the value that should
#' summarised/calculated.
#' @param lag `numeric`, lag seconds added to reference date and extend the
#' range to 24 h + lag seconds (e.g. laboratory values take some time)
#' @param fun `function`, to apply over the values
#' @param \ldots further arguments passed to `fun`.
#' @noRd
.valueAt <- function(x, tp, vcol="Value", lag=0L, fun=.maxNa, ...) {
    match.fun(fun)(x[.prev24h(x$Date, ref=tp, lag=lag) & x$Valid, vcol], ...)
}
