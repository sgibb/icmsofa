#' Plot SOFA scores
#'
#' Plotting scores
#'
#' @param x `data.frame`
#' @param path `character`, file path
#' @param timepoints `data.frame`, with timepoints
#' @export
plotSofa <- function(x, path, timepoints=NULL) {
    invisible(lapply(split(x, x$CaseId), function(sb) {
        file <- file.path(path, paste(sb$CaseId[1L], "png", sep="."))
        if (!is.null(timepoints)) {
            .plotSofa(
                sb, file=file,
                timepoints=unlist(
                    timepoints[
                        timepoints[,1L] == sb$CaseId[1L],
                        seq_len(ncol(timepoints) - 1L) + 1L,
                        drop=TRUE
                    ]
                )
            )
        } else {
            .plotSofa(sb, file=file)
        }
    }))
}

#' Plot SOFA and SubScores
#'
#' Internal function
#'
#' @param x `data.frame`, single patient
#' @param file `character`, filename (if given plotted to png)
#' @param timepoints `double`, named timepoints
#' @noRd
.plotSofa <- function(x, file=NULL, timepoints=NULL) {
    if (!is.null(file)) {
        png(
            file,
            height=1440,
            width=min((length(.hourly(x$Date)) / 2L + 12L) * 50L, 30000),
            pointsize=18
        )
        on.exit(dev.off(), add=TRUE)
    } else {
        old <- par(no.readonly=TRUE)
        on.exit(par(old))
    }
    layout(matrix(1L:6L), heights=c(2L, 2L, 1L, 1L, 1L, 1L))
           #heights=c(rep(2L, 5L), 3L))
    par(mar=c(0L, 9L, 0L, 3L))

    d <- data.frame(
        nms=c("FIO2", "PAO2", "HORV", "SOFA"),
        lnms=c("FiO2", "PaO2 [mmHg]", "Horovitz [mmHg]", "SOFA Subscore"),
        col=c("#A6CEE3", "#1F78B4", "#B2DF8A", "#B15928"),
        pch=c(20, 20, 17, 15),
        scl=c(1L, 500L, 500L, 4L),
        line=c(0L, 3L, 6L, 0L),
        side=c(2L, 2L, 2L, 4L),
        stringsAsFactors=FALSE
    )
    .plotSubScores(x, d, timepoints)

    d <- data.frame(
        nms=c("IBP", "DOB", "NOR", "SOFA"),
        lnms=c("Mean (Non)-Invasive Blood Pressure [mmHg]", "Dobutamine [ml/h]",
               "Norepinephrine [\u00B5g/kg/min]", "SOFA Subscore"),
        col=c("#E31A1C", "#6A3D9A", "#CAB2D6", "#B15928"),
        pch=c(20, 20, 20, 15),
        scl=c(150L, 10L, 1L, 4L),
        line=c(0L, 3L, 6L, 0L),
        side=c(2L, 2L, 2L, 4L),
        stringsAsFactors=FALSE
    )
    .plotSubScores(x, d, timepoints)

    d <- data.frame(
        nms=c("BILI", "SOFA"),
        lnms=c("Bilirubin [\u00B5mol/l]", "SOFA Subscore"),
        col=c("#33A02C", "#B15928"),
        pch=c(20, 15),
        scl=c(200L, 4L),
        line=c(0L, 0L),
        side=c(2L, 4L),
        stringsAsFactors=FALSE
    )
    .plotSubScores(x, d, timepoints)

    d <- data.frame(
        nms=c("PLT", "SOFA"),
        lnms=c("Platelets [Gpt/l]", "SOFA Subscore"),
        col=c("#FB9A99", "#B15928"),
        pch=c(20, 15),
        scl=c(150L, 4L),
        line=c(0L, 0L),
        side=c(2L, 4L),
        stringsAsFactors=FALSE
    )
    .plotSubScores(x, d, timepoints)

    d <- data.frame(
        nms=c("CREA", "SOFA"),
        lnms=c("Creatinine [\u00B5mol/l]", "SOFA Subscore"),
        col=c("#FDBF6F", "#B15928"),
        pch=c(20, 15),
        scl=c(450L, 4L),
        line=c(0L, 0L),
        side=c(2L, 4L),
        stringsAsFactors=FALSE
    )
    .plotSubScores(x, d, timepoints)

    par(mar=c(7L, 9L, 0L, 3L))
    .plotSofaScores(x, timepoints)
}

#' Plot SOFA SubScores
#'
#' @param x `data.frame`
#' @param d `data.frame`, with names, position, colors etc.
#' @param timepoints `double`, named timepoints
#' @noRd
.plotSubScores <- function(x, d, timepoints=NULL) {
    y <- seq(0, 1, by=0.2)
    iSofa <- nrow(d)

    at <- .hourly(x$Date)
    days <- .daily(x$Date)
    even <- as.numeric(at) %% 7200L == 0L

    plot(NA, xlim=range(at), ylim=c(0L, 1L), type="n",
         axes=FALSE, ann=FALSE, frame.plot=FALSE)

    abline(v=at[even], col="#808080", lwd=0.5, lty=3L)
    abline(h=0L, col="#808080", lwd=0.5, lty=1L)
    abline(v=days, col="#808080", lwd=0.5)

    if (!is.null(timepoints)) {
        abline(v=timepoints, col="#FF7F00")
        text(
            x=timepoints, y=0L, labels=names(timepoints), srt=90, adj=c(0L, 1.1),
            col="#FF7F00", cex=1.2
        )
    }

    for (i in seq_len(nrow(d) - 1L)) {
        points(
            x$Date[x$Type == d$nms[i]],
            x$Value[x$Type == d$nms[i]] / d$scl[i],
            col=d$col[i], pch=d$pch[i], type="b"
        )

        points(
            x$Date[x$Type == d$nms[i]],
            x$SubScore[x$Type == d$nms[i]] / d$scl[iSofa],
            col=d$col[iSofa], type="s", lwd=1.2
        )

        points(
            x$Date[x$Type == d$nms[i]],
            x$SubScore[x$Type == d$nms[i]] / d$scl[iSofa],
            col=d$col[iSofa], type="p", pch=d$pch[iSofa], cex=1.2
        )

        axis(side=d$side[i], at=y, labels=FALSE, line=d$line[i], col=d$col[i])
        mtext(
            side=d$side[i], at=y, text=y * d$scl[i], line=d$line[i] + 0.5,
            col=d$col[i], cex=0.8
        )
        mtext(
            side=d$side[i], line=d$line[i] + 1.5, text=d$lnms[i], col=d$col[i],
            cex=0.8
        )
    }
    axis(
        side=d$side[iSofa], at=seq(0, 1, by=0.25), labels=FALSE,
        line=d$line[iSofa]
    )
    mtext(
        side=d$side[iSofa], at=seq(0, 1, by=0.25), text=0L:4L, col=d$col[iSofa],
        line=d$line[iSofa] + 1L, cex=0.8
    )
    mtext(
        side=d$side[iSofa], line=d$line[iSofa] + 2L, text=d$lnms[iSofa],
        col=d$col, cex=0.8
    )
    legend(
        "bottomright", legend=d$lnms, col=d$col, pch=d$pch, lwd=1L, bty="n"
    )
}

#' Plot SOFA Scores
#'
#' @param x `data.frame`
#' @param timepoints `double`, named timepoints
#' @param at `POSIXct`, hourly sequence
#' @noRd
.plotSofaScores <- function(x, timepoints=NULL) {
    y <- seq(0L, 24L, by=2L)
    y2 <- seq(0L, 24L, by=4L)
    at <- .hourly(x$Date)
    days <- .daily(x$Date)
    even <- as.numeric(at) %% 7200L == 0L

    plot(NA, xlim=range(at), ylim=c(0L, 24L), type="n",
         axes=FALSE, ann=FALSE, frame.plot=FALSE)

    abline(v=at[even], col="#808080", lwd=0.5, lty=3L)
    abline(h=0L, col="#808080", lwd=0.5, lty=1L)
    abline(v=days, col="#808080", lwd=0.5)
    text(x=rep(days, each=length(y2)), y=rep(y2, length(days)), labels=y2,
         col="#808080", adj=c(0L, 0.5), cex=0.5)

    if (!is.null(timepoints)) {
        abline(v=timepoints, col="#FF7F00")
        text(
            x=timepoints, y=0L, labels=names(timepoints), srt=90, adj=c(0L, 1.1),
            col="#FF7F00", cex=1.2
        )
    }

    axis(side=1L, at=at[even], labels=FALSE)
    text(
        x=at[even], y=par("usr")[3L], labels=format(at[even], "%Y-%m-%d %H:%M"),
        srt=60, adj=c(1.1, 0.5), xpd=TRUE
    )

    points(x$Date, x$Sofa, type="s", lwd=1.2, col="#B15928")
    points(x$Date, x$Sofa, type="p", pch=20L, cex=1.2, col="#B15928")
    axis(side=2L, at=y2, labels=FALSE, line=0L, col="#B15928")
    axis(side=4L, at=y2, labels=FALSE, line=0L, col="#B15928")
    mtext(side=2L, at=y2, text=y2, col="#B15928", line=1L, cex=0.5)
    mtext(side=4L, at=y2, text=y2, col="#B15928", line=1L, cex=0.5)
    mtext(side=2L, text="SOFA Score", col="#B15928", line=2L, cex=0.5)
    mtext(side=4L, text="SOFA Score", col="#B15928", line=2L, cex=0.5)
    legend(
        "bottomright", legend="SOFA Score", col="#B15928", pch=20L, lwd=1L,
        bty="n"
    )
    title(sub=x$CaseId[1L], adj=1L, cex=2L)
}
