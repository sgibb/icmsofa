context("sofa")

test_that(".addRespirationSubScore", {
    r <- data.frame(CaseId=c(1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3),
                    Date=as.POSIXct(
                        c("2014-02-26 07:00:00",
                          "2014-02-26 07:30:00",
                          "2014-02-26 07:55:00",
                          "2014-02-26 07:55:00",
                          "2014-02-26 07:55:00",
                          "2014-02-26 08:00:00",
                          "2014-03-11 07:00:00",
                          "2014-03-11 09:00:00",
                          "2014-03-11 09:00:00",
                          "2014-04-12 07:00:00",
                          "2014-04-12 19:00:00",
                          "2014-04-12 19:00:00"), tz="UTC"),
                    Begin=as.POSIXct(
                        c("2014-02-26 07:00:00",
                          "2014-02-26 07:30:00",
                          "2014-02-26 07:55:00",
                          "2014-02-26 07:55:00",
                          "2014-02-26 07:55:00",
                          "2014-02-26 08:00:00",
                          "2014-03-11 07:00:00",
                          "2014-03-11 09:00:00",
                          "2014-03-11 09:00:00",
                          "2014-04-12 07:00:00",
                          "2014-04-12 19:00:00",
                          "2014-04-12 19:00:00"), tz="UTC"),
                    End=as.POSIXct(
                        c("2014-02-26 07:30:00",
                          "2014-02-26 08:00:00",
                          "2014-02-26 08:00:00",
                          "2014-02-26 08:00:00",
                          "2014-02-26 08:00:00",
                          "2014-02-26 09:00:00",
                          "2014-03-11 08:00:00",
                          "2014-03-11 10:00:00",
                          "2014-03-11 10:00:00",
                          "2014-04-14 08:00:00",
                          "2014-04-12 10:00:00",
                          "2014-04-12 10:00:00"), tz="UTC"),
                    Type=c("FIO2", "FIO2", "PAO2", "PAO2", "HORV",
                           "FIO2", "FIO2", "PAO2", "HORV",
                           "O2INS", "PAO2", "EHORV"),
                    Value=c(0.5, 0.4, 100, 100, 250, 0.3, 0.3, 80, 80/0.21,
                            0.4, 85, 212.5),
                    RESP=NA_integer_,
                    Valid=TRUE,
                    stringsAsFactors=FALSE)
    r$RESP[c(5, 9, 12)] <- c(2, 1, 2)
    d <- r[-c(5, 9, 12),]
    rownames(d) <- NULL
    expect_equal(icmsofa:::.addRespirationSubScore(d), r)
})

test_that(".addCirculationSubScore", {
    r <- data.frame(CaseId=c(1, 1, 1, 1, 1, 2, 2, 3, 3),
                    Date=as.POSIXct(
                        c("2014-02-26 05:00:00",
                          "2014-02-26 07:00:00",
                          "2014-02-26 07:00:00",
                          "2014-02-26 07:55:00",
                          "2014-02-26 08:00:00",
                          "2014-03-11 07:00:00",
                          "2014-03-11 09:00:00",
                          "2014-04-12 07:00:00",
                          "2014-04-12 19:00:00"), tz="UTC"),
                    Begin=as.POSIXct(
                        c("2014-02-26 05:00:00",
                          "2014-02-26 07:00:00",
                          "2014-02-26 07:00:00",
                          "2014-02-26 07:55:00",
                          "2014-02-26 08:00:00",
                          "2014-03-11 07:00:00",
                          "2014-03-11 09:00:00",
                          "2014-04-12 07:00:00",
                          "2014-04-12 19:00:00"), tz="UTC"),
                    End=as.POSIXct(
                        c("2014-02-28 10:00:00",
                          "2014-02-26 08:00:00",
                          "2014-02-26 08:00:00",
                          "2014-02-26 09:00:00",
                          "2014-03-11 08:00:00",
                          "2014-03-11 08:30:00",
                          "2014-03-11 10:00:00",
                          "2014-04-14 08:00:00",
                          "2014-04-12 10:00:00"), tz="UTC"),
                    Type=c("MAP", "NOR", "DOB", "MAP", "NOR", "NOR", "MAP", "NOR", "MAP"),
                    Value=c(65, 2.1, 1, 65, 0.05, 0.11, 75, 0.1, 65),
                    CIRC=c(1, 4, 4, 4, 3, 4, 0, 3, 3),
                    Valid=TRUE,
                    stringsAsFactors=FALSE)
    d <- r
    d$CIRC <- NA_integer_
    rownames(d) <- NULL
    expect_equal(icmsofa:::.addCirculationSubScore(d), r)
})

test_that(".respScoreForEstimatedParams", {
    d <- data.frame(CaseId=1,
                    Date=as.POSIXct(
                        c("2014-02-26 05:00:00",
                          "2014-02-26 07:00:00",
                          "2014-02-26 07:00:00",
                          "2014-02-26 07:55:00",
                          "2014-02-26 08:00:00"), tz="UTC"),
                    Type=c("EHORV", "HORV", "HORV", "EHORV", "HORV"),
                    Value=c(210, 199, 150, 80, 100),
                    RESP=c(2, 3, 3, 4, 1),
                    Valid=TRUE,
                    stringsAsFactors=FALSE)
    expect_equal(icmsofa:::.respScoreForEstimatedParams(d, d$Date[2],
        resp=NA, method="inferior"), 2)
    expect_equal(icmsofa:::.respScoreForEstimatedParams(d, d$Date[2],
        resp=3, method="inferior"), 3)
    expect_equal(icmsofa:::.respScoreForEstimatedParams(d, d$Date[5],
        resp=NA, method="keep"), 4)
    expect_equal(icmsofa:::.respScoreForEstimatedParams(d, d$Date[5],
        resp=3, method="keep"), 4)
    expect_equal(icmsofa:::.respScoreForEstimatedParams(d, d$Date[3],
        resp=3, method="keep"), 3)
    expect_equal(icmsofa:::.respScoreForEstimatedParams(d, d$Date[5],
        resp=NA, method="ignore"), NA)
    expect_equal(icmsofa:::.respScoreForEstimatedParams(d, d$Date[5],
        resp=3, method="ignore"), 3)
})

test_that(".valueAt", {
    d <- data.frame(CaseId=1,
                    Date=as.POSIXct(
                        c("2014-02-26 05:00:00",
                          "2014-02-26 07:00:00",
                          "2014-02-26 07:00:00",
                          "2014-02-26 07:55:00",
                          "2014-02-26 08:00:00",
                          "2014-03-11 07:00:00",
                          "2014-03-11 09:00:00",
                          "2014-04-12 07:00:00",
                          "2014-04-12 19:00:00"), tz="UTC"),
                    Type=c("MAP", "NOR", "DOB", "MAP", "NOR", "NOR", "MAP", "NOR", "MAP"),
                    Value=c(65, 2.1, 1, 65, 0.05, 0.11, 75, 0.1, 65),
                    CIRC=c(1, 4, 4, 4, 3, 4, 0, 3, 3),
                    Valid=TRUE,
                    stringsAsFactors=FALSE)
    expect_equal(icmsofa:::.valueAt(d, d$Date[5], vcol="CIRC"), 4)
    expect_equal(icmsofa:::.valueAt(d, d$Date[5], vcol="CIRC", fun=min), 1)
    expect_equal(icmsofa:::.valueAt(d[d$Type == "NOR",], d$Date[5],
                                    vcol="Value",), 2.1)
    expect_equal(icmsofa:::.valueAt(d[d$Type == "NOR",], d$Date[2],
                                    vcol="Value", fun=min, lag=3*3600), 0.05)
    d$Valid[2] <- FALSE
    expect_equal(icmsofa:::.valueAt(d[d$Type == "NOR",], d$Date[5],
                                    vcol="Value"), 0.05)
})
