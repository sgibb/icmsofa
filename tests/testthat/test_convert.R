context("convert")

test_that(".correctFiO2Times", {
    d <- data.frame(CaseId=c(1, 1, 1, 1, 2),
                    Date=as.POSIXct(
                        c("2014-02-26 22:33:00 UTC",
                          "2014-02-26 23:03:00 UTC",
                          "2014-02-27 02:00:00 UTC",
                          "2014-02-27 02:05:00 UTC",
                          "2014-03-11 09:05:00 UTC")),
                    Begin=as.POSIXct(
                        c("2014-02-26 22:00:00 UTC",
                          "2014-02-26 22:00:00 UTC",
                          "2014-02-26 22:00:00 UTC",
                          "2014-02-26 22:00:00 UTC",
                          "2014-03-11 09:00:00 UTC")),
                    End=as.POSIXct(
                        c("2014-02-28 00:02:00 UTC",
                          "2014-02-28 00:02:00 UTC",
                          "2014-02-28 00:02:00 UTC",
                          "2014-02-28 00:02:00 UTC",
                          "2014-03-11 10:10:00 UTC")),
                    Type=c("FIO2", "FIO2", "FIO2", "PAO2", "FIO2"),
                    Valid=TRUE,
                    stringsAsFactors=FALSE)

    r <- d
    r$Begin <- r$Date
    r$Begin[4] <- d$Begin[4]
    r$End[1] <- r$Begin[2]
    r$End[c(2:3, 5)] <- r$Date[c(2:3, 5)] + 3600
    expect_equal(icmsofa:::.correctFiO2Times(d), r)
})

test_that(".convertSpO2intoPaO2", {
    d <- data.frame(CaseId=c(1, 1, 1, 1),
                    Date=as.POSIXct(
                        c("2014-02-26 22:33:00 UTC",
                          "2014-02-26 23:03:00 UTC",
                          "2014-02-27 02:00:00 UTC",
                          "2014-03-11 09:05:00 UTC")),
                    Begin=as.POSIXct(
                        c("2014-02-26 22:00:00 UTC",
                          "2014-02-26 22:00:00 UTC",
                          "2014-02-26 22:00:00 UTC",
                          "2014-03-11 09:00:00 UTC")),
                    End=as.POSIXct(
                        c("2014-02-28 00:02:00 UTC",
                          "2014-02-28 00:02:00 UTC",
                          "2014-02-28 00:02:00 UTC",
                          "2014-03-11 10:10:00 UTC")),
                    Description=rep(c("FIO2", "SPO2"), c(1, 3)),
                    Type=rep(c("FIO2", "SPO2"), c(1, 3)),
                    Value=c(0.21, 80, 99, 98),
                    Valid=TRUE,
                    stringsAsFactors=FALSE)
    r <- rbind(d, d[2:4,])
    r$Value[5:7] <- c(44, 145, 112)
    r$Type[5:7] <- "EPAO2"
    r$Description[5:7] <- "estimated PaO2"
    r <- r[order(r$Date),]
    rownames(r) <- NULL
    expect_equal(icmsofa:::.convertSpO2intoPaO2(d), r)
})

test_that(".o2FlowRateToFiO2", {
    expect_error(icmsofa:::.o2FlowRateToFiO2("A"))
    expect_equal(
        icmsofa:::.o2FlowRateToFiO2(1:10),
        c(0.24, 0.28, 0.32, 0.36, 0.40, 0.50, 0.60, 0.60, 0.60, 0.60)
    )
})

test_that(".spo2ToPaO2", {
    expect_error(icmsofa:::.spo2ToPaO2("A"))
    expect_equal(
        icmsofa:::.spo2ToPaO2(79:100),
        c(44, 44, 45, 46, 47, 49, 50, 52, 53, 55, 57, 60, 62, 65, 69, 73, 79,
          86, 96, 112, 145, 145)
    )
})
