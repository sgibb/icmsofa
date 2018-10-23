context("timepoints")

test_that("extractTimeFrame", {
    d <- data.frame(CaseId=rep(1:2, c(6, 3)),
                    Date=as.POSIXct(
                        c("2014-02-26 05:00:00",
                          "2014-02-26 07:00:00",
                          "2014-02-26 07:55:00",
                          "2014-02-26 08:00:00",
                          "2014-02-27 05:00:00",
                          "2014-02-27 08:00:00",
                          "2014-04-12 05:00:00",
                          "2014-04-12 07:00:00",
                          "2014-04-12 19:00:00"), tz="UTC"),
                    Valid=TRUE,
                    stringsAsFactors=FALSE)
    tp <- data.frame(Caseid=1:2,
                     Date=as.POSIXct(
                        c(
                          "2014-02-27 05:00:00",
                          "2014-04-12 07:00:00"), tz="UTC"),
                    stringsAsFactors=FALSE)
    r <- d[c(1:5, 7:8),]
    expect_equal(extractTimeFrame(d, tp), r)
    r <- d[c(1:6, 7:8),]
    expect_equal(extractTimeFrame(d, tp, lag=3*3600), r)
})
