context("utils")

test_that(".daily", {
    expect_equal(icmsofa:::.daily(as.POSIXct(
        c("2014-02-26 22:50:10 UTC", "2014-02-27 00:02:00 UTC",
          "2014-02-27 00:02:00 UTC"))),
        as.POSIXct(c("2014-02-26 00:00:00 UTC", "2014-02-27 00:00:00 UTC",
                     "2014-02-28 00:00:00 UTC")))
})

test_that(".fillNa", {
    expect_equal(icmsofa:::.fillNa(1:10), 1:10)
    expect_equal(icmsofa:::.fillNa(c(NA, 1, NA, NA, 2, NA, 1, NA, 3)),
                 c(NA, 1, 1, 1, 2, 2, 1, 1, 3))
    expect_equal(icmsofa:::.fillNa(c(1, NA, 2, 1, 3, NA)),
                 c(1, 1, 2, 1, 3, 3))
})

test_that(".hourly", {
    expect_equal(icmsofa:::.hourly(as.POSIXct(
        c("2014-02-26 22:50:10 UTC", "2014-02-27 00:02:00 UTC",
          "2014-02-27 00:02:00 UTC"))),
        as.POSIXct(c("2014-02-26 22:00:00 UTC", "2014-02-26 23:00:00 UTC",
                     "2014-02-27 00:00:00 UTC", "2014-02-27 01:00:00 UTC")))
})

test_that(".maxNa", {
    expect_equal(icmsofa:::.maxNa(1:10), 10)
    expect_equal(icmsofa:::.maxNa(c(NA, 1, NA, NA, 2, NA, 1, NA, 3)), 3)
    expect_equal(icmsofa:::.maxNa(c(NA, NA, NA)), NA_integer_)
    expect_equal(icmsofa:::.maxNa(NULL), NA_integer_)
})

test_that(".prev24h", {
    x <- as.POSIXct(c("2018-08-11 21:40", "2018-08-11 21:44",
                      "2018-08-11 21:45", "2018-08-11 21:46",
                      "2018-08-12 21:45", "2018-08-12 21:46"),
                    format="%Y-%m-%d %H:%M",
                    origin="1970-01-01 00:00", tz="UTC")
    expect_error(icmsofa:::.prev24h(1:10, 2))
    expect_error(icmsofa:::.prev24h(x, 2))
    expect_equal(icmsofa:::.prev24h(x, x[4]),
                 c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
    expect_equal(icmsofa:::.prev24h(x, x[5]),
                 c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))
    expect_equal(icmsofa:::.prev24h(x, x[6]),
                 c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
})

test_that(".sofaTypeId", {
    expect_error(icmsofa:::.sofaTypeId(1:10))
    expect_equal(icmsofa:::.sofaTypeId(
        c("HORV", "DOB", "NOR", "IBP", "BILI", "PLT", "CREA", "FOO")),
        c(1, 2, 2, 2, 3:5, NA))
})
