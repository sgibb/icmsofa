context("utils")

test_that(".asPosixCt", {
    expect_equal(icmsofa:::.asPosixCt("26.02.14 22:50:10"),
        as.POSIXct("2014-02-26 22:50:10", tz="UTC"))
    expect_equal(icmsofa:::.asPosixCt("26022014 225010", "%d%m%Y %H%M%S"),
        as.POSIXct("2014-02-26 22:50:10", tz="UTC"))
})

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

test_that(".inRange", {
    expect_error(icmsofa:::.inRange("A"))
    expect_error(icmsofa:::.inRange(1:10))
    expect_error(icmsofa:::.inRange(1:10, range="A"))
    expect_error(icmsofa:::.inRange(1:10, range=1.0))
    expect_equal(icmsofa:::.inRange(1:10, 2, 9),
                 c(FALSE, rep(TRUE, 8), FALSE))
    expect_equal(icmsofa:::.inRange(c(NA, 1:10), 2, 9),
                 c(NA, FALSE, rep(TRUE, 8), FALSE))

    expect_equal(1:10 %range% c(2, 9),
                 c(FALSE, rep(TRUE, 8), FALSE))
    expect_equal(1:10 %inside% c(2, 9),
                 c(FALSE, FALSE, rep(TRUE, 6), FALSE, FALSE))
})

test_that(".maxNa", {
    expect_equal(icmsofa:::.maxNa(1:10), 10)
    expect_equal(icmsofa:::.maxNa(c(NA, 1, NA, NA, 2, NA, 1, NA, 3)), 3)
    expect_equal(icmsofa:::.maxNa(c(NA, NA, NA)), NA_integer_)
    expect_equal(icmsofa:::.maxNa(NULL), NA_integer_)
})

test_that(".minNa", {
    expect_equal(icmsofa:::.minNa(1:10), 1)
    expect_equal(icmsofa:::.minNa(c(NA, 1, NA, NA, 2, NA, 1, NA, 3)), 1)
    expect_equal(icmsofa:::.minNa(c(NA, NA, NA)), NA_integer_)
    expect_equal(icmsofa:::.minNa(NULL), NA_integer_)
})

test_that(".prev24h", {
    x <- as.POSIXct(c("2018-08-11 21:40", "2018-08-11 21:44",
                      "2018-08-11 21:45", "2018-08-11 21:46",
                      "2018-08-12 21:45", "2018-08-12 21:46"),
                    format="%Y-%m-%d %H:%M",
                    origin="1970-01-01 00:00", tz="UTC")
    expect_equal(icmsofa:::.prev24h(x, x[4]),
                 c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
    expect_equal(icmsofa:::.prev24h(x, x[5]),
                 c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))
    expect_equal(icmsofa:::.prev24h(x, x[6]),
                 c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
    expect_equal(icmsofa:::.prev24h(x, x[3], 60),
                 c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
    expect_equal(icmsofa:::.prev24h(x, x[5], 0, -60),
                 c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE))
    expect_equal(icmsofa:::.prev24h(x, x[5], 0, 60),
                 c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
})
