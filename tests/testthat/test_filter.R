context("filter")

test_that(".filterPaO2", {
    expect_equal(icmsofa:::.filterPaO2(c(9, 10, 400, 600, 601)),

                 c(NA, 10, 400, 600, NA))
})

test_that(".filterFiO2", {
    expect_equal(icmsofa:::.filterFiO2(c(0.20, 0.21, 1.0, 1.01)),
                 c(NA, 0.21, 1.0, NA))
})

test_that(".filterIbp", {
    expect_equal(icmsofa:::.filterIbp(c(10, 20, 50, 100, 150, 200)),
                 c(NA, 20, 50, 100, 150, NA))
    expect_equal(icmsofa:::.filterIbp(c(10, 20, 50, 100, 150, 200),
                                      range=c(50, 100)),
                 c(NA, NA, 50, 100, NA, NA))
})

test_that(".filterRange", {
    expect_error(icmsofa:::.filterRange("A"))
    expect_error(icmsofa:::.filterRange(1:10))
    expect_error(icmsofa:::.filterRange(1:10, range="A"))
    expect_error(icmsofa:::.filterRange(1:10, range=1.0))
    expect_message(icmsofa:::.filterRange(1:10, c(2, 9), verbose=TRUE),
                   "2 values removed")
    expect_silent(icmsofa:::.filterRange(1:10, c(0, 11), verbose=TRUE))
    expect_equal(icmsofa:::.filterRange(1:10, c(2, 9), verbose=FALSE),
                 c(NA, 2:9, NA))
    expect_equal(icmsofa:::.filterRange(c(NA, 1:10), c(2, 9), verbose=FALSE),
                 c(NA, NA, 2:9, NA))
})

test_that(".filterBga", {
    d <- data.frame(CaseId=1,
                    Date=rep(1:3, each=2),
                    Type=c("BGA", "PAO2", "BGA", "PAO2", "BGA", "PAO2"),
                    Value=c(1, 100, 2, 50, 9, 0),
                    stringsAsFactors=FALSE)
    expect_equal(icmsofa:::.filterBga(d, keep=c("a", "v", "m")), d)
    r <- d
    r$Value[c(4, 6)] <- NA
    expect_equal(icmsofa:::.filterBga(d, keep="a"), r)
    r <- d
    r$Value[6] <- NA
    expect_equal(icmsofa:::.filterBga(d, keep=c("a", "v")), r)
    r <- d
    r$Value[c(2, 4)] <- NA
    expect_message(icmsofa:::.filterBga(d, keep=c("a", "v"), verbose=TRUE),
                   "1 paO2 values removed")
    expect_silent(icmsofa:::.filterBga(d, keep=c("a", "v"), verbose=FALSE))
    expect_equal(icmsofa:::.filterBga(d, keep=("m")), r)
    expect_error(icmsofa:::.filterBga(d, keep=""))
})
