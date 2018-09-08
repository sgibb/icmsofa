context("filter")

test_that(".filterBga", {
    d <- data.frame(CaseId=1,
                    Date=rep(1:3, each=2),
                    Type=c("BGA", "PAO2", "BGA", "PAO2", "BGA", "PAO2"),
                    Value=c(1, 100, 2, 50, 999, 0),
                    Valid=TRUE,
                    stringsAsFactors=FALSE)
    expect_equal(icmsofa:::.filterBga(d, keep=c("a", "v", "m")), d)
    r <- d
    r$Valid[3:6] <- FALSE
    expect_equal(icmsofa:::.filterBga(d, keep="a"), r)
    r$Valid <- TRUE
    r$Valid[5:6] <- FALSE
    expect_equal(icmsofa:::.filterBga(d, keep=c("a", "v")), r)
    expect_message(icmsofa:::.filterBga(d, keep=c("a", "v"), verbose=TRUE),
                   "1 paO2 values removed")
    expect_silent(icmsofa:::.filterBga(d, keep=c("a", "v"), verbose=FALSE))
    r$Valid <- TRUE
    r$Valid[1:4] <- FALSE
    expect_equal(icmsofa:::.filterBga(d, keep=("m")), r)
    expect_error(icmsofa:::.filterBga(d, keep=""))
})
