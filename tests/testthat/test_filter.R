context("filter")

test_that(".filterIbp", {
    expect_error(sofa:::.filterIbp("A"))
    expect_error(sofa:::.filterIbp(1:10, range="A"))
    expect_error(sofa:::.filterIbp(1:10, range=1.0))
    expect_equal(sofa:::.filterIbp(c(10, 20, 50, 100, 150, 200)),
                 c(NA, 20, 50, 100, 150, NA))
    expect_equal(sofa:::.filterIbp(c(10, 20, 50, 100, 150, 200), range=c(50, 100)),
                 c(NA, NA, 50, 100, NA, NA))
})
