context("horovitz")

test_that(".horovitz", {
    expect_error(icmsofa:::.horovitz(200:300, 1.0))
    expect_error(icmsofa:::.horovitz(200, c(0.9, 1.0)))
    expect_error(icmsofa:::.horovitz(0.1, 1.0))
    expect_error(icmsofa:::.horovitz(610, 1.0))
    expect_error(icmsofa:::.horovitz(200, 0.2))
    expect_error(icmsofa:::.horovitz(200, 1.1))
    expect_equal(icmsofa:::.horovitz(1:4 * 100, c(0.21, 0.4, 0.6, 0.8)),
                 1:4 * 100 / c(0.21, 0.4, 0.6, 0.8))
    expect_equal(icmsofa:::.horovitz(c(100, NA, 200), c(0.21, 0.3, NA)),
                 c(100 / 0.21, NA, NA))
})
