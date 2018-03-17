context("scores")

test_that(".horovitz2sofa", {
    expect_equal(icmsofa:::.horovitz2sofa(c(500, 400, 4:1 * 100 - 1)),
                 as.integer(c(0, 0:4)))
    expect_equal(icmsofa:::.horovitz2sofa(c(400, 4:1 * 100 - 1), FALSE),
                 as.integer(c(0:2, 2, 2)))
    expect_equal(icmsofa:::.horovitz2sofa(c(400, 4:1 * 100 - 1),
                                       c(FALSE, TRUE, FALSE, TRUE, FALSE)),
                 as.integer(c(0:3, 2)))
})

test_that(".circulation2sofa", {
    expect_equal(icmsofa:::.circulation2sofa(
                    c(70, 69, NA, 1, 0.01, 0.1, 0.2),
                    c("IBP", "IBP", "DOB", "DOB", "NOR", "NOR", "NOR")),
                 as.integer(c(0, 1, NA, 2, 3, 3, 4)))
})

test_that(".bilirubin2sofa", {
    expect_equal(icmsofa:::.bilirubin2sofa(
                    c(NA, 19, 20, 32, 33, 101, 102, 204, 205)),
                 c(NA, 0, 1, 1, 2, 2, 3, 3, 4))
})

test_that(".platelets2sofa", {
    expect_equal(icmsofa:::.platelets2sofa(
                    c(NA, 150, 149, 100, 99, 50, 49, 20, 19, 0)),
                 c(NA, 0, 1, 1, 2, 2, 3, 3, 4, 4))
})

test_that(".creatinine2sofa", {
    expect_equal(icmsofa:::.creatinine2sofa(
        c(NA, 109, 110, 170, 171, 299, 300, 440, 441, 500)
    ), c(NA, 0, 1, 1, 2, 2, 3, 3, 4, 4))
})
