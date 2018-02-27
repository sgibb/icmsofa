context("utils")

test_that(".fillNa", {
    expect_equal(sofa:::.fillNa(1:10), 1:10)
    expect_equal(sofa:::.fillNa(c(NA, 1, NA, NA, 2, NA, 1, NA, 3)),
                 c(NA, 1, 1, 1, 2, 2, 1, 1, 3))
    expect_equal(sofa:::.fillNa(c(1, NA, 2, 1, 3, NA)),
                 c(1, 1, 2, 1, 3, 3))
})

test_that(".maxNa", {
    expect_equal(sofa:::.maxNa(1:10), 10)
    expect_equal(sofa:::.maxNa(c(NA, 1, NA, NA, 2, NA, 1, NA, 3)), 3)
    expect_equal(sofa:::.maxNa(c(NA, NA, NA)), NA_integer_)
    expect_equal(sofa:::.maxNa(NULL), NA_integer_)
})

test_that(".sofaTypeId", {
    expect_error(sofa:::.sofaTypeId(1:10))
    expect_equal(sofa:::.sofaTypeId(
        c("HOR", "DOB", "NOR", "IBP", "BIL", "PLT", "CRE", "FOO")),
        c(1, 2, 2, 2, 3:5, NA))
}
