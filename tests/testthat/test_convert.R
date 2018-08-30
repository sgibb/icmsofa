context("convert")

test_that(".o2FlowRateToFiO2", {
    expect_error(icmsofa:::.o2FlowRateToFiO2("A"))
    expect_equal(
        icmsofa:::.o2FlowRateToFiO2(1:10),
        c(0.24, 0.28, 0.32, 0.36, 0.40, 0.50, 0.60, 0.60, 0.60, 0.60)
    )
})
