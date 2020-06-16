context("Palceholder")

library(fishflux)

test_that("Simple corner cases", {
    model <- cnp_model_mcmc(TL = 10, param = list(Qc_m = 40, Qn_m = 10, Qp_m = 4, theta_m = 3))
    expect_is(model, "list")
})
