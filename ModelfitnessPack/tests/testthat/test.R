context("Calculate the all fit statistics of models")
test_that("Correctly get the fit statistics", {
  expect_that(FitStats(y = seq(1, 100, by=1) +  rnorm(100, 0, 1), 
                       p = matrix(seq(1, 100, by=1), ncol=1), 
                       r = rep(median(y), 100), 
                       fits = c("rmse", "mad", "mape", "meape", "rmsle", "mrae")),
              is_true(is.matrix())
)})