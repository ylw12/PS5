context("Calculate the all fit statistics of models")
expect_is(FitStats(y = seq(1, 100, by=1) +  rnorm(100, 0, 1), 
                   p = matrix(seq(1, 100, by=1), ncol=1), 
                   r = rep(50.5, 100), 
                   fits = c("rmse", "mad", "mape", "meape", "rmsle", "mrae")), "matrix")
