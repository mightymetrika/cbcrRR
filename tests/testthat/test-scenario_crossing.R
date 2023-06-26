test_that("scenario_crossing returns correct output structure", {
  set.seed(123)
  scenarios <- scenario_crossing(n_pers=c(100, 200), n_time=c(5, 6))

  # Test output type
  expect_equal(class(scenarios), "list")

  # Test output size
  expect_equal(length(scenarios), 768)

  # Test each dataframe structure
  for (df in scenarios) {
    expect_s3_class(df, "data.frame")
    expect_equal(ncol(df), 8)
    expected_cols <- c("ID", "time", "covar", "int", "slo", "slo_cov",
                       "y", "mod_name")
    expect_equal(colnames(df), expected_cols)
    expect_true(all(!is.na(df)))
  }
})

test_that("scenario_crossing returns different results with different seeds", {
  set.seed(123)
  scenarios1 <- scenario_crossing(n_pers=c(100, 200), n_time=c(5, 6))

  set.seed(456)
  scenarios2 <- scenario_crossing(n_pers=c(100, 200), n_time=c(5, 6))

  expect_false(all(sapply(scenarios1, function(df) mean(df$y)) == sapply(scenarios2, function(df) mean(df$y))))
})
