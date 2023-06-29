test_that("Test that run simulation study works", {
  # set.seed(287)
  # rss_2r <- run_simulation_study(n_reps = 2)
  expect_equal(length(rss_2r), 2)
})
