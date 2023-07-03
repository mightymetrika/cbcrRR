test_that("sim_plot", {
  plots <- sim_plot(ssr_rss_2r, "rmse_i", c("np", "nt", "bint", "bst", "mi", "vi", "ms", "vs", "cis", "mr", "vr"))
  expect_equal(length(plots), 288)
})
