test_that("Test that summarize_simulation_results works", {
  expect_equal(nrow(ssr_rss_2r), 2281)
  expect_equal(ncol(ssr_rss_2r), 32)
})
