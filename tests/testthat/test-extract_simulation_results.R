test_that("multiplication works", {
  ext_res <- extract_simulation_results(results = rss_2r)
  expect_equal(length(ext_res), 4385)
})
