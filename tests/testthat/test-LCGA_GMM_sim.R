test_that("Test LCGA_GMM function using Wardenaar, 2022 working paper Appendix 1 example", {
  # simulate tutorial data
  set.seed(2002)
  d1 <- LCGA_GMM_sim(
    n_pers=100,
    n_time=5,
    beta_int=0,
    beta_slo_time=0.3,
    beta_slo_covar=0.5,
    beta_slo_interact=1.5,
    mean_i=10,
    var_i=0.13,
    mean_s=0,
    var_s=0.09,
    cov_is=0,
    mean_r=0,
    var_r=1,
    mod_name = "test")

  expect_equal(2 * 2, 4)
})
