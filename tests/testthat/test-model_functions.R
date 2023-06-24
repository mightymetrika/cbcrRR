test_that("run_lcga_models works", {
  set.seed(123)
  sim_data <- LCGA_GMM_sim(n_pers = 100, n_time = 5, beta_int = 0, beta_slo_time = 1,
                           beta_slo_covar = 0, beta_slo_interact = 0, mean_i = 0, var_i = 1, mean_s = 0,
                           var_s = 1, cov_is = 0, mean_r = 0, var_r = 1, mod_name = "test_model")

  lcga_mods <- run_lcga_models(sim_data = sim_data)
  expect_equal(length(lcga_mods), 6)
})

test_that("run_lme4_models works", {
  set.seed(123)
  sim_data <- LCGA_GMM_sim(n_pers = 100, n_time = 5, beta_int = 0, beta_slo_time = 1,
                           beta_slo_covar = 0, beta_slo_interact = 0, mean_i = 0, var_i = 1, mean_s = 0,
                           var_s = 1, cov_is = 0, mean_r = 0, var_r = 1, mod_name = "test_model")

  lme4_mods <- run_lme4_models(sim_data = sim_data)
  expect_equal(length(lme4_mods), 6)
})

test_that("run_lm_models works", {
  set.seed(123)
  sim_data <- LCGA_GMM_sim(n_pers = 100, n_time = 5, beta_int = 0, beta_slo_time = 1,
                           beta_slo_covar = 0, beta_slo_interact = 0, mean_i = 0, var_i = 1, mean_s = 0,
                           var_s = 1, cov_is = 0, mean_r = 0, var_r = 1, mod_name = "test_model")

  lm_mods <- run_lm_models(sim_data = sim_data)
  expect_equal(length(lm_mods), 3)
})

# test_that("run_cbc_models works", {
#   set.seed(123)
#   sim_data <- LCGA_GMM_sim(n_pers = 100, n_time = 5, beta_int = 0, beta_slo_time = 1,
#                            beta_slo_covar = 0, beta_slo_interact = 0, mean_i = 0, var_i = 1, mean_s = 0,
#                            var_s = 1, cov_is = 0, mean_r = 0, var_r = 1, mod_name = "test_model")
#
#   cbc_mods <- run_cbc_models(sim_data = sim_data)
#
#   lcbc <- length(cbc_mods)
#   expect_equal(lcbc, 3)
# })
