test_that("extract_lcga_info works", {
  set.seed(123)
  sim_data <- LCGA_GMM_sim(n_pers = 100, n_time = 5, beta_int = 0, beta_slo_time = 1,
                           beta_slo_covar = 0, mean_i = 0, var_i = 1, mean_s = 0,
                           var_s = 1, cov_is = 0, mean_r = 0, var_r = 1, mod_name = "test_model",
                           covar_noise = c(0, 1))

  lcga_mods <- run_lcga_models(sim_data = sim_data)
  lcga_res <- extract_lcga_info(lcga_mods)
  expect_equal(length(lcga_res), 4)
})

test_that("extract_lme4_info works", {
  set.seed(123)
  sim_data <- LCGA_GMM_sim(n_pers = 100, n_time = 5, beta_int = 0, beta_slo_time = 1,
                           beta_slo_covar = 0, mean_i = 0, var_i = 1, mean_s = 0,
                           var_s = 1, cov_is = 0, mean_r = 0, var_r = 1, mod_name = "test_model")

  lme4_mods <- run_lme4_models(sim_data = sim_data)
  lme4_res <- extract_lme4_info(lme4_mods)
  expect_equal(length(lme4_res), 4)
})

test_that("extract_lm_info works", {
  set.seed(123)
  sim_data <- LCGA_GMM_sim(n_pers = 100, n_time = 5, beta_int = 0, beta_slo_time = 1,
                           beta_slo_covar = 0, mean_i = 0, var_i = 1, mean_s = 0,
                           var_s = 1, cov_is = 0, mean_r = 0, var_r = 1, mod_name = "test_model")

  lm_mods <- run_lm_models(sim_data = sim_data)
  lm_res <- extract_lm_info(lm_mods)
  expect_equal(length(lm_res), 2)
})

test_that("extract_sand_info works", {
  set.seed(123)
  sim_data <- LCGA_GMM_sim(n_pers = 100, n_time = 5, beta_int = 0, beta_slo_time = 1,
                           beta_slo_covar = 0, mean_i = 0, var_i = 1, mean_s = 0,
                           var_s = 1, cov_is = 0, mean_r = 0, var_r = 1, mod_name = "test_model")

  lm_mods <- run_lm_models(sim_data = sim_data)
  sand_res <- extract_sand_info(lm_mods)
  expect_equal(length(sand_res), 2)
})

test_that("extract_cbc_info works", {
  set.seed(123)
  sim_data <- LCGA_GMM_sim(n_pers = 100, n_time = 5, beta_int = 0, beta_slo_time = 1,
                           beta_slo_covar = 0, mean_i = 0, var_i = 1, mean_s = 0,
                           var_s = 1, cov_is = 0, mean_r = 0, var_r = 1, mod_name = "test_model",
                           covar_noise = c(0, 1))

  cbc_mods <- run_cbc_models(sim_data = sim_data)
  cbc_res <- extract_cbc_info(cbc_mods)
  expect_equal(length(cbc_res), 2)
})


test_that("extract_cbcRR_info works", {
  set.seed(123)
  sim_data <- LCGA_GMM_sim(n_pers = 100, n_time = 5, beta_int = 0, beta_slo_time = 1,
                           beta_slo_covar = 0, mean_i = 0, var_i = 1, mean_s = 0,
                           var_s = 1, cov_is = 0, mean_r = 0, var_r = 1, mod_name = "test_model",
                           covar_noise = c(0, 1))

  cbc_mods <- run_cbc_models(sim_data = sim_data)
  cbcRR_res <- extract_cbcRR_info(cbc_mods)
  expect_equal(length(cbcRR_res), 2)
})

