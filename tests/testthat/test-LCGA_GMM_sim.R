test_that("Test LCGA_GMM function using Wardenaar, 2022 working paper Appendix 1 example", {
  # simulate tutorial data
  set.seed(2002)
  d1 <- LCGA_GMM_sim(
    n_pers=100,
    n_time=5,
    beta_int=0,
    beta_slo_time=0.3,
    mean_i=10,
    var_i=0.13,
    mean_s=0,
    var_s=0.09,
    cov_is=0,
    mean_r=0,
    var_r=1,
    mod_name = "test")

  expect_equal(length(d1), 6)
  expect_equal(nrow(d1), 500)
})

test_that("LCGA_GMM_sim returns correct output structure", {
  set.seed(123)
  sim_data <- LCGA_GMM_sim(n_pers = 100, n_time = 5, beta_int = 0, beta_slo_time = 1,
                           mean_i = 0, var_i = 1, mean_s = 0,
                           var_s = 1, cov_is = 0, mean_r = 0, var_r = 1, mod_name = "test_model")

  # Test output type
  expect_s3_class(sim_data, "data.frame")

  # Test output size
  expect_equal(nrow(sim_data), 100 * 5)
  expect_equal(ncol(sim_data), 6)

  # Test output column names
  expected_cols <- c("ID", "time", "int", "slo", "y", "mod_name")
  expect_equal(colnames(sim_data), expected_cols)

  # Test model name
  expect_equal(unique(sim_data$mod_name), "test_model")

  # Test for no missing values
  expect_true(all(!is.na(sim_data)))
})

test_that("LCGA_GMM_sim returns different results with different seeds", {
  set.seed(123)
  sim_data1 <- LCGA_GMM_sim(n_pers = 100, n_time = 5, beta_int = 0, beta_slo_time = 1,
                            mean_i = 0, var_i = 1, mean_s = 0,
                            var_s = 1, cov_is = 0, mean_r = 0, var_r = 1, mod_name = "test_model")

  set.seed(456)
  sim_data2 <- LCGA_GMM_sim(n_pers = 100, n_time = 5, beta_int = 0, beta_slo_time = 1,
                            mean_i = 0, var_i = 1, mean_s = 0,
                            var_s = 1, cov_is = 0, mean_r = 0, var_r = 1, mod_name = "test_model")

  expect_false(all(sim_data1$y == sim_data2$y))
})

# LCGA_GMM_sim2 <- function(n_pers, n_time, beta_int, beta_slo_time,
#                          mean_i, var_i, mean_s, var_s, cov_is,
#                          mean_r, var_r, mod_name = NA_character_) {
#
#   REff = MASS::mvrnorm(n_pers, mu=c(mean_i,mean_s), Sigma=rbind(c(var_i, cov_is),
#                                                                 c(cov_is, var_s) ))
#
#   colnames(REff) = c("intercept","slope_time");
#
#   dat= data.frame(ID = rep(1:n_pers, each=n_time),
#                   time = rep(1:n_time, times=n_pers),
#                   int = rep(REff[,1], each=n_time),
#                   slo = rep(REff[,2], each=n_time),
#                   y = NA,
#                   mod_name = mod_name)
#
#   dat$time <- dat$time-1
#
#   y = with(dat, (beta_int + int) +
#            + (beta_slo_time + slo)*time
#            + rnorm(n=n_pers*n_time, mean=mean_r, sd=sqrt(var_r)))
#   dat$y <-y
#
#   return(dat) }
#
# sim_data <- LCGA_GMM_sim2(n_pers = 100, n_time = 5, beta_int = 0, beta_slo_time = 1,
# mean_i = 0, var_i = 1, mean_s = 0,
# var_s = 1, cov_is = 0, mean_r = 0, var_r = 1)
