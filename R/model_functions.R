#' Run Latent Class Growth Analysis (LCGA) Models
#'
#' This function runs Latent Class Growth Analysis (LCGA) models on simulated data.
#' It uses the `hlme` function from the `lcmm` package to fit the models.
#' Two models are fitted: a random intercept model, and a random intercept and slope model.
#'
#' @param sim_data A data frame of simulated data, created by `LCGA_GMM_sim`.
#' It should include columns for the individual ID, time point, and response variable.
#' @param ng Number of classes/groups to be used in the LCGA models. Default is 1.
#'
#' @return A list of model results. The list includes two elements: 'ri',
#' which stores the result of the random intercept model,
#' and 'ris', which stores the result of the random intercept and slope model.
#'
#' @references
#' Proust-Lima, C., Philipps, V., & Liquet, B. (2017). Estimation of Extended Mixed Models Using
#' Latent Classes and Latent Processes: The R Package lcmm. Journal of Statistical Software, 78(2),
#' 1-56. doi:10.18637/jss.v078.i02
#'
#' @examples
#' sim_data <- LCGA_GMM_sim(n_pers = 100, n_time = 5, beta_int = 0, beta_slo_time = 1,
#' mean_i = 0, var_i = 1, mean_s = 0, var_s = 1, cov_is = 0, mean_r = 0, var_r = 1)
#' model_results <- run_lcga_models(sim_data, ng = 1)
#'
#' @export
run_lcga_models <- function(sim_data, ng = 1) {

  # List to store model results
  model_outs <- list()

  # Random intercept model
  model_outs$ri <- lcmm::hlme(y ~ time, random = ~ 1 | ID, subject = 'ID',
                                       data = sim_data, ng = ng)


  # Random intercept and slope model
  model_outs$ris <- lcmm::hlme(y ~ time, random = ~ time | ID, subject = 'ID',
                                        data = sim_data, ng = ng)


  return(model_outs)
}

#' Run Linear Mixed-Effects Models
#'
#' This function runs linear mixed-effects models on simulated data using the `lmer`
#' function from the `lme4` package. Two models are fitted: a random intercept model,
#' and a random intercept and slope model.
#'
#' @param sim_data A data frame of simulated data, created by `LCGA_GMM_sim`.
#' It should include columns for the individual ID, time point, and response variable.
#'
#' @return A list of model results. The list includes two elements: 'ri', which stores
#' the result of the random intercept model, and 'ris', which stores the result of the
#' random intercept and slope model.
#'
#' @references
#' Bates D, Maechler M, Bolker B, Walker S (2015). "Fitting Linear Mixed-Effects Models Using lme4."
#' Journal of Statistical Software, 67(1), 1-48. doi: 10.18637/jss.v067.i01.
#'
#' @examples
#' sim_data <- LCGA_GMM_sim(n_pers = 100, n_time = 5, beta_int = 0, beta_slo_time = 1,
#' mean_i = 0, var_i = 1, mean_s = 0, var_s = 1, cov_is = 0, mean_r = 0, var_r = 1)
#' model_results <- run_lme4_models(sim_data)
#'
#' @export
run_lme4_models <- function(sim_data) {

  # List to store model results
  model_outs <- list()

  # Random intercept model
  model_outs$ri <- lme4::lmer(y ~ time + (1 | ID), data = sim_data)

  # Random intercept and slope model
  model_outs$ris <- lme4::lmer(y ~ time + (time | ID), data = sim_data)

  return(model_outs)
}

#' Run Ordinary Least Squares (OLS) Regression Model
#'
#' This function runs an Ordinary Least Squares (OLS) regression model on simulated data
#' using the `lm` function from the `stats` package.
#'
#' @param sim_data A data frame of simulated data, created by `LCGA_GMM_sim`.
#' It should include columns for the time point and response variable.
#'
#' @return A list of model results. The list includes one element: 'ols',
#' which stores the result of the ordinary least squares regression model.
#'
#' @references
#' R Core Team (2021). R: A language and environment for statistical computing. R Foundation for
#' Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
#'
#' @examples
#' sim_data <- LCGA_GMM_sim(n_pers = 100, n_time = 5, beta_int = 0, beta_slo_time = 1,
#' mean_i = 0, var_i = 1, mean_s = 0, var_s = 1, cov_is = 0, mean_r = 0, var_r = 1)
#' model_results <- run_lm_models(sim_data)
#'
#' @export
run_lm_models <- function(sim_data) {

  # List to store model results
  model_outs <- list()

  # Simple linear model
  model_outs$ols <- stats::lm(y ~ time, data = sim_data)

  return(model_outs)
}

#' Run Case-by-case (cbc) Regression Models
#'
#' This function runs case-by-case (cbc) regression models on simulated data
#' using the `cbc_lm` function from the `OLStrajr` package. It includes a built-in
#' error handling mechanism to manage potential errors during model fitting.
#'
#' @param sim_data A data frame of simulated data, created by `LCGA_GMM_sim`.
#' It should include columns for the time point and response variable.
#' @param stop_zeroSD A logical value indicating whether to stop (throw an error) when
#' the estimated standard deviation is zero. Default is FALSE, which means a warning is issued
#' and NULL is returned if the standard deviation is zero.
#' @param level Confidence level for the bootstrap confidence intervals. Default is 0.95.
#'
#' @return A list of model results. The list includes one element: 'ols',
#' which stores the result of the ordinary least squares regression model.
#'
#' @references
#' Carrig, M. M., Wirth, R. J., & Curran, P. J. (2004). A SAS Macro for Estimating and Visualizing
#' Individual Growth Curves. Structural Equation Modeling: A Multidisciplinary Journal, 11(1), 132-149.
#' doi:10.1207/S15328007SEM1101_9
#'
#' @examples
#' sim_data <- LCGA_GMM_sim(n_pers = 100, n_time = 5, beta_int = 0, beta_slo_time = 1,
#' mean_i = 0, var_i = 1, mean_s = 0, var_s = 1, cov_is = 0, mean_r = 0, var_r = 1)
#' model_results <- run_cbc_models(sim_data)
#'
#' @export
run_cbc_models <- function(sim_data, stop_zeroSD = FALSE, level = 0.95){

  # Function to fit a model and handle potential errors
  fit_model <- function(formula, data, stop_zeroSD) {
    tryCatch({
      OLStrajr::cbc_lm(formula = formula, .case = "ID", data = data,
                       stop_zeroSD = stop_zeroSD, boot.ci_options = list(conf = level))
    }, error = function(e) {
      if (stop_zeroSD == FALSE) {
        warning(paste("Could not fit model with formula:", formula, ": ", e$message))
        return(NULL)
      } else {
        stop(e)
      }
    })
  }

  # List to store model results
  model_outs <- list()

  # Simple linear model
  model_outs$ols <- fit_model(formula = y ~ time, data = sim_data,
                                       stop_zeroSD = stop_zeroSD)


  return(model_outs)
}
