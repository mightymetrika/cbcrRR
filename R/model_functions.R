run_lcga_models <- function(sim_data, ng = 1) {

  # List to store model results
  model_outs <- list()

  # Random intercept model
  model_outs$ri <- lcmm::hlme(y ~ time, random = ~ 1 | ID, subject = 'ID',
                                       data = sim_data, ng = ng)

  # Random intercept model w/ co
  model_outs$ri_co <- lcmm::hlme(y ~ time + covar, random = ~ 1 | ID, subject = 'ID',
                                     data = sim_data, ng = ng)


  # Random intercept and slope model
  model_outs$ris <- lcmm::hlme(y ~ time, random = ~ time | ID, subject = 'ID',
                                        data = sim_data, ng = ng)

  # Random intercept and slope model w/ co
  model_outs$ris_co <- lcmm::hlme(y ~ time + covar, random = ~ time | ID, subject = 'ID',
                                      data = sim_data, ng = ng)

  return(model_outs)
}


run_lme4_models <- function(sim_data) {

  # List to store model results
  model_outs <- list()

  # Random intercept model
  model_outs$ri <- lme4::lmer(y ~ time + (1 | ID), data = sim_data)

  # Random intercept model w/ co
  model_outs$ri_co <- lme4::lmer(y ~ time + covar + (1 | ID), data = sim_data)

  # Random intercept and slope model
  model_outs$ris <- lme4::lmer(y ~ time + (time | ID), data = sim_data)

  # Random intercept and slope model w/ co
  model_outs$ris_co <- lme4::lmer(y ~ time + covar + (time | ID), data = sim_data)

  return(model_outs)
}


run_lm_models <- function(sim_data) {

  # List to store model results
  model_outs <- list()

  # Simple linear model
  model_outs$ols <- stats::lm(y ~ time, data = sim_data)

  # Simple linear model w/ co
  model_outs$ols_co <- stats::lm(y ~ time + covar, data = sim_data)

  return(model_outs)
}

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

  # Simple linear model w/ co
  model_outs$ols_co <- fit_model(formula = y ~ time + covar, data = sim_data,
                                     stop_zeroSD = stop_zeroSD)

  return(model_outs)
}
