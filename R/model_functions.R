run_lcga_models <- function(sim_data, ng = 1) {

  # List to store model results
  model_outs <- list()

  # Random intercept model w/ no interaction no covar
  model_1 <- lcmm::hlme(y ~ time, random = ~ 1 | ID, subject = 'ID',
                        data = sim_data, ng = ng)
  model_outs$ri_no_intco <- model_1

  # Random intercept model w/ no interaction
  model_2 <- lcmm::hlme(y ~ time + covar, random = ~ 1 | ID, subject = 'ID',
                        data = sim_data, ng = ng)
  model_outs$ri_no_int <- model_2

  # Random intercept model w/ interaction
  model_3 <- lcmm::hlme(y ~ time * covar, random = ~ 1 | ID, subject = 'ID',
                        data = sim_data, ng = ng)
  model_outs$ri_int <- model_3

  # Random intercept and slope model w/ no interaction no covar
  model_4 <- lcmm::hlme(y ~ time, random = ~ time | ID, subject = 'ID',
                        data = sim_data, ng = ng)
  model_outs$ris_no_intco <- model_4

  # Random intercept and slope model w/ no interaction
  model_5 <- lcmm::hlme(y ~ time + covar, random = ~ time | ID, subject = 'ID',
                        data = sim_data, ng = ng)
  model_outs$ris_no_int <- model_5

  # Random intercept and slope model w/ interaction
  model_6 <- lcmm::hlme(y ~ time * covar, random = ~ time | ID, subject = 'ID',
                        data = sim_data, ng = ng)
  model_outs$ris_int <- model_6

  return(model_outs)
}


run_lme4_models <- function(sim_data) {

  # List to store model results
  model_outs <- list()

  # Random intercept model w/ no interaction or covar
  model_1 <- lme4::lmer(y ~ time + (1 | ID), data = sim_data)
  model_outs$ri_no_intco <- model_1

  # Random intercept model w/ no interaction
  model_2 <- lme4::lmer(y ~ time + covar + (1 | ID), data = sim_data)
  model_outs$ri_no_int <- model_2

  # Random intercept model w/ interaction
  model_3 <- lme4::lmer(y ~ time * covar + (1 | ID), data = sim_data)
  model_outs$ri_int <- model_3

  # Random intercept and slope model w/ no interaction or covar
  model_4 <- lme4::lmer(y ~ time + (time | ID), data = sim_data)
  model_outs$ris_no_intco <- model_4

  # Random intercept and slope model w/ no interaction
  model_5 <- lme4::lmer(y ~ time + covar + (time | ID), data = sim_data)
  model_outs$ris_no_int <- model_5

  # Random intercept and slope model w/ interaction
  model_6 <- lme4::lmer(y ~ time * covar + (time | ID), data = sim_data)
  model_outs$ris_int <- model_6

  return(model_outs)
}


run_lm_models <- function(sim_data) {

  # List to store model results
  model_outs <- list()

  # Simple linear model w/ no interaction or covar
  model_1 <- stats::lm(y ~ time, data = sim_data)
  model_outs$ols_no_intco <- model_1


  # Simple linear model w/ no interaction
  model_2 <- stats::lm(y ~ time + covar, data = sim_data)
  model_outs$ols_no_int <- model_2

  # Simple linear model w/ interaction
  model_3 <- stats::lm(y ~ time * covar, data = sim_data)
  model_outs$ols_int <- model_3

  return(model_outs)
}


run_cbc_lm_models <- function(sim_data){

  # List to store model results
  model_outs <- list()

  # Simple linear model w/ no interaction or covar
  model_1 <- OLStrajr::cbc_lm(formula = y ~ time, .case = "ID", data = sim_data)
  model_outs$ols_no_intco <- model_1

  # ### NEED TO FIX TRY CATCH IN OLStrajr BEFORE FINALIZING THIS SECTION ###
  # # Simple linear model w/ no interaction
  # model_2 <- OLStrajr::cbc_lm(formula = y ~ time + covar, .case = "ID", data = sim_data)
  # model_outs$ols_no_int <- model_2
  #
  # # Simple linear model w/ interaction
  # model_3 <- OLStrajr::cbc_lm(formula = y ~ time*covar, .case = "ID", data = sim_data)
  # model_outs$ols_int <- model_3

  return(model_outs)

}
