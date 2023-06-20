run_lcga_models <- function(sim_data, ng = 1) {

  # List to store model results
  model_outs <- list()

  # Random intercept model w/ no interaction
  model_1 <- lcmm::hlme(y ~ time + covar, random = ~ 1 | ID, subject = 'ID',
                        data = sim_data, ng = ng)
  model_outs$ri_no_int <- model_1

  # Random intercept model w/ interaction
  model_2 <- lcmm::hlme(y ~ time * covar, random = ~ 1 | ID, subject = 'ID',
                        data = sim_data, ng = ng)
  model_outs$ri_int <- model_2

  # Random intercept and slope model w/ no interaction
  model_3 <- lcmm::hlme(y ~ time + covar, random = ~ time | ID, subject = 'ID',
                        data = sim_data, ng = ng)
  model_outs$ris_no_int <- model_3

  # Random intercept and slope model w/ interaction
  model_4 <- lcmm::hlme(y ~ time * covar, random = ~ time | ID, subject = 'ID',
                        data = sim_data, ng = ng)
  model_outs$ris_int <- model_4

  return(model_outs)
}


run_lme4_models <- function(sim_data) {

  # List to store model results
  model_outs <- list()

  # Random intercept model w/ no interaction
  model_1 <- lme4::lmer(y ~ time + covar + (1 | ID), data = sim_data)
  model_outs$ri_no_int <- model_1

  # Random intercept model w/ interaction
  model_2 <- lme4::lmer(y ~ time * covar + (1 | ID), data = sim_data)
  model_outs$ri_int <- model_2

  # Random intercept and slope model w/ no interaction
  model_3 <- lme4::lmer(y ~ time + covar + (time | ID), data = sim_data)
  model_outs$ris_no_int <- model_3

  # Random intercept and slope model w/ interaction
  model_4 <- lme4::lmer(y ~ time * covar + (time | ID), data = sim_data)
  model_outs$ris_int <- model_4

  return(model_outs)
}
