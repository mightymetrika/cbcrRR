run_simulation_study <- function(n_reps, ng = 1, level = 0.95, ...) {

  # Initialize list to store results
  results <- vector("list", n_reps)

  # Loop over repetitions
  for (i in seq_len(n_reps)) {
    # Get all scenarios
    scenarios <- scenario_crossing(...)

    # Initialize list to store the results for this scenario
    scenario_results <- vector("list", length = length(scenarios))

    # Loop over scenarios
    for (j in seq_along(scenarios)) {

      # Get the current scenario
      scenario <- scenarios[[j]]

      # Fit lcga models
      lcga_model_outs <- run_lcga_models(sim_data = scenario, ng = ng)

      # Extract lcga model information
      lcga_model_info <- extract_lcga_info(model_outs = lcga_model_outs,
                                           level = level)

      # Fit lme4 models
      lme4_model_outs <- run_lme4_models(sim_data = scenario)

      # Extract lme4 model information
      lme4_model_info <- extract_lme4_info(model_outs = lme4_model_outs,
                                           level = level)

      # Fit lm models
      lm_model_outs <- run_lm_models(sim_data = scenario)

      # Extract lm model information
      lm_model_info <-extract_lm_info(model_outs = lm_model_outs, level = level)

      # Extract sand model information
      sand_model_info <- extract_sand_info(model_outs = lm_model_outs,
                                           level = level)

      # Fit cbc models
      cbc_model_outs <- run_cbc_models(sim_data = scenario, level = level)

      # Extract cbc model information
      cbc_model_info <- extract_cbc_info(model_outs = cbc_model_outs,
                                         level = level)

      # Extract cbcRR model information
      cbcRR_model_info <- extract_cbcRR_info(model_outs = cbc_model_outs,
                                             level = level)

      # Store true values and model info in list
      scenario_results[[j]] <- list(true_values = scenario,
                                    lcga_model_info = lcga_model_info,
                                    lme4_model_info = lme4_model_info,
                                    lm_model_info = lm_model_info,
                                    sand_model_info = sand_model_info,
                                    cbc_model_info = cbc_model_info,
                                    cbcRR_model_info = cbcRR_model_info)

    }

    # Store results for this scenario
    results[[i]] <- scenario_results
  }

  return(results)
}

