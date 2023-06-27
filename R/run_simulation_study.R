run_simulation_study <- function(n_reps, ng = 1, ...) {

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
      lcga_model_info <- extract_lcga_info(model_outs = lcga_model_outs)

      # Fit lme4 models
      lme4_model_outs <- run_lme4_models(sim_data = scenario)

      # Extract lme4 model information
      lme4_model_info <- extract_lme4_info(model_outs = lme4_model_outs)

      # Store true values and model info in list
      scenario_results[[j]] <- list(true_values = scenario,
                                    lcga_model_info = lcga_model_info,
                                    lme4_model_info = lme4_model_info)

    }

    # Store results for this scenario
    results[[i]] <- scenario_results
  }

  return(results)
}

