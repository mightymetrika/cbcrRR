run_simulation_study <- function(n_reps, ...) {

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

      # Fit models
      model_outs <- run_lme4_models(sim_data = scenario)

      # Extract information
      model_info <- extract_lme4_info(model_outs = model_outs)

      # Store true values and model info in list
      scenario_results[[j]] <- list(true_values = scenario, model_info = model_info)

    }

    # Store results for this scenario
    results[[i]] <- scenario_results
  }

  return(results)
}
