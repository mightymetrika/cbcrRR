extract_simulation_results <- function(results) {

  # Initialize list to store summary stats
  summary_stats <- list()

  # Counter for successful models
  successful_models <- 0

  # Loop over each simulation repetition
  for (i in seq_along(results)) {

    # Get the results for this repetition
    rep_results <- results[[i]]

    # Loop over each scenario for this repetition
    for (j in seq_along(rep_results)) {

      # Get the results for this scenario
      scenario_results <- rep_results[[j]]

      # Get the true values for this scenario
      true_values <- scenario_results$true_values

      # Loop over each model for this scenario
      for (model_name in names(scenario_results)) {

        # Skip the 'true_values' element
        if (model_name != "true_values") {

          # Get the results for this model
          model_results <- scenario_results[[model_name]]

          # Check if the model ran successfully
          if (!is.character(model_results)) {

            for(mod in names(model_results))

            # Calculate the RMSE
            rmse <- sqrt(mean((model_results[[mod]]$coef[[2]] - true_values$slo)^2))

            # Calculate the absolute bias
            abs_bias <- abs(mean(model_results[[mod]]$coef[[2]] - true_values$slo))   ###Stopped HERE###

            # Calculate the 95% coverage
            coverage <- mean(model_results$conf.low <= true_values & true_values <= model_results$conf.high)

            # Calculate the true standard deviation
            true_sd <- stats::sd(true_values)

            # Calculate the mean estimated standard error
            mean_se <- mean(model_results$Std.Error)

            # Calculate power
            power <- mean(model_results$p.value < 0.05)

            # Increment the counter for successful models
            successful_models <- successful_models + 1

            # Store summary stats for this model
            summary_stats[[paste0(i, "_", j, "_", model_name)]] <-
              list(rmse = rmse, abs_bias = abs_bias, coverage = coverage,
                   true_sd = true_sd, mean_se = mean_se, power = power)

          }

        }

      }

    }

  }

  return(list(summary_stats = summary_stats, successful_models = successful_models))
}
