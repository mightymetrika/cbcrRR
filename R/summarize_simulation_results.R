summarize_simulation_results <- function(results) {
  # Initialize a data frame to store the summary statistics
  summary_df <- data.frame()

  # Group results by unique parameter sets
  param_sets <- unique(sapply(results, function(x) unlist(x$tvs)))

  for (param_set in seq_along(param_sets)) {
    # Get results for current parameter set
    param_results <- results[sapply(results, function(x) all(unlist(x$tvs) == param_sets[[param_set]]))]

    # Calculate summary statistics for each model within the current parameter set
    for (i in names(param_results)) {
      # Get the current results
      current_results <- param_results[[i]]

      # Calculate the desired statistics
      rmse_i = sqrt(mean(current_results$ierror^2, na.rm = TRUE))
      abs_bias_i = mean(abs(current_results$ierror), na.rm = TRUE)
      cover_95_i = mean(current_results$icover, na.rm = TRUE)

      rmse_s = sqrt(mean(current_results$serror^2, na.rm = TRUE))
      abs_bias_s = mean(abs(current_results$serror), na.rm = TRUE)
      cover_95_s = mean(current_results$scover, na.rm = TRUE)

      true_sd_i = mean(current_results$itrue_sd, na.rm = TRUE)
      true_sd_s = mean(current_results$strue_sd, na.rm = TRUE)

      mean_est_se_i = mean(current_results$i_se, na.rm = TRUE)
      mean_est_se_s = mean(current_results$s_se, na.rm = TRUE)

      se_overunder_i = ifelse(mean_est_se_i > true_sd_i, "overestimate", "underestimate")
      se_overunder_s = ifelse(mean_est_se_s > true_sd_s, "overestimate", "underestimate")

      ipower = mean(current_results$ipower, na.rm = TRUE)
      spower = mean(current_results$spower, na.rm = TRUE)

      # Calculate the RMSE and absolute bias for standard errors
      rmse_se_i = sqrt(mean(current_results$i_se_error^2, na.rm = TRUE))
      abs_bias_se_i = mean(abs(current_results$i_se_error), na.rm = TRUE)
      rmse_se_s = sqrt(mean(current_results$s_se_error^2, na.rm = TRUE))
      abs_bias_se_s = mean(abs(current_results$s_se_error), na.rm = TRUE)

      # Count the number of successful models
      success_count = sum(current_results$success, na.rm = TRUE)

      # Extract model and scenario names from the current results name
      model_scenario_name <- strsplit(i, "_")[[1]]
      model_name <- paste0(model_scenario_name[1], "_", model_scenario_name[2])
      scenario_name <- paste(model_scenario_name[4:length(model_scenario_name)], collapse = "_")

      # Create a data frame with the summary statistics for this model
      model_summary_df <- data.frame(
        model = model_name,
        scenario = scenario_name,
        np = current_results$tvs$np,
        nt = current_results$tvs$nt,
        bint = current_results$tvs$bint,
        bst = current_results$tvs$bst,
        mi = current_results$tvs$mi,
        vi = current_results$tvs$vi,
        ms = current_results$tvs$ms,
        vs = current_results$tvs$vs,
        cis = current_results$tvs$cis,
        mr = current_results$tvs$mr,
        vr = current_results$tvs$vr,
        rmse_i = rmse_i,
        abs_bias_i = abs_bias_i,
        cover_95_i = cover_95_i,
        rmse_s = rmse_s,
        abs_bias_s = abs_bias_s,
        cover_95_s = cover_95_s,
        true_sd_i = true_sd_i,
        true_sd_s = true_sd_s,
        mean_est_se_i = mean_est_se_i,
        mean_est_se_s = mean_est_se_s,
        se_overunder_i = se_overunder_i,
        se_overunder_s = se_overunder_s,
        ipower = ipower,
        spower = spower,
        rmse_se_i = rmse_se_i,
        abs_bias_se_i = abs_bias_se_i,
        rmse_se_s = rmse_se_s,
        abs_bias_se_s = abs_bias_se_s,
        success_count = success_count
      )

      # Append the summary statistics for this model to the overall summary data frame
      summary_df <- rbind(summary_df, model_summary_df)
    }
  }
  return(summary_df)
}
