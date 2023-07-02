summarize_simulation_results <- function(results) {
  # Initialize a data frame to store the summary statistics
  summary_df <- data.frame()

  # Group results by unique parameter sets
  param_sets <- t(unique(t(sapply(results, function(x) unlist(x$tvs))))) |> as.data.frame()

  for (param_set in seq_along(param_sets)) {
    # Get results for current parameter set
    param_results <- results[sapply(results, function(x) all(unlist(x$tvs) == param_sets[[param_set]]))]

    # Group by model
    model_names <- lapply(names(param_results), function(x) {
      parts <- strsplit(x, "_")[[1]]
      paste0(parts[1:(length(parts) - 2)], collapse = "_")
    })
    model_groups <- unique(unlist(model_names))

    for (model in model_groups) {
      # Get results for current model within the current parameter set
      model_results <- param_results[grep(paste0("^", model, "_"), names(param_results))]

      # Initialize lists to accumulate results
      ierror_list <- list()
      serror_list <- list()
      icover_list <- list()
      scover_list <- list()
      itrue_sd_list <- list()
      strue_sd_list <- list()
      i_se_list <- list()
      s_se_list <- list()
      ipower_list <- list()
      spower_list <- list()
      i_se_error_list <- list()
      s_se_error_list <- list()
      success_list <- list()

      for (i in names(model_results)) {
        # Get the current results
        current_results <- model_results[[i]]

        # Append the current results to the lists
        ierror_list <- c(ierror_list, current_results$ierror)
        serror_list <- c(serror_list, current_results$serror)
        icover_list <- c(icover_list, current_results$icover)
        scover_list <- c(scover_list, current_results$scover)
        itrue_sd_list <- c(itrue_sd_list, current_results$itrue_sd)
        strue_sd_list <- c(strue_sd_list, current_results$strue_sd)
        i_se_list <- c(i_se_list, current_results$i_se)
        s_se_list <- c(s_se_list, current_results$s_se)
        ipower_list <- c(ipower_list, current_results$ipower)
        spower_list <- c(spower_list, current_results$spower)
        i_se_error_list <- c(i_se_error_list, current_results$i_se_error)
        s_se_error_list <- c(s_se_error_list, current_results$s_se_error)
        success_list <- c(success_list, current_results$success)
      }

      # Calculate the summary statistics over all the results
      rmse_i = sqrt(mean(unlist(ierror_list)^2, na.rm = TRUE))
      abs_bias_i = mean(abs(unlist(ierror_list)), na.rm = TRUE)
      cover_95_i = mean(unlist(icover_list), na.rm = TRUE)
      rmse_s = sqrt(mean(unlist(serror_list)^2, na.rm = TRUE))
      abs_bias_s = mean(abs(unlist(serror_list)), na.rm = TRUE)
      cover_95_s = mean(unlist(scover_list), na.rm = TRUE)
      true_sd_i = mean(unlist(itrue_sd_list), na.rm = TRUE)
      true_sd_s = mean(unlist(strue_sd_list), na.rm = TRUE)
      mean_est_se_i = mean(unlist(i_se_list), na.rm = TRUE)
      mean_est_se_s = mean(unlist(s_se_list), na.rm = TRUE)
      se_overunder_i = ifelse(mean_est_se_i > true_sd_i, "overestimate", "underestimate")
      se_overunder_s = ifelse(mean_est_se_s > true_sd_s, "overestimate", "underestimate")
      ipower = mean(unlist(ipower_list), na.rm = TRUE)
      spower = mean(unlist(spower_list), na.rm = TRUE)
      rmse_se_i = sqrt(mean(unlist(i_se_error_list)^2, na.rm = TRUE))
      abs_bias_se_i = mean(abs(unlist(i_se_error_list)), na.rm = TRUE)
      rmse_se_s = sqrt(mean(unlist(s_se_error_list)^2, na.rm = TRUE))
      abs_bias_se_s = mean(abs(unlist(s_se_error_list)), na.rm = TRUE)
      success_count = sum(unlist(success_list), na.rm = TRUE)

      # Extract model and scenario names from the current results name
      model_scenario_name <- strsplit(names(model_results)[1], "_")[[1]]
      model_name <- model
      scenario_name <- paste(model_scenario_name[(length(model_scenario_name) - 1):length(model_scenario_name)], collapse = "_")

      # Create a data frame with the summary statistics for this model
      model_summary_df <- data.frame(
        model = model_name,
        scenario = scenario_name,
        np = model_results[[1]]$tvs$np,
        nt = model_results[[1]]$tvs$nt,
        bint = model_results[[1]]$tvs$bint,
        bst = model_results[[1]]$tvs$bst,
        mi = model_results[[1]]$tvs$mi,
        vi = model_results[[1]]$tvs$vi,
        ms = model_results[[1]]$tvs$ms,
        vs = model_results[[1]]$tvs$vs,
        cis = model_results[[1]]$tvs$cis,
        mr = model_results[[1]]$tvs$mr,
        vr = model_results[[1]]$tvs$vr,
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
