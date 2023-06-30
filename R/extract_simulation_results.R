extract_simulation_results <- function(results) {

  # Initialize list to store summary stats
  summary_stats <- list()

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
      mod_name <- unique(true_values$mod_name)
      np <- stringr::str_match(mod_name, "np_\\s*(.*?)\\s*_nt_")[[2]] |> as.numeric()
      nt <- stringr::str_match(mod_name, "_nt_\\s*(.*?)\\s*_bint_")[[2]] |> as.numeric()
      bint <- stringr::str_match(mod_name, "_bint_\\s*(.*?)\\s*_bst_")[[2]]|> as.numeric()
      bst <- stringr::str_match(mod_name, "_bst_\\s*(.*?)\\s*_mi_")[[2]] |> as.numeric()
      mi <- stringr::str_match(mod_name, "_mi_\\s*(.*?)\\s*_vi_")[[2]] |> as.numeric()
      vi <- stringr::str_match(mod_name, "_vi_\\s*(.*?)\\s*_ms_")[[2]] |> as.numeric()
      ms <- stringr::str_match(mod_name, "_ms_\\s*(.*?)\\s*_vs_")[[2]] |> as.numeric()
      vs <- stringr::str_match(mod_name, "_vs_\\s*(.*?)\\s*_cis_")[[2]] |> as.numeric()
      cis <- stringr::str_match(mod_name, "_cis_\\s*(.*?)\\s*_mr_")[[2]] |> as.numeric()
      mr <- stringr::str_match(mod_name, "_mr_\\s*(.*?)\\s*_vr_")[[2]] |> as.numeric()
      vr <- sub(".*_vr_", "", mod_name) |> as.numeric()

      tvs <- list(np = np, nt = nt, bint = bint, bst = bst, mi = mi, vi = vi,
                  ms = ms, vs = vs, cis = cis, mr = mr, vr = vr)

      # Loop over each model for this scenario
      for (model_name in names(scenario_results)) {

        # Skip the 'true_values' element
        if (model_name != "true_values") {

          # Get the results for this model
          model_results <- scenario_results[[model_name]]

          for(mod in names(model_results)) {

            # Check if the model ran successfully
            if (!is.character(model_results[[mod]])) {

              # Calculate errors
              ierror <- model_results[[mod]]$coef[[1]] - tvs$bint
              serror <- model_results[[mod]]$coef[[2]] - tvs$bst

              # Calculate the 95% coverage
              icover <- model_results[[mod]]$conf.low[[1]] <= tvs$bint & tvs$bint <= model_results[[mod]]$conf.high[[1]]
              scover <- model_results[[mod]]$conf.low[[2]] <= tvs$bst & tvs$bst <= model_results[[mod]]$conf.high[[2]]

              # Calculate the true standard deviation
              u_true_values <- true_values |> dplyr::distinct(ID, .keep_all = TRUE)
              itrue_sd <- stats::sd(u_true_values$int)
              strue_sd <- stats::sd(u_true_values$slo)

              # Calculate the estimated standard error
              i_se <- model_results[[mod]]$Se[[1]]
              s_se <- model_results[[mod]]$Se[[2]]

              # Calculate the error of standard error
              i_se_error <- i_se - itrue_sd
              s_se_error <- s_se - strue_sd

              # Calculate power
              ipower <- model_results[[mod]]$p.value[[1]] < 0.05
              spower <- model_results[[mod]]$p.value[[2]] < 0.05

              # # Counter for successful models
              success <- 1

              # Store summary stats for this model
              summary_stats[[paste0(model_name, "_", mod, "_", i, "_", j)]] <-
                list(ierror = ierror, serror = serror, icover = icover,
                     scover = scover, itrue_sd = itrue_sd, strue_sd = strue_sd,
                     i_se = i_se, s_se = s_se, i_se_error= i_se_error,
                     s_se_error = s_se_error, ipower = ipower, spower = spower,
                     success = success, tvs = tvs)

            }

          }
        }
      }
    }
  }
  return(summary_stats)
}

