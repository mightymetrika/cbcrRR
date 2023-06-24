extract_lcga_info <- function(model_outs) {
  # Initialize list to store model information
  model_info <- list()

  # Loop through each model in the model_outs list
  for (i in seq_along(model_outs)) {
    # Get the summary of the model
    model_summary <- summary(model_outs[[i]])

    # Convert summary to data.frame and add row names as 'term' column
    model_summary <- data.frame(term = row.names(model_summary), model_summary)

    # Reorder the columns so that 'term' is the first column
    model_summary <- model_summary[, c("term", setdiff(names(model_summary), "term"))]

    # Store the extracted information in the list
    model_info[[names(model_outs)[i]]] <- model_summary
  }

  return(model_info)
}

extract_lme4_info <- function(model_outs) {

  # Initialize list to store model information
  model_info <- list()

  # Loop through each model in the model_outs list
  for (i in seq_along(model_outs)) {

    # Get the summary of the model
    model_summary <- summary(lmerTest::as_lmerModLmerTest(model_outs[[i]]))

    # Extract fixed effects to data.frame and add row names as 'term' column
    model_df <- as.data.frame(model_summary$coefficients)
    model_df$term <- row.names(model_summary$coefficients)

    # Change the term from "(Intercept)" to "intercept"
    model_df$term <- gsub("\\(Intercept\\)", "intercept", model_df$term)

    # Rename columns
    names(model_df)[names(model_df) == "Estimate"] <- "coef"
    names(model_df)[names(model_df) == "Std. Error"] <- "Se"
    names(model_df)[names(model_df) == "Pr(>|t|)"] <- "p.value"

    # Reorder the columns so that 'term' is the first column
    model_df <- model_df[, c("term", setdiff(names(model_df), "term"))]

    # Store the extracted information in the list
    model_info[[names(model_outs)[i]]] <- model_df
  }

  return(model_info)
}

extract_lm_info <- function(model_outs) {
  # Initialize list to store model information
  model_info <- list()

  # Loop through each model in the model_outs list
  for (i in seq_along(model_outs)) {
    # Get the summary of the model
    model_summary <- summary(model_outs[[i]])

    # Extract the necessary info
    model_df <- data.frame(
      term = row.names(model_summary$coefficients),
      coef = model_summary$coefficients[, "Estimate"],
      Se = model_summary$coefficients[, "Std. Error"],
      p.value = model_summary$coefficients[, "Pr(>|t|)"]
    )

    # Change the term from "(Intercept)" to "intercept"
    model_df$term <- gsub("\\(Intercept\\)", "intercept", model_df$term)

    # Store the extracted information in the list
    model_info[[names(model_outs)[i]]] <- model_df
  }

  return(model_info)
}


extract_sand_info <- function(model_outs) {
  # Initialize list to store model information
  model_info <- list()

  # Loop through each model in the model_outs list
  for (i in seq_along(model_outs)) {

    # Get the summary of the model
    model_summary <- as.data.frame(broom::tidy(lmtest::coeftest(model_outs[[i]],
                                                                vcov = sandwich::vcovHC, df = Inf)))

    # Set names
    names(model_summary) <- c("term", "coef", "Se", "statistic", "p.value")

    # Change the term from "(Intercept)" to "intercept"
    model_summary$term <- gsub("\\(Intercept\\)", "intercept", model_summary$term)

    # Store the extracted information in the list
    model_info[[names(model_outs)[i]]] <- model_summary
  }

  return(model_info)
}

extract_cbc_info <- function(model_outs) {
  # Initialize list to store model information
  model_info <- list()

  # Loop through each model in the model_outs list
  for (i in seq_along(model_outs)) {

    # Check if the model contains summary estimates
    if (length(model_outs[[i]][["summary_estimates"]]$mean_coef) != 0){

      # Get the summary of the model
      model_summary <- model_outs[[i]]$summary_estimates

      # Get mean p-value across models
      p_values <- lapply(model_outs[[i]]$models, function(x) broom::tidy(x)[["p.value"]])
      median_p_values <- sapply(1:length(p_values[[1]]), function(j) stats::median(sapply(p_values, `[[`, j)))

      # Construct a data frame with the summary information
      model_df <- data.frame(
        term = names(model_summary$mean_coef),
        coef = unlist(model_summary$mean_coef),
        Se = unlist(model_summary$se_coef),
        p.value = unlist(median_p_values)
      )

      # Change the term from "(Intercept)" to "intercept"
      model_df$term <- gsub("\\(Intercept\\)", "intercept", model_df$term)
    } else {
      # If no summary estimates, create an empty data frame
      model_df <- data.frame()
    }

    # Store the extracted information in the list
    model_info[[names(model_outs)[i]]] <- model_df
  }

  return(model_info)
}

extract_cbcRR_info <- function(model_outs) {
  # Initialize list to store model information
  model_info <- list()

  # Loop through each model in the model_outs list
  for (i in seq_along(model_outs)) {

    # Check if the model contains summary estimates
    if (length(model_outs[[i]][["summary_estimates"]]$mean_coef) != 0){
      # Get all individual models
      indiv_models <- lapply(model_outs[[i]]$models, broom::tidy)

      # Calculate Rubin's Rules variables
      m <- length(indiv_models)
      ests <- do.call(cbind, lapply(indiv_models, function(x) x$estimate))
      ses <- do.call(cbind, lapply(indiv_models, function(x) x$std.error))
      wi.var <- rowMeans(ses ^ 2)
      diffs <- sweep(ests, 1, rowMeans(ests))
      bw.var <- rowSums(diffs ^ 2) / (m - 1)

      # Calculate Rubin's Rules combined estimates and standard errors
      terms <- indiv_models[[1]]$term
      coefs <- rowMeans(ests)
      Ses <- sqrt(wi.var + bw.var * (1 + 1 / m))

      # Compute t-statistics and calculate p-values
      r <- ((1 + 1 / m) * bw.var) / wi.var
      df <- (m - 1) * (1 + 1 / r) ^ 2
      t_stat <- coefs / Ses
      pvalues <- 2 * stats::pt(-abs(t_stat), df = df)

      # Create a data frame with the results
      model_df <- data.frame(term = terms, coef = coefs, Se = Ses, p.value = pvalues)

      # Change the term from "(Intercept)" to "intercept"
      model_df$term <- gsub("\\(Intercept\\)", "intercept", model_df$term)

    } else {
      # If no summary estimates, create an empty data frame
      model_df <- data.frame()
    }

    # Store the extracted information in the list
    model_info[[names(model_outs)[i]]] <- model_df
  }

  return(model_info)
}
