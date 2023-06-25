extract_lcga_info <- function(model_outs, level = 0.95) {
  # Initialize list to store model information
  model_info <- list()

  # Loop through each model in the model_outs list
  for (i in seq_along(model_outs)) {
    # Get the summary of the model
    model_summary <- summary(model_outs[[i]])

    # Calculate confidence intervals
    model_ci <- stats::confint(model_outs[[i]], level = level)
    model_ci <- stats::setNames(as.data.frame(model_ci), c("conf.low", "conf.high"))
    row.names(model_ci) <- make.names(row.names(model_ci), unique = TRUE)
    model_ci$term <- row.names(model_ci)

    # Convert summary to data.frame and add row names as 'term' column
    model_summary <- data.frame(term = make.names(row.names(model_summary), unique = TRUE), model_summary)

    # Merge model_summary and model_ci
    model_summary <- merge(model_summary, model_ci, by = "term")

    # Reorder the columns so that 'term' is the first column
    model_summary <- model_summary[, c("term", setdiff(names(model_summary), "term"))]

    # Store the extracted information in the list
    model_info[[names(model_outs)[i]]] <- model_summary
  }

  return(model_info)
}

extract_lme4_info <- function(model_outs, level = 0.95) {

  # Initialize list to store model information
  model_info <- list()

  # Loop through each model in the model_outs list
  for (i in seq_along(model_outs)) {

    # Get the summary of the model
    model_summary <- summary(lmerTest::as_lmerModLmerTest(model_outs[[i]]))

    # Extract fixed effects to data.frame and add row names as 'term' column
    model_df <- as.data.frame(model_summary$coefficients)
    model_df$term <- row.names(model_summary$coefficients)

    # Calculate confidence intervals
    model_ci <- stats::confint(model_outs[[i]], level = level)
    model_ci <- stats::setNames(as.data.frame(model_ci), c("conf.low", "conf.high"))
    model_ci$term <- row.names(model_ci)

    # Change the term from "(Intercept)" to "intercept"
    model_df$term <- gsub("\\(Intercept\\)", "intercept", model_df$term)
    model_ci$term <- gsub("\\(Intercept\\)", "intercept", model_ci$term)

    # Rename columns
    names(model_df)[names(model_df) == "Estimate"] <- "coef"
    names(model_df)[names(model_df) == "Std. Error"] <- "Se"
    names(model_df)[names(model_df) == "Pr(>|t|)"] <- "p.value"

    # Merge model_df and model_ci
    model_df <- merge(model_df, model_ci, by = "term")

    # Reorder the columns so that 'term' is the first column
    model_df <- model_df[, c("term", setdiff(names(model_df), "term"))]

    # Store the extracted information in the list
    model_info[[names(model_outs)[i]]] <- model_df
  }

  return(model_info)
}

extract_lm_info <- function(model_outs, level = 0.95) {
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

    # Calculate confidence intervals
    model_ci <- stats::confint(model_outs[[i]], level = level)
    model_ci <- stats::setNames(as.data.frame(model_ci), c("conf.low", "conf.high"))
    model_ci$term <- row.names(model_ci)

    # Change the term from "(Intercept)" to "intercept"
    model_df$term <- gsub("\\(Intercept\\)", "intercept", model_df$term)
    model_ci$term <- gsub("\\(Intercept\\)", "intercept", model_ci$term)

    # Merge model_df and model_ci
    model_df <- merge(model_df, model_ci, by = "term")

    # Reorder the columns so that 'term' is the first column
    model_df <- model_df[, c("term", setdiff(names(model_df), "term"))]

    # Store the extracted information in the list
    model_info[[names(model_outs)[i]]] <- model_df
  }

  return(model_info)
}

extract_sand_info <- function(model_outs, level = 0.95) {
  # Initialize list to store model information
  model_info <- list()

  # Loop through each model in the model_outs list
  for (i in seq_along(model_outs)) {

    # Get the summary of the model
    model_summary <- as.data.frame(broom::tidy(lmtest::coeftest(model_outs[[i]],
                                                                vcov = sandwich::vcovHC, df = Inf),
                                               conf.int = TRUE, conf.level = level))

    # Set names
    names(model_summary) <- c("term", "coef", "Se", "statistic", "p.value",
                              "conf.low", "conf.high")

    # Change the term from "(Intercept)" to "intercept"
    model_summary$term <- gsub("\\(Intercept\\)", "intercept", model_summary$term)

    # Store the extracted information in the list
    model_info[[names(model_outs)[i]]] <- model_summary
  }

  return(model_info)
}

extract_cbc_info <- function(model_outs, level = 0.95) {
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

      # Extract the confidence intervals
      conf_ints <- lapply(model_summary$ci_coef, function(x) x)

      # Construct a data frame with the summary information
      model_df <- data.frame(
        term = names(model_summary$mean_coef),
        coef = unlist(model_summary$mean_coef),
        Se = unlist(model_summary$se_coef),
        p.value = unlist(median_p_values),
        conf.low = sapply(conf_ints, function(x) x[1]),
        conf.high = sapply(conf_ints, function(x) x[2])
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

extract_cbcRR_info <- function(model_outs, level = 0.95) {  # add level here
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

      # Compute confidence intervals
      t.c <- stats::qt(1 - (1 - level) / 2, df = df, lower.tail = TRUE)  # corrected issue here
      conf.low <- coefs - t.c * Ses
      conf.high <- coefs + t.c * Ses

      # Create a data frame with the results
      model_df <- data.frame(term = terms, coef = coefs, Se = Ses, p.value = pvalues,
                             conf.low = conf.low, conf.high = conf.high)  # added conf.low and conf.high here

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
