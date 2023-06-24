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


# extract_cbc_info <- function(model_outs) {
#   # Initialize list to store model information
#   model_info <- list()
#
#   # Loop through each model in the model_outs list
#   for (i in seq_along(model_outs)) {
#
#     test <- lapply(model_outs[[i]]$models, function(x) broom::tidy(x)[["p.value"]])
#
#     mean_pout <- list()
#     for (j in 1:length(test[[1]])){
#       meanp <- lapply(test, function(x)mean(x[[j]]))
#       mean_pout[[j]]
#     }
#     meanp <- sapply(test, mean)
#     # Get the summary of the model
#     model_summary <- model_outs[[i]]$summary_estimates
#     terms <- names(model_summary$mean_coef)
#     coefs <- unlist(model_summary$mean_coef)
#     Ses <- unlist(model_summary$se_coef)
#
#     model_df <- data.frame(term = terms,
#                            coef = coefs,
#                            Se = Ses)
#
#     # Change the term from "(Intercept)" to "intercept"
#     model_df$term <- gsub("\\(Intercept\\)", "intercept", model_df$term)
#
#     # Store the extracted information in the list
#     model_info[[names(model_df)[i]]] <- model_df
#   }
#
#   return(model_info)
# }
