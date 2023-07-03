sim_plot <- function(df, selected_var,
                     group_vars = c("np", "nt", "bint", "bst", "mi", "vi", "ms",
                                    "vs", "cis", "mr", "vr")) {

  df$model <- stringr::str_replace(df$model, "_model_info", "")

  df_grouped <- df |>
    dplyr::select(c(group_vars, selected_var, "model")) |>
    dplyr::group_by(dplyr::across(tidyr::all_of(group_vars)))

  groups <- dplyr::group_split(df_grouped)

  plots <- vector("list", length(groups))

  for(i in seq_along(groups)){
    # create a caption for each plot
    caption <- paste0(group_vars, ": ", sapply(groups[[i]][1, group_vars], as.character), collapse = ", ")

    plots[[i]] <- ggplot2::ggplot(groups[[i]], ggplot2::aes(x = model, y = !!rlang::sym(selected_var))) +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Model", y = selected_var, caption = caption) +
      ggplot2::theme_minimal()

    # create a name for each plot in a format like c("np: 50", "nt: 3", ...)
    plot_name <- paste0(group_vars, ": ", sapply(groups[[i]][1, group_vars], as.character), collapse = "_")
    plot_name <- gsub(" ", "", plot_name)
    names(plots)[i] <- plot_name
  }

  return(plots)
}
