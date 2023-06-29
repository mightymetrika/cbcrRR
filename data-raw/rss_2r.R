## code to prepare `rss_2r` dataset goes here
set.seed(287)
rss_2r <- run_simulation_study(n_reps = 2)

usethis::use_data(rss_2r, overwrite = TRUE)
