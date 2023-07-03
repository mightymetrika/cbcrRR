## code to prepare `esr_rss_2r` dataset goes here
esr_rss_2r <- extract_simulation_results(results = rss_2r)

usethis::use_data(esr_rss_2r, overwrite = TRUE)
