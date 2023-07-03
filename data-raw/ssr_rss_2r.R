## code to prepare `ssr_rss_2r` dataset goes here
ssr_rss_2r <- summarize_simulation_results(results = esr_rss_2r)

usethis::use_data(ssr_rss_2r, overwrite = TRUE)
