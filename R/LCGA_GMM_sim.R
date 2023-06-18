#' Simulate Longitudinal Data for Latent Class Growth Analysis and Growth Mixture Modeling
#'
#' This function generates simulated longitudinal data with random intercept(s) and slope(s)
#' for use in latent class growth analysis and growth mixture modeling studies.
#' It creates a dataset for a specified number of individuals (`n_pers`) and time points (`n_time`),
#' each with a random intercept and slope derived from a multivariate normal distribution.
#' The response variable at each time point for each individual is calculated using a linear model
#' that includes the random intercept and slope, a covariate effect, and a time-covariate interaction effect.
#' Gaussian noise is added to this.
#'
#' @param n_pers Number of individuals in the simulation.
#' @param n_time Number of time points in the simulation.
#' @param beta_int Fixed intercept for the linear model.
#' @param beta_slo_time Fixed effect of time (slope) for the linear model.
#' @param beta_slo_covar Fixed effect of the covariate for the linear model.
#' @param beta_slo_interact Fixed effect of the time-covariate interaction for the linear model.
#' @param mean_i Mean of the random intercepts.
#' @param var_i Variance of the random intercepts.
#' @param mean_s Mean of the random slopes.
#' @param var_s Variance of the random slopes.
#' @param cov_is Covariance between the random intercepts and slopes.
#' @param mean_r Mean of the Gaussian noise added to the response variable.
#' @param var_r Variance of the Gaussian noise added to the response variable.
#'
#' @return A data frame with the simulated longitudinal data.
#' Each row corresponds to one time point for one individual.
#' The data frame includes columns for the individual ID, time point, covariate,
#' random intercept and slope, and response variable.
#'
#' @references
#' This function is based on code copied and pasted from the following work:
#' Wardenaar, K. J. (2022). Latent Class Growth Analysis and Growth Mixture Modeling using R:
#' A tutorial for two R-packages and a comparison with Mplus (Version 5). Unpublished manuscript,
#' Department of Psychiatry, University Medical Center Groningen, Groningen, the Netherlands.
#' Available at: https://osf.io/m58wx/
#'
#' @export
#'
#' @examples
#' sim_data <- LCGA_GMM_sim(n_pers = 100, n_time = 5, beta_int = 0, beta_slo_time = 1,
#' beta_slo_covar = 0, beta_slo_interact = 0, mean_i = 0, var_i = 1, mean_s = 0,
#' var_s = 1, cov_is = 0, mean_r = 0, var_r = 1)
#' head(sim_data)
LCGA_GMM_sim <- function(n_pers, n_time, beta_int, beta_slo_time, beta_slo_covar,
                    beta_slo_interact, mean_i, var_i, mean_s, var_s, cov_is, mean_r, var_r) {

  REff = MASS::mvrnorm(n_pers, mu=c(mean_i,mean_s), Sigma=rbind(c(var_i, cov_is),
                                                          c(cov_is, var_s) ))

  colnames(REff) = c("intercept","slope_time");

  dat= data.frame(ID = rep(1:n_pers, each=n_time),
                  time = rep(1:n_time, times=n_pers),
                  covar= c(rep(0,n_pers/2, each=n_time),rep(1,n_pers/2,
                                                            each=n_time)),
                  int = rep(REff[,1], each=n_time),
                  slo = rep(REff[,2], each=n_time),
                  slo_cov=rep(beta_slo_covar, each=n_time),
                  slo_interact=rep(beta_slo_interact, each=n_time),
                  y = NA)

  dat$time <- dat$time-1
  dat$interact <- dat$time*dat$covar

  y = with(dat, (beta_int + int) + (beta_slo_covar)*covar
           + (beta_slo_time + slo)*time
           + (beta_slo_interact)*interact
           + rnorm(n=n_pers*n_time, mean=mean_r, sd=sqrt(var_r)))
  dat$y <-y

  return(dat) }