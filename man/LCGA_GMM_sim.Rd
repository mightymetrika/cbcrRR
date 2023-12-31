% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LCGA_GMM_sim.R
\name{LCGA_GMM_sim}
\alias{LCGA_GMM_sim}
\title{Simulate Longitudinal Data for Latent Class Growth Analysis and Growth Mixture Modeling}
\usage{
LCGA_GMM_sim(
  n_pers,
  n_time,
  beta_int,
  beta_slo_time,
  mean_i,
  var_i,
  mean_s,
  var_s,
  cov_is,
  mean_r,
  var_r,
  mod_name = NA_character_
)
}
\arguments{
\item{n_pers}{Number of individuals in the simulation.}

\item{n_time}{Number of time points in the simulation.}

\item{beta_int}{Fixed intercept for the linear model.}

\item{beta_slo_time}{Fixed effect of time (slope) for the linear model.}

\item{mean_i}{Mean of the random intercepts.}

\item{var_i}{Variance of the random intercepts.}

\item{mean_s}{Mean of the random slopes.}

\item{var_s}{Variance of the random slopes.}

\item{cov_is}{Covariance between the random intercepts and slopes.}

\item{mean_r}{Mean of the Gaussian noise added to the response variable.}

\item{var_r}{Variance of the Gaussian noise added to the response variable.}

\item{mod_name}{A model identifier}
}
\value{
A data frame with the simulated longitudinal data.
Each row corresponds to one time point for one individual.
The data frame includes columns for the individual ID, time point, covariate,
random intercept and slope, and response variable.
}
\description{
This function generates simulated longitudinal data with random intercept(s) and slope(s)
for use in latent class growth analysis and growth mixture modeling studies.
It creates a dataset for a specified number of individuals (\code{n_pers}) and time points (\code{n_time}),
each with a random intercept and slope derived from a multivariate normal distribution.
The response variable at each time point for each individual is calculated using a linear model
that includes the random intercept and slope.
}
\examples{
sim_data <- LCGA_GMM_sim(n_pers = 100, n_time = 5, beta_int = 0, beta_slo_time = 1,
mean_i = 0, var_i = 1, mean_s = 0, var_s = 1, cov_is = 0, mean_r = 0, var_r = 1)
head(sim_data)
}
\references{
This function is based on code copied and pasted from the following work:
Wardenaar, K. J. (2022). Latent Class Growth Analysis and Growth Mixture Modeling using R:
A tutorial for two R-packages and a comparison with Mplus (Version 5). Unpublished manuscript,
Department of Psychiatry, University Medical Center Groningen, Groningen, the Netherlands.
Available at: https://osf.io/m58wx/
}
