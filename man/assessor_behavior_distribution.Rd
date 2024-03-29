% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting_functions.R
\name{assessor_behavior_distribution}
\alias{assessor_behavior_distribution}
\title{Assessor behavior distributions}
\usage{
assessor_behavior_distribution(
  get_mcmc_samples_result,
  n_assessors,
  name_mean = "nu",
  names_assessors = "voter",
  title = NULL,
  xlim_min = -1,
  xlim_max = 1,
  scale = 1.75
)
}
\arguments{
\item{get_mcmc_samples_result}{the mcmc samples}

\item{n_assessors}{number of voters in the panel / call}

\item{name_mean}{the name of the parameter estimating the average voter
behavior (default = "nu").}

\item{names_assessors}{names of the voters to be written on the y-axis
ticks (default = "voter").}

\item{title}{title of the plot (default = NULL, no title).}

\item{xlim_min}{minimum of the x-axis (default = -1).}

\item{xlim_max}{maximum of the x-axis (default = 1).}

\item{scale}{for the `geom_density_ridges_gradient()` (default = 1.75).}
}
\value{
the result is a plot of the posterior distributions of the average
voter behaviors.
}
\description{
This function plots the distibutions of the average voter behavior
(the $mu_j$'s in the model). Make sure that when using the get_mcmc_samples()
function, the parameter for the average voter behavior (default nu) is
sampled from the JAGS model.
}
