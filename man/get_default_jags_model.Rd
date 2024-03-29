% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jags_functions.R
\name{get_default_jags_model}
\alias{get_default_jags_model}
\title{Write a default jags model}
\usage{
get_default_jags_model(
  outcome_variable = "continuous",
  nine_point_scale_continuous = FALSE,
  residuals = "homogeneous",
  subpanels = FALSE,
  path = "default_jags_model.txt",
  quiet = FALSE
)
}
\arguments{
\item{outcome_variable}{the nature of the outcome variable: continuous or
ordinal.  By default the grades are assumed to be `"continuous"`.}

\item{nine_point_scale_continuous}{boolean variable indicated whether or not
a nine-point continuous scale is used. By default is is set to `FALSE`, which
means that a six-point scale is used by default. Be aware that those
specifications are due to the usage of the package by the Swiss National
Science Foundation.}

\item{residuals}{the nature of the residuals in the model: homogeneous or
heterogeneous. By default the residuals are assumed to be `"homogeneous"`.}

\item{subpanels}{the individual votes come from different sub-panels that
need to be merged. By default this is set to `FALSE` meaning that rankings
are produced for each panel separately.}

\item{path}{where should it be stored, relative to getwd()? By default this
is set to `"jags_model.txt"`. Hence it is simply written to the working
directory.}

\item{quiet}{if set to `TRUE`, the function does not show any messages of
caution.}
}
\description{
This function writes a default JAGS model, given the parameters and stores it
as a. txt in a certain path.
}
\details{
Note that many of the specific default parameters are specified for easy
usage and integration in the funding evaluation system at the Swiss National
Science Foundation and might not be applicable to other evaluation systems.

The model defined here has a random component for the proposal and
the assessor. If the parameter subpanels is set to TRUE an additional
grouping for the panel is defined. The user is invited to write their own
model definition if more flexibility is needed.

The path to a user-specified model can then be given as parameter to the
`get_mcmc_samples()` or the `get_er_from_jags()` functions. The user can also
simply decide between a continuous or ordinal outcome variable, homogeneous
or heterogeneous residuals, with or without subpanel merging (those options
are all integrated), write the model in a .txt file and adapt it from there.
}
\examples{
# The model definition .txt is stored in the file "default_jags_model.txt"
\dontrun{
get_default_jags_model()
}
}
