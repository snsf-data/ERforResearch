% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jags_functions.R
\name{get_mcmc_samples}
\alias{get_mcmc_samples}
\title{MCMC samples}
\usage{
get_mcmc_samples(
  data,
  id_proposal,
  id_assessor,
  grade_variable,
  path_to_jags_model = NULL,
  n_chains = 4,
  n_iter = 10000,
  n_burnin = 4000,
  n_adapt = 1000,
  id_panel = NULL,
  max_iter = 1e+06,
  theta_name = "proposal_intercept",
  tau_name_proposal = "tau_proposal",
  sigma_name = "sigma",
  tau_name_assessor = "tau_assessor",
  tau_name_panel = "tau_panel",
  rank_theta_name = "rank_theta",
  assessor_name = "assessor_intercept",
  assessor_behavior_name = "nu",
  ordinal_scale = FALSE,
  point_scale = NULL,
  heterogeneous_residuals = FALSE,
  seed = 1991,
  quiet = FALSE,
  dont_bind = FALSE,
  inits_type = "random",
  names_variables_to_sample = NULL,
  initial_values = NULL,
  rhat_threshold = 1.01,
  runjags_method = "parallel",
  minimal_testing = FALSE
)
}
\arguments{
\item{data}{data frame, in long format, with all needed variables as
specified in the JAGS model defined in the text file with path given in
`path_to_jags_model`.}

\item{id_proposal}{the name of the variable in `data` that indicates the ID
of the evaluated proposal.}

\item{id_assessor}{the name of the variable in `data` that indicates the ID
of the assessor. The default `= NULL`, for the case where each assessor only
evaluates/grades one proposal.}

\item{grade_variable}{the name of the variable in `data` with the outcome
variable, i.e. the grade or score.}

\item{path_to_jags_model}{the path to text file including the JAGS model
definition. By default `= NULL`, and the function will use a default model as
implemented in the package; in `get_default_jags_model()`.}

\item{n_chains}{the number of chains for the JAGS sampler. The default number
of chains is set to four. This creates optimal conditions and should not be
changed. The same parameter in `runjags::run.jags()` is called `n.chains`.}

\item{n_iter}{how many iterations should be used in the JAGS sampler? This is
the same as `sample` in the `runjags::run.jags()` function. It is set to
`10000` by default.}

\item{n_burnin}{the number of burnin iterations which will not be included in
the adaptation phase. By default it is set to `4000` and the same parameter
in `runjags::run.jags()` is called `burnin`.}

\item{n_adapt}{the number of adaptive iterations discarded for the adaptation
phase. By default it is set to `1000`. The same parameter in
`runjags::run.jags()` is called `adapt`.}

\item{id_panel}{the name of the variable in `data` that indicates the ID
of the panel. The default `= NULL`, for the case where all proposals were
evaluated in the same panel, or were each panel creates its own ranking.
In the other scenario, a ranking would be established combining or merging
all panels.}

\item{max_iter}{the maximum number of iteration. The JAGS sample will be
extended until convergence of the chains. To ensure that the sampler does not
run and extend forever a maximum number of iterations per chain can be
defined. Once this number of iterations is achieved, the sampler will not be
further extended. By default, the function allows up to `1000000` iterations
before stopping.}

\item{theta_name}{the name of the proposal intercept in the JAGS model.
The default that also goes with the default JAGS model build in the package
is `proposal_intercept`.}

\item{tau_name_proposal}{the name of tau in the JAGS model, being the
standard error of the proposal effects. The default that also goes with the
default JAGS model build in the package is `tau_proposal`.}

\item{sigma_name}{name of the standard deviation of the full model. The
default that also goes with the default JAGS model build in the package is
`sigma`.}

\item{tau_name_assessor}{name of the standard error of the assessor effect in
the JAGS model.  The default that also goes with the default JAGS model build
in the package is `tau_assessor`.}

\item{tau_name_panel}{the name of the standard error of the panel effect, if
needed. The default that also goes with the default JAGS model build in the
package is `tau_panel`. This is only needed if a ranking has to be
established combining or merging all panels, and therefore only important if
`id_panel` is not `NULL`.}

\item{rank_theta_name}{the name of the rank of theta in the JAGS model. The
default that also goes with the default JAGS model build in the package is
`rank_theta`.}

\item{assessor_name}{the name of the assessor intercept in the JAGS model.
The default that also goes with the default JAGS model build in the package
is `assessor_intercept`.}

\item{assessor_behavior_name}{the name of the parameter in the JAGS model
indicating the voter behavior. By default it is set to `nu` as this is the
name in the default JAGS model.}

\item{ordinal_scale}{dummy variable informing us on whether or not the
outcome is on an ordinal scale. By default, we assume a numeric scale and
this parameter is set to `FALSE`.}

\item{point_scale}{integer informing us on the number of points of the
ordinal scale; not needed for continuous scale. By default, we assume a
numeric scale and do not need this information and the  parameter is set to
`NULL`.}

\item{heterogeneous_residuals}{dummy variable informing us on whether or not
the residuals should be heterogeneous. By default the residuals are assumed
homogeneous and this parameter is set to `FALSE`.}

\item{seed}{the seed for the JAGS model (default = `1991`). This seed will
generate the seeds for the JAGS samplers, which ensures reproducibility; see
also Details.}

\item{quiet}{if the default model is used this function generates a warning.
if `quiet = TRUE`, the warning is not shown.}

\item{dont_bind}{setting this parameter to `TRUE` will pool all the chains
together before returning the MCMC. By default it is however set to `FALSE`.}

\item{inits_type}{the type of the initial values. By default the initial
values are randomly selected, i.e. `inits_type = "random"`.
Alternatively, if four chains are used, the initial values can also be
`"overdispersed"`.}

\item{names_variables_to_sample}{the variables to sample can be specified,
writin their names here, as a character-vector. The default is `NULL` and
the default variables are used.}

\item{initial_values}{The list of initial values for the jags sampler can be
provided directly. Otherwise `get_inits_overdispersed_four_chains` for the
overdispersed version is used, or they are randomly selected. Always using a
seed to ensure computational reproducibility.}

\item{rhat_threshold}{the threshold for rhat to decide whether or not the
chains converged. Gelman suggested 1.1, but the smaller the better. Hence
this functions threshold is set to `1.01` by default.}

\item{runjags_method}{the method with which to call JAGS (from
`runjags::run.jags()` with the default being set to `parallel`).}

\item{minimal_testing}{should only the most important model parameters be
tested for convergence, e.g. the ranks and the variances? By default this is
set to `FALSE`.}
}
\value{
The function returns a list with: the MCMC samples in `samples` for
all parameters defined in the model, more information on the number of
chains, and the length of the adaptation, iteration and burnin phase, and
an indicator on whether or not the chains converged (according to all Rhats
being smaller than `rhat_threshold`). Additionally a summary report matrix
from `runjags::run.jags` is included (in `summary`) for each sampled
parameter with, among others, the effective sample size, MCerror, and rhat
values.
}
\description{
Helper function to get the mcmc samples. It is essentially a function wrapped
around the function `runjags::run.jags()`. The function also tests
convergence of the chains and extends them until convergence is achieved.
Find more in Details.
}
\details{
A note on convergence:

#' Note that a convergence test is applied in this function: If with
the specified length of adaptation and burnin phase together with the number
of specified iterations not all Rhat values are below `rhat_threshold`, the
latter Ns (n_adapt, n_burnin, and n_iter) are multiplied by multiplied by 5,
10, 5x10, 10x10, 5x10^2, 10x10^2, etc, until either all Rhat values are
below `rhat_threshold`or the number of iteration would exceed max_iter. If
the Rhat values are still not all small enough a warning message is printed.

A note on reproducibility:

To ensure reproducibility of the chains, all parameters need explicit
starting values and each chain needs an explicitly selected random samplers
(`.RNG.name`) and an explicitly selected seed (`.RNG.seed`). For four chains
the samplers are chosen among `base::Wichmann-Hill`,
`base::Marsaglia-Multicarry`, `base::Super-Duper`, `base::Mersenne-Twister`.
For more, or less than four chains a sample (with or without replacement) of
these samplers is selected.
}
\examples{
data_panel1 <- get_mock_data() \%>\%
     filter(panel == "p1")
\dontrun{
mcmc_samples <- get_mcmc_samples(data = data_panel1,
                                 id_proposal = "proposal",
                                 id_assessor = "assessor",
                                 grade_variable = "num_grade")
                                 }
}
