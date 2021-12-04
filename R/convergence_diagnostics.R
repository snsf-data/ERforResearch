#' Helper function to define overdispersed initial values
#'
#'(note that this function can only be applied when two chains are used,
#' otherwise the user has to provide the initial values)
#' @param merging_sections should the sections be merged?, default is FALSE,
#' e.g. a ranking is done for each section/panel separately.
#' @param ordinal_scale are the evaluation scores on an ordinal scale?,
#' default is FALSE.
#' @param point_scale if on an ordinal scale, what is the number of points on
#' the scale?, default is NULL
#' @param n_applications number of applications / proposals considered.
#' @param n_voters number of voters / panel members.
#' @param n_sections number of sections / panels, if merged, default is NULL.
#' @param grades the grades given to the proposals (the whole
#' n_applications*n_voters long vector)
#' @param seed seed used for the sampling
#'
#' @return a list of overdispersed initial values for the two chains
#' @export
get_inits_overdispersed_two_chains <- function(merging_sections = FALSE,
                                               ordinal_scale = FALSE,
                                               point_scale = NULL,
                                               n_applications,
                                               n_voters,
                                               n_sections = NULL,
                                               grades = NULL,
                                               seed){

  # If the results of the panels are NOT merged and the scores are on a numeric scale:
  if(!merging_sections & !(ordinal_scale)) {
    inits_chain1 <- # Inits for first chain
      list(application_intercept =
             rep.int(-4, times = n_applications),
           voter_intercept = matrix(rep(-4, times = n_voters *
                                          n_applications),
                                    ncol = n_voters,
                                    nrow = n_applications),
           nu = rep(-2, n_voters),
           sigma = 10^{-4},
           tau_application =  10^{-4},
           tau_voter = 10^{-6},
           # need this part to get reproducible results
           .RNG.name = "base::Wichmann-Hill",
           .RNG.seed = seed)
    inits_chain2 <- # Inits for second chain
      list(application_intercept =
             rep.int(4, times = n_applications),
           voter_intercept = matrix(rep(4, times = n_voters *
                                          n_applications),
                                    ncol = n_voters,
                                    nrow = n_applications),
           nu = rep(2, n_voters),
           sigma = 1,
           tau_application = 1.5,
           tau_voter = 1,
           # need this part to get reproducible results
           .RNG.name = "base::Wichmann-Hill",
           .RNG.seed = seed)
    inits <- list(inits_chain1, inits_chain2)
  }

  # If initial values should be overdispersed, the results of the panels are
  # to be merged and the scores are on a numeric scale:
  if (merging_sections & !(ordinal_scale)) {
    inits_chain1 <- # Inits for first chain
      list(application_intercept =
             rep.int(-4, times = n_applications),
           voter_intercept = matrix(rep(-4, times = n_voters *
                                          n_applications),
                                    ncol = n_voters,
                                    nrow = n_applications),
           nu = rep(-2, n_voters),
           sigma = 10^{-4},
           tau_application =  10^{-4},
           tau_voter = 10^{-6},
           tau_section = 10^{-6},
           section_intercept =
             rep.int(-5, times = n_sections),
           # need this part to get reproducible results
           .RNG.name = "base::Wichmann-Hill",
           .RNG.seed = seed)
    inits_chain2 <- # Inits for second chain
      list(application_intercept =
             rep.int(4, times = n_applications),
           voter_intercept = matrix(rep(4, times = n_voters *
                                          n_applications),
                                    ncol = n_voters,
                                    nrow = n_applications),
           nu = rep(2, n_voters),
           sigma = 1,
           tau_application = 1.5,
           tau_voter = 1,
           tau_section = 1,
           section_intercept = rep.int(5, times = n_sections),
           # need this part to get reproducible results
           .RNG.name = "base::Wichmann-Hill",
           .RNG.seed = seed)
    inits <- list(inits_chain1, inits_chain2)
  }

  # If initial values should be overdispersed, the results of the panels are
  # to be merged and the scores are on a numeric scale:
  if (!merging_sections & ordinal_scale) {
    inits_chain1 <- # Inits for first chain
      list(application_intercept =
             rep.int(-4, times = n_applications),
           voter_intercept =
             matrix(rep(-4, times = n_voters *
                          n_applications),
                    ncol = n_voters,
                    nrow = n_applications),
           nu = rep(-2, n_voters),
           sigma = 10^{-4},
           tau_application = 10^{-4},
           tau_voter = 10^{-6},
           cc = seq(.5, point_scale - 1, 1),
           # need this part to get reproducible results
           .RNG.name = "base::Wichmann-Hill",
           .RNG.seed = seed,
           latent_trait = grades)
    inits_chain2 <- # Inits for second chain
      list(application_intercept =
             rep.int(4, times = n_applications),
           voter_intercept = matrix(rep(4, times = n_voters * n_applications),
                                    ncol = n_voters, nrow = n_applications),
           nu = rep(2, n_voters),
           sigma = 1,
           tau_application = 1.5,
           tau_voter = 1,
           cc = seq(.5, point_scale - 1, 1),
           # need this part to get reproducible results
           .RNG.name = "base::Wichmann-Hill",
           .RNG.seed = seed,
           latent_trait = grades)
    inits <- list(inits_chain1, inits_chain2)
  }
  # If initial values should be overdispersed, the results of the panels are
  # to be merged and the scores are on an ordinal scale:
  if (merging_sections & ordinal_scale) {
    inits_chain1 <- # Inits for first chain
      list(application_intercept =
             rep.int(-4, times = n_applications),
           voter_intercept = matrix(rep(-4, times = n_voters *
                                          n_applications),
                                    ncol = n_voters,
                                    nrow = n_applications),
           nu = rep(-2, n_voters),
           sigma = 10^{-4},
           tau_application = 10^{-4},
           tau_voter = 10^{-6},
           tau_section = 10^{-6},
           section_intercept = rep.int(-5, times = n_sections),
           cc = seq(.5, point_scale - 1, 1),
           # need this part to get reproducible results
           .RNG.name = "base::Wichmann-Hill",
           .RNG.seed = seed,
           latent_trait = grades)
    inits_chain2 <- # Inits for second chain
      list(application_intercept =
             rep.int(4, times = n_applications),
           voter_intercept = matrix(rep(4, times = n_voters *
                                          n_applications),
                                    ncol = n_voters,
                                    nrow = n_applications),
           nu = rep(2, n_voters),
           sigma = 1,
           tau_application = 1.5,
           tau_voter = 1,
           tau_section = 1,
           section_intercept = rep.int(5, times = n_sections),
           cc = seq(.5, point_scale - 1, 1),
           # need this part to get reproducible results
           .RNG.name = "base::Wichmann-Hill",
           .RNG.seed = seed,
           latent_trait = grades)
    inits <- list(inits_chain1, inits_chain2)
  }
  return(inits)
}


#' Function to get the maximal values of the Rhat statistic
#'
#' (defaults implemented for two chains)
#'
#' @param data long data frame with all information as in the jags model
#' defined below.
#' @param id_application the name of the application variable in the data
#' @param id_voter the name of the voter variable in the data (default = NULL)
#' @param grade_variable the name of the outcome variable
#' @param n_iter how many iterations used in the JAGS sampler?
#'  (default = 5000)
#' @param n_chains number of chains for the JAGS sampler. (default = 2)
#' @param n_adapt number of iterations discarded for the adaptation phase.
#'  (default = 1000)
#' @param n_burnin number of burnin iterations discarded. (default = 1000)
#' @param inits_type type of the initial values, default is "random", but if two
#' chains are used, the initial values can also be  "overdispersed"
#' @param id_section name of the section/panel variable
#' @param tau_section_name name of the standard error of the section effect, if
#' needed
#' @param other_variables are there other variables we would like to extract?
#' (NULL by default)
#' @param ordinal_scale dummy variable informing us on whether or not the
#' outcome is on an ordinal scale (default = FALSE)
#' @param point_scale integer informing us on the number of points of the
#' ordinal scale; not needed for continuous scale (default = NULL)
#' @param path_to_jags_model the path to the jags txt file, if null, the
#' default model is used. (default = NULL)
#' @param initial_values The list of initial values can be provided directly
#' @param seed set a seed for the JAGS model (default = 1991)
#'
#' @return a number: the maximal values of the computed Rhats
#' @export
#'
get_max_rhat <- function(data, id_application, id_voter, grade_variable,
                         n_burnin = 1000, n_iter = 5000, n_adapt = 1000,
                         n_chains = 2,
                         inits_type = "random",
                         id_section = NULL,
                         tau_section_name = NULL,
                         other_variables = c("nu", "voter_intercept"),
                         ordinal_scale = FALSE,
                         point_scale = NULL,
                         path_to_jags_model = NULL,
                         initial_values = NULL, seed = 1991) {

  # Test(s)
  ## 1) At the moment no default jags model is implemented with merging of
  # panels/sections
  if (!is.null(id_section) & is.null(path_to_jags_model)) {
    stop(paste0("At the moment no default jags model is implemented with ",
                "merging of panels/sections, please add the path to the model ",
                "in path_to_jags_model."))
  }

  # The other variables to be computed
  if (!is.null(id_section)) {
    other_variables <- c(other_variables, "section_intercept")
  }
  if (ordinal_scale) other_variables <- c(other_variables, "cc")


  # refit the model without pooling the 2 chains
  mcmc <- get_mcmc_samples(data = data,
                           id_application = id_application,
                           id_voter = id_voter,
                           id_section = id_section,
                           grade_variable = grade_variable,
                           n_chains = n_chains, n_iter = n_iter,
                           n_burnin = n_burnin, n_adapt = n_adapt,
                           ordinal_scale = ordinal_scale,
                           point_scale = point_scale,
                           tau_section_name = tau_section_name,
                           # also trace the variables which are not for primary
                           # interest to get Rhat values for these as well
                           other_variables = other_variables,
                           seed = seed, quiet = TRUE,
                           dont_bind = TRUE,
                           inits_type = inits_type,
                           path_to_jags_model = path_to_jags_model,
                           initial_values = initial_values)


  # do not perform multivariate computation since this sometimes leads to
  # error messages and we do not need that result
  # use the whole series, not only the second half (autoburnin = FALSE)
  rhat <- gelman.diag(mcmc$samples, autoburnin = FALSE,
                      multivariate = FALSE)$psrf[ ,1]

  return(max(rhat))
}
