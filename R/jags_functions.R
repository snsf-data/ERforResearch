# -------------------------------
# Bayesian hierarchical modelling
# -------------------------------

options(dplyr.summarise.inform = FALSE)

# The model:
#' Write a default jags model
#'
#' This function simply stores a txt file with a default jags model, in a
#' certain path.
#' @param path where should it be stored, from here()? (default = jags_model.txt)
#' @import here
#' @details The model defined here has a random component for the application /
#' the proposal and the voter / the evaluator. There is no other grouping
#' variable defined, as for example a section or panel. The user is invited to
#' write their own model definition if more flexibility is needed.
#' @examples
#' # The model definition .txt is stored in the file "default_jags_model.txt"
#' get_default_jags_model()
#' @export
get_default_jags_model <- function(path = "default_jags_model.txt") {
  cat("model{
      # Likelihood:
      for (i in 1:n) { # i is not the application but the review
          grade[i] ~ dnorm(mu[i], inv_sigma2)
                # inv_sigma2 is precision (1 / variance)
          mu[i] <- overall_mean + application_intercept[num_application[i]] +
          voter_intercept[num_application[i], num_voter[i]]
          }
      # Ranks: (for the expected ranks)
      rank_theta[1:n_application] <- rank(-application_intercept[])
      # Priors:
      for (j in 1:n_application){
        application_intercept[j] ~ dnorm(0, inv_tau_application2)
      }
      for (l in 1:n_voters){
        for(j in 1:n_application){
          voter_intercept[j, l] ~ dnorm(nu[l], inv_tau_voter2)
        }
      }
      for (l in 1:n_voters){
        nu[l] ~ dnorm(0, 4) # 1/(0.5^2) = 4
      }
      sigma ~ dunif(0.000001, 2)
      inv_sigma2 <- pow(sigma, -2)
      inv_tau_application2 <- pow(tau_application, -2)
      tau_application ~ dunif(0.000001, 2)
      inv_tau_voter2 <- pow(tau_voter, -2)
      tau_voter ~ dunif(0.000001, 2)
      }",
      file = here(path))
}


#' Mcmc samples
#'
#' Helper function to get the mcmc samples
#' @param data long data frame with all information as in the jags model
#' defined below.
#' @param id_application the name of the application variable in the data
#' @param id_voter the name of the voter variable in the data (default = NULL)
#' @param grade_variable the name of the outcome variable
#' @param model the model definition as character. (default = NULL)
#' @param path_to_jags_model the path to the jags txt file, if null, the
#' default model is used. (default = NULL)
#' @param n_burnin number of burnin iterations
#' @param n_iter how many iterations used in the JAGS sampler?
#'  (default = 5000)
#' @param n_chains number of chains for the JAGS sampler. (default = 4)
#' @param id_section name of the section
#' @param theta_name the name of the application identifier in the JAGS model.
#' (default = application_intercept")
#' @param tau_name name of the tau in the jags model, being the standard error
#' of the application effects. This variable is needed for the computation of
#' the rankability. (default = tau_application)
#' @param tau_voter_name name of the standard error of the voter effect. This
#' variable is needed for the computation of the rankability
#' (default = tau_voter)
#' @param tau_section_name name of the standard error of the section effect, if
#' needed
#' @param sigma_name name of the standard deviation of the full model.
#' (default = sigma)
#' @param other_variables are there other variables we would like to extract?
#' (NULL by default)
#' @param rank_theta_name the name of the rank of theta in the JAGS model
#' (default = rank_theta)
#' @param seed set a seed for the JAGS model (default = 1991)
#' @param quiet if the default model is used this function generates a warning.
#' if quiet = TRUE, this warning is not shown
#' @param dont_bind if TRUE the different chains are not pooled, and the MCMC
#' object is returned as it is.
#' @import rjags
#' @import coda
#' @import dplyr
#' @importFrom dplyr filter
#' @return matrix with the samples for all parameters defined in the model.
#' @export
#' @examples
#' data_panel1 <- get_mock_data() %>%
#'      filter(panel == "p1")
#' mcmc_samples <- get_mcmc_samples(data = data_panel1,
#'                                  id_application = "application",
#'                                  id_voter = "voter",
#'                                  grade_variable = "num_grade")

get_mcmc_samples <- function(data, id_application, id_voter,
                             grade_variable,
                             model = NULL,
                             path_to_jags_model = NULL,
                             n_chains = 2, n_iter = 10000,
                             n_burnin = 5000, id_section = NULL,
                             theta_name = "application_intercept",
                             tau_name = "tau_application",
                             sigma_name = "sigma",
                             tau_voter_name = "tau_voter",
                             tau_section_name = NULL,
                             rank_theta_name = "rank_theta",
                             other_variables = NULL,
                             seed = 1991, quiet = FALSE,
                             dont_bind = FALSE) {

  # Tests:
  ## 1) are all the relevant variables in the data?
  presence_of_variables <- !c(id_application, id_voter, grade_variable,
                              id_section) %in% names(data)
  if (any(presence_of_variables)) {
    stop(paste0(c(id_application, id_voter, grade_variable, id_section)[
      which(presence_of_variables)], " needed but not present in dataset."))
  }
  ## 2) The grade variable has to be numeric.
  if ((data %>%
       dplyr::pull(grade_variable) %>%
       class()) != "numeric") {
    stop(paste0("The grade_variable has to be numeric. Extensions for ",
                "non-linear models will be provided at a later stage."))
  }
  ## 3) If no path to model definition is given a default one is used
  if (is.null(path_to_jags_model) & !is.null(model)) {
    path_to_jags_model <- model
  } else {
    if (is.null(path_to_jags_model)) {
      if (!quiet)
        print("Default model is used (check get_default_jags_model function!).")
      get_default_jags_model()
      path_to_jags_model <- here("default_jags_model.txt")
    }
  }


  # We need to add integer/count-like numeric ids for the application and the
  # voters in the data for the computation of the JAGS model:
  num_application <- data %>%
    select(id_application) %>%
    distinct() %>%
    mutate(num_application = seq_len(n()))
  num_voter <- data %>%
    select(id_voter) %>%
    distinct() %>%
    mutate(num_voter = seq_len(n()))
  if (!is.null(id_section)) {
    num_section <- data %>%
      select(id_section) %>%
      distinct() %>%
      mutate(num_section = seq_len(n()))
  }
  # Pasting everything together
  data <- data %>%
    left_join(num_application, by = id_application) %>%
    left_join(num_voter, by = id_voter)
  if (!is.null(id_section)) {
    data <- data %>%
      left_join(num_section, by = id_section)
  }

  # Deleting potential NAs:
  data <- data %>%
    filter(!is.na(grade_variable))

  # RJAGS needs a list containing all the data
  data_for_jags <-
    list(n = nrow(data),
         n_application = length(unique(data$num_application)),
         n_voters = length(unique(data$num_voter)),
         grade = data %>% dplyr::pull(get(grade_variable)),
         overall_mean = data %>%
           group_by(num_application) %>%
           summarise(av = mean(get(grade_variable), na.rm = TRUE)) %>%
           dplyr::pull(.data$av) %>%
           mean(),
         num_application = data$num_application,
         num_voter = data$num_voter)
  if (!is.null(id_section)) { # if we have a section effect
    data_for_jags <- c(data_for_jags,
                       list(n_section = length(unique(data$num_section)),
                            num_section = data$num_section))
  }


  # Sample certain _variables_ from the model
  variables <- c(theta_name, tau_name, tau_voter_name, sigma_name,
                 other_variables, rank_theta_name, tau_section_name)
  # Initialise the thetas around 0
  # named_list <- list(rnorm(0, 0.001))
  # names(named_list) <- theta_name

  inits <- rep(list(list(.RNG.name = "base::Wichmann-Hill",
                         .RNG.seed = seed)), n_chains)
  if (n_chains > 1) {
    for (i in seq_len(n_chains)) inits[[i]]$.RNG.seed <- seed + (i-1)
  }
  # Build the jags model
  mod1 <- jags.model(file = path_to_jags_model,
                     data = data_for_jags,
                     n.chains = n_chains, n.adapt = n_burnin,
                     inits = inits, #function() named_list,
                     quiet = TRUE)

  samps1 <- coda.samples(mod1, variable.names = variables,
                         n.iter = n_iter, quiet = TRUE)
  if (dont_bind) {
    mcmc_samples <- mcmc(samps1)
  } else mcmc_samples <- mcmc(do.call(rbind, samps1))

  return(mcmc_samples)
}
