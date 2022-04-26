# -------------------------------
# Bayesian hierarchical modelling
# -------------------------------

options(dplyr.summarise.inform = FALSE)

# The model:
#' Write a default jags model
#'
#' This function simply stores a txt file with a default jags model, in a
#' certain path.
#' @param outcome_variable the nature of the outcome variable: continuous or
#' ordinal. (default = "continuous")
#' @param residuals the nature of the residuals in the model: homogeneous or
#' heterogeneous. (default = "homogeneous")
#' @param subsections the individual votes come from different sub-panels that
#' need to be merged. (default = FALSE)
#' @param path where should it be stored, from getwd()? (default = jags_model.txt)
#' @param quiet if TRUE, do not show messages.
#' @param nine_point_scale_continuous only needed if nine-point continuous scale
#' is used (default = FALSE).
#' @details The model defined here has a random component for the application /
#' the proposal and the voter / the evaluator. If the parameter subpanels is set
#' to TRUE an addtional grouping for the section or panel is defined. The user
#' is invited to write their own model definition if more flexibility is needed.
#' The path to the latter can then be given as parameter to get_mcmc_samples.
#' The user can however already decide between a continuous or ordinal outcome
#' variable, homogeneous or heterogeneous residuals, and with or without
#' subpanel merging (those options are integrated).
#' @examples
#' # The model definition .txt is stored in the file "default_jags_model.txt"
#' \dontrun{
#' get_default_jags_model()
#' }
#' @export
get_default_jags_model <- function(outcome_variable = "continuous",
                                   nine_point_scale_continuous = FALSE,
                                   residuals = "homogeneous",
                                   subsections = FALSE,
                                   path = "default_jags_model.txt",
                                   quiet = FALSE) {
  if (!(outcome_variable %in% c("continuous", "ordinal")) |
      !(residuals %in% c("homogeneous", "heterogeneous"))){
    stop("The outcome variable can either be continuous or ordinal, while the
         residuals can be homogeneous or heterogeneous. Check the function
         parameters outcome_variable and residuals!")
  }

  if (outcome_variable == "ordinal" & !quiet) {
    print("Be aware that the default ordinal model fixes the outcome to a six-
          point scale.")
  }

  if (outcome_variable == "continuous" & !nine_point_scale_continuous &
      residuals == "homogeneous" & !subsections){
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
      for (l in 1:n_voter){
        for(j in 1:n_application){
          voter_intercept[j, l] ~ dnorm(nu[l], inv_tau_voter2)
        }
      }
      for (l in 1:n_voter){
        nu[l] ~ dnorm(0, 4) # 1/(0.5^2) = 4
      }
      sigma ~ dunif(0.000001, 2)
      inv_sigma2 <- pow(sigma, -2)
      inv_tau_application2 <- pow(tau_application, -2)
      tau_application ~ dunif(0.000001, 2)
      inv_tau_voter2 <- pow(tau_voter, -2)
      tau_voter ~ dunif(0.000001, 2)
      }",
        file = paste0(getwd(), "/", path))
  }
  if (outcome_variable == "continuous" & nine_point_scale_continuous &
      residuals == "homogeneous" & !subsections){
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
      for (l in 1:n_voter){
        for(j in 1:n_application){
          voter_intercept[j, l] ~ dnorm(nu[l], inv_tau_voter2)
        }
      }
      for (l in 1:n_voter){
        nu[l] ~ dnorm(0, 1) # 1/(1^2) = 1
      }
      sigma ~ dunif(0.000001, 3)
      inv_sigma2 <- pow(sigma, -2)
      inv_tau_application2 <- pow(tau_application, -2)
      tau_application ~ dunif(0.000001, 3)
      inv_tau_voter2 <- pow(tau_voter, -2)
      tau_voter ~ dunif(0.000001, 3)
      }",
        file = paste0(getwd(), "/", path))
  }
  if (outcome_variable == "ordinal" & residuals == "homogeneous" &
      !subsections){
    cat("model{
      # Likelihood:
      for (i in 1:n) { # i is not the application but the review
          grade[i] ~ dinterval(latent_trait[i], c[])
          latent_trait[i] ~ dnorm(mu[i], inv_sigma2)
          mu[i] <- overall_mean + application_intercept[num_application[i]] +
            voter_intercept[num_application[i], num_voter[i]]
      }
      # Ranks:
      rank_theta[1:n_application] <- rank(-application_intercept[])
      # Priors:
      for (j in 1:n_application){
        application_intercept[j] ~ dnorm(0, inv_tau_application2)
      }
      for (l in 1:n_voter){
        for(j in 1:n_application){
          voter_intercept[j, l] ~ dnorm(nu[l], inv_tau_voter2)
        }
      }
      for (l in 1:n_voter){
        nu[l] ~ dnorm(0, 4) # 1/(0.5^2) = 4
      }

      for (k in 1:5){
        cc[k] ~ dunif(-1000, 1000)
      }
      c[1:5] <- sort(cc)

      sigma ~ dunif(0.000001, 2)
      inv_sigma2 <- pow(sigma, -2)
      inv_tau_application2 <- pow(tau_application, -2)
      tau_application ~ dunif(0.000001, 2)
      inv_tau_voter2 <- pow(tau_voter, -2)
      tau_voter ~ dunif(0.000001, 2)
    }", file = paste0(getwd(), "/", path))
  }
  if (outcome_variable == "continuous" & !nine_point_scale_continuous &
      residuals == "heterogeneous" & !subsections){
    cat("model{
     # Likelihood:
      for (i in 1:n) { # i is not the application but the review
      grade[i] ~ dnorm(mu[i], inv_sigma2[num_application[i]])
      # inv_sigma2 is precision (1 / variance)
      mu[i] <- overall_mean + application_intercept[num_application[i]] +
      voter_intercept[num_application[i], num_voter[i]]
      # + section_intercept[num_section[i]] # if needed
      }
      # Ranks:
      rank_theta[1:n_application] <- rank(-application_intercept[])
      # Priors:
      for (j in 1:n_application){
        application_intercept[j] ~ dnorm(0, inv_tau_application2)
        sigma2[j] = max(10^(-6),
                    exp(alpha + beta * log(mean_application[j]) + omega[j]))
        inv_sigma2[j] = 1/sigma2[j]
        omega[j] ~ dnorm(0, inv_tau_omega2)
      }
      for (l in 1:n_voter){
        for(j in 1:n_application){
          voter_intercept[j, l] ~ dnorm(nu[l], inv_tau_voter2)
        }
      }
      for (l in 1:n_voter){
        nu[l] ~ dnorm(0, 4) # 1/(0.5^2) = 4
      }

      inv_tau_application2 <- pow(tau_application, -2)
      inv_tau_omega2 <- pow(tau_omega, -2)
      inv_tau_voter2 <- pow(tau_voter, -2)
      tau_application ~ dunif(10^(-6), 2)
      tau_voter ~ dunif(10^(-6), 2)
      tau_omega ~ dunif(10^(-6), 10)

      alpha ~ dnorm(0, 0.01) #T(0.001, 1)
      beta ~ dnorm(0, 0.01) #T(0.001, 1)
      }",
        file = paste0(getwd(), "/", path))
  }
  if (outcome_variable == "ordinal" & residuals == "heterogeneous" &
      !subsections){
    cat("model{
          # Likelihood:
      for (i in 1:n) { # i is not the application but the review
          grade[i] ~ dinterval(latent_trait[i], c[])
          latent_trait[i] ~ dnorm(mu[i], inv_sigma2[num_application[i]])
          mu[i] <- overall_mean + application_intercept[num_application[i]] +
            voter_intercept[num_application[i], num_voter[i]]
      }
      # Ranks:
      rank_theta[1:n_application] <- rank(-application_intercept[])
      # Priors:
      for (j in 1:n_application){
        application_intercept[j] ~ dnorm(0, inv_tau_application2)
        sigma2[j] = max(10^(-6),
                    exp(alpha + beta * log(mean_application[j]) + omega[j]))
        inv_sigma2[j] = 1/sigma2[j]
        omega[j] ~ dnorm(0, inv_tau_omega2)
      }
      for (l in 1:n_voter){
        for(j in 1:n_application){
          voter_intercept[j, l] ~ dnorm(nu[l], inv_tau_voter2)
        }
      }
      for (l in 1:n_voter){
        nu[l] ~ dnorm(0, 4) # 1/(0.5^2) = 4
      }

      for (k in 1:5){
        cc[k] ~ dunif(-1000, 1000)
      }
      c[1:5] <- sort(cc)

      inv_tau_omega2 <- pow(tau_omega, -2)
      inv_tau_application2 <- pow(tau_application, -2)
      inv_tau_voter2 <- pow(tau_voter, -2)
      tau_application ~ dunif(10^(-6), 2)
      tau_voter ~ dunif(10^(-6), 2)
      tau_omega ~ dunif(10^(-6), 10)

      alpha ~ dnorm(0, 0.01) #T(0.001, 1)
      beta ~ dnorm(0, 0.01) #T(0.001, 1)
    }", file = paste0(getwd(), "/", path))
  }

  if (outcome_variable == "continuous" & !nine_point_scale_continuous &
      residuals == "homogeneous" & subsections){
    cat("model{
      # Likelihood:
      for (i in 1:n) { # i is not the application but the review
          grade[i] ~ dnorm(mu[i], inv_sigma2)
                # inv_sigma2 is precision (1 / variance)
          mu[i] <- overall_mean + application_intercept[num_application[i]] +
                   voter_intercept[num_application[i], num_voter[i]] +
                   section_intercept[num_section[i]]
      }
      # Ranks: (for the expected ranks)
      rank_theta[1:n_application] <- rank(-application_intercept[])
      # Priors:
      for (j in 1:n_application){
        application_intercept[j] ~ dnorm(0, inv_tau_application2)
      }
      for (l in 1:n_voter){
        for(j in 1:n_application){
          voter_intercept[j, l] ~ dnorm(nu[l], inv_tau_voter2)
        }
      }
      for (l in 1:n_voter){
        nu[l] ~ dnorm(0, 4) # 1/(0.5^2) = 4
      }
      for (l in 1:n_section){
        section_intercept[l] ~ dnorm(0, inv_tau_section2)
      }

      inv_sigma2 <- pow(sigma, -2)
      inv_tau_application2 <- pow(tau_application, -2)
      inv_tau_voter2 <- pow(tau_voter, -2)
      inv_tau_section2 <- pow(tau_section, -2)
      sigma ~ dunif(10^(-6), 2)
      tau_application ~ dunif(10^(-6), 2)
      tau_voter ~ dunif(10^(-6), 2)
      tau_section ~ dunif(10^(-6), 2)
      }",
        file = paste0(getwd(), "/", path))
  }
  if (outcome_variable == "continuous" & nine_point_scale_continuous &
      residuals == "homogeneous" & subsections){
    cat("model{
      # Likelihood:
      for (i in 1:n) { # i is not the application but the review
          grade[i] ~ dnorm(mu[i], inv_sigma2)
                # inv_sigma2 is precision (1 / variance)
          mu[i] <- overall_mean + application_intercept[num_application[i]] +
                   voter_intercept[num_application[i], num_voter[i]] +
                   section_intercept[num_section[i]]
      }
      # Ranks: (for the expected ranks)
      rank_theta[1:n_application] <- rank(-application_intercept[])
      # Priors:
      for (j in 1:n_application){
        application_intercept[j] ~ dnorm(0, inv_tau_application2)
      }
      for (l in 1:n_voter){
        for(j in 1:n_application){
          voter_intercept[j, l] ~ dnorm(nu[l], inv_tau_voter2)
        }
      }
      for (l in 1:n_voter){
        nu[l] ~ dnorm(0, 1) # 1/(1^2) = 1
      }
      for (l in 1:n_section){
        section_intercept[l] ~ dnorm(0, inv_tau_section2)
      }

      inv_sigma2 <- pow(sigma, -2)
      inv_tau_application2 <- pow(tau_application, -2)
      inv_tau_voter2 <- pow(tau_voter, -2)
      inv_tau_section2 <- pow(tau_section, -2)
      sigma ~ dunif(10^(-6), 3)
      tau_application ~ dunif(10^(-6), 3)
      tau_voter ~ dunif(10^(-6), 3)
      tau_section ~ dunif(10^(-6), 3)
      }",
        file = paste0(getwd(), "/", path))
  }
  if (outcome_variable == "ordinal" & residuals == "homogeneous" &
      subsections){
    cat("model{
      # Likelihood:
      for (i in 1:n) { # i is not the application but the review
          grade[i] ~ dinterval(latent_trait[i], c[])
          latent_trait[i] ~ dnorm(mu[i], inv_sigma2)
          mu[i] <- overall_mean + application_intercept[num_application[i]] +
            voter_intercept[num_application[i], num_voter[i]] +
                   section_intercept[num_section[i]]
      }
      # Ranks:
      rank_theta[1:n_application] <- rank(-application_intercept[])
      # Priors:
      for (j in 1:n_application){
        application_intercept[j] ~ dnorm(0, inv_tau_application2)
      }
      for (l in 1:n_voter){
        for(j in 1:n_application){
          voter_intercept[j, l] ~ dnorm(nu[l], inv_tau_voter2)
        }
      }
      for (l in 1:n_voter){
        nu[l] ~ dnorm(0, 4) # 1/(0.5^2) = 4
      }
      for (l in 1:n_section){
        section_intercept[l] ~ dnorm(0, inv_tau_section2)
      }

      for (k in 1:5){
        cc[k] ~ dunif(-1000, 1000)
      }
      c[1:5] <- sort(cc)

      sigma ~ dunif(0.000001, 2)
      inv_sigma2 <- pow(sigma, -2)
      inv_tau_application2 <- pow(tau_application, -2)
      tau_application ~ dunif(0.000001, 2)
      inv_tau_voter2 <- pow(tau_voter, -2)
      tau_voter ~ dunif(0.000001, 2)
      inv_tau_section2 <- pow(tau_section, -2)
      tau_section ~ dunif(0.000001, 2)
    }", file = paste0(getwd(), "/", path))
  }
  if (outcome_variable == "continuous" & !nine_point_scale_continuous &
      residuals == "heterogeneous" & subsections){
    cat("model{
     # Likelihood:
      for (i in 1:n) { # i is not the application but the review
      grade[i] ~ dnorm(mu[i], inv_sigma2[num_application[i]])
      # inv_sigma2 is precision (1 / variance)
      mu[i] <- overall_mean + application_intercept[num_application[i]] +
               voter_intercept[num_application[i], num_voter[i]]+
               section_intercept[num_section[i]]
      }
      # Ranks:
      rank_theta[1:n_application] <- rank(-application_intercept[])
      # Priors:
      for (j in 1:n_application){
        application_intercept[j] ~ dnorm(0, inv_tau_application2)
        sigma2[j] = max(10^(-6),
                    exp(alpha + beta * log(mean_application[j]) + omega[j]))
        inv_sigma2[j] = 1/sigma2[j]
        omega[j] ~ dnorm(0, inv_tau_omega2)
      }
      for (l in 1:n_voter){
        for(j in 1:n_application){
          voter_intercept[j, l] ~ dnorm(nu[l], inv_tau_voter2)
        }
      }
      for (l in 1:n_voter){
        nu[l] ~ dnorm(0, 4) # 1/(0.5^2) = 4
      }
      for (l in 1:n_section){
        section_intercept[l] ~ dnorm(0, inv_tau_section2)
      }

      inv_tau_application2 <- pow(tau_application, -2)
      inv_tau_omega2 <- pow(tau_omega, -2)
      inv_tau_voter2 <- pow(tau_voter, -2)
      tau_application ~ dunif(10^(-6), 2)
      tau_voter ~ dunif(10^(-6), 2)
      tau_omega ~ dunif(10^(-6), 10)
      inv_tau_section2 <- pow(tau_section, -2)
      tau_section ~ dunif(0.000001, 2)

      alpha ~ dnorm(0, 0.01) #T(0.001, 1)
      beta ~ dnorm(0, 0.01) #T(0.001, 1)
      }",
        file = paste0(getwd(), "/", path))
  }
  if (outcome_variable == "ordinal" & residuals == "heterogeneous" &
      subsections){
    cat("model{
          # Likelihood:
      for (i in 1:n) { # i is not the application but the review
          grade[i] ~ dinterval(latent_trait[i], c[])
          latent_trait[i] ~ dnorm(mu[i], inv_sigma2[num_application[i]])
          mu[i] <- overall_mean + application_intercept[num_application[i]] +
                   voter_intercept[num_application[i], num_voter[i]] +
                   section_intercept[num_section[i]]
      }
      # Ranks:
      rank_theta[1:n_application] <- rank(-application_intercept[])
      # Priors:
      for (j in 1:n_application){
        application_intercept[j] ~ dnorm(0, inv_tau_application2)
        sigma2[j] = max(10^(-6),
                    exp(alpha + beta * log(mean_application[j]) + omega[j]))
        inv_sigma2[j] = 1/sigma2[j]
        omega[j] ~ dnorm(0, inv_tau_omega2)
      }
      for (l in 1:n_voter){
        for(j in 1:n_application){
          voter_intercept[j, l] ~ dnorm(nu[l], inv_tau_voter2)
        }
      }
      for (l in 1:n_voter){
        nu[l] ~ dnorm(0, 4) # 1/(0.5^2) = 4
      }
      for (l in 1:n_section){
        section_intercept[l] ~ dnorm(0, inv_tau_section2)
      }


      for (k in 1:5){
        cc[k] ~ dunif(-1000, 1000)
      }
      c[1:5] <- sort(cc)

      inv_tau_omega2 <- pow(tau_omega, -2)
      inv_tau_application2 <- pow(tau_application, -2)
      inv_tau_voter2 <- pow(tau_voter, -2)
      inv_tau_section2 <- pow(tau_section, -2)
      tau_application ~ dunif(10^(-6), 2)
      tau_voter ~ dunif(10^(-6), 2)
      tau_omega ~ dunif(10^(-6), 10)
      tau_section ~ dunif(10^(-6), 2)

      alpha ~ dnorm(0, 0.01) #T(0.001, 1)
      beta ~ dnorm(0, 0.01) #T(0.001, 1)
    }", file = paste0(getwd(), "/", path))
  }
}


#' Mcmc samples
#'
#' Helper function to get the mcmc samples. The function tests convergence
#' diagnostics at the same time. Find more in Details.
#'
#' @param data long data frame with all information as in the jags model
#' defined below.
#' @param id_application the name of the application variable in the data
#' @param id_voter the name of the voter variable in the data (default = NULL)
#' @param grade_variable the name of the outcome variable
#' @param path_to_jags_model the path to the jags txt file, if null, the
#' default model is used. (default = NULL)
#' @param n_iter how many iterations used in the JAGS sampler?
#'  (default = 5000)
#' @param n_chains number of chains for the JAGS sampler. The default number of
#' chains is set to four. This creates optimal conditions and should not be
#' changed.
#' @param n_adapt number of iterations discarded for the adaptation phase.
#'  (default = 1000)
#' @param n_burnin number of burnin iterations discarded. (default = 1000)
#' @param max_iter maximum number of iteration (default = 1 million)
#' @param id_section name of the section
#' @param theta_name the name of the application identifier in the JAGS model.
#' (default = application_intercept")
#' @param tau_name name of the tau in the jags model, being the standard error
#' of the application effects (default = tau_application).
#' @param tau_voter_name name of the standard error of the voter effect
#' (default = tau_voter).
#' @param rank_theta_name the name of the rank of theta in the JAGS model
#' (default = rank_theta)
#' @param voter_name the name of the voter intercept in the JAGS model (default
#' = voter_intercept).
#' @param tau_section_name name of the standard error of the section effect, if
#' needed (default = tau_section).
#' @param sigma_name name of the standard deviation of the full model.
#' (default = sigma)
#' @param ordinal_scale dummy variable informing us on whether or not the
#' outcome is on an ordinal scale (default = FALSE)
#' @param point_scale integer informing us on the number of points of the
#' ordinal scale; not needed for continuous scale (default = NULL)
#' @param heterogeneous_residuals dummy variable informing us on whether or not
#' the residuals should be heterogeneous (in this case you have to update the
#' JAGS model too, default = FALSE)
#' @param seed set a seed for the JAGS model (default = 1991)
#' @param quiet if the default model is used this function generates a warning.
#' if quiet = TRUE, this warning is not shown
#' @param dont_bind if TRUE the different chains are not pooled, and the MCMC
#' object is returned as it is (default = FALSE).
#' @param inits_type type of the initial values, default is "random", but if two
#' chains are used, the initial values can also be  "overdispersed"
#' @param variables_to_sample should the default variables be samples, or will
#' they be "specified" (in names_variables_to_sample), default is "default".
#' @param names_variables_to_sample if variables to sample are specified, write
#' their names here, as a character-vector, default is NULL.
#' @param initial_values The list of initial values for the jags sampler can be
#' provided directly
#' @param compute_ess Should the effective sample size and the mcmc errors be
#' calculated? (default = FALSE)
#' @param maximal_testing should the rhat of all parameters be computed (TRUE),
#' or just the essential ones (FALSE)? (default = FALSE)
#' @import rjags
#' @import coda
#' @import dplyr
#' @importFrom dplyr filter
#' @importFrom stats rnorm runif
#' @return The function returns a list with: a matrix with the samples for all
#'  parameters defined in the model, and more information on the number of
#'  chains, and the length of the adaptation, iteration and burnin phase, and
#'  an indicator on whether or not the chains converged (according to Rhat being
#'  smaller than 1.01.). Note that n_iter, burnin, adapt are the number per
#'  chain. Then, optionally the effective sample size (ess) for the relevant
#'  parameters is calculated as well as the MCMC error
#'  (= sd(parameter)/sqrt(ess)) and added to the list.
#'
#' @details Note that a convergence test is applied in this function: If with
#' the specified length of adaptation and burnin phase together with the number
#' of specified iterations not all Rhat values are below 1.01, the latter Ns
#' (n_adapt, n_burnin, and n_iter) are multiplied by multiplied by 5, 10, 5x10,
#' 10x10, 5x10^2, 10x10^2, etc, until either all Rhat values are below 1.01
#' or the number of iteration would exceed max_iter. If the Rhat values are
#' still not all small enough a warning message is printed.
#' @export
#' @examples
#' data_panel1 <- get_mock_data() %>%
#'      filter(panel == "p1")
#' \dontrun{
#' mcmc_samples <- get_mcmc_samples(data = data_panel1,
#'                                  id_application = "application",
#'                                  id_voter = "voter",
#'                                  grade_variable = "num_grade")
#'                                  }
get_mcmc_samples <- function(data, id_application, id_voter,
                             grade_variable,
                             path_to_jags_model = NULL,
                             n_chains = 4, n_iter = 5000,
                             n_burnin = 1000, n_adapt = 1000,
                             id_section = NULL, max_iter = 1000000,
                             theta_name = "application_intercept",
                             tau_name = "tau_application",
                             sigma_name = "sigma",
                             tau_voter_name = "tau_voter",
                             tau_section_name = "tau_section",
                             rank_theta_name = "rank_theta",
                             voter_name = "voter_intercept",
                             ordinal_scale = FALSE,
                             point_scale = NULL,
                             heterogeneous_residuals = FALSE,
                             seed = 1991, quiet = FALSE,
                             dont_bind = FALSE,
                             inits_type = "random", # or "overdispersed"
                             variables_to_sample = "default",
                             # either "default" or "specified"
                             # (specify the names in the argument below)
                             names_variables_to_sample = NULL,
                             initial_values = NULL,
                             compute_ess = FALSE, maximal_testing = FALSE) {

  ## Tests:
  #########
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
    stop(paste0("The grade_variable has to be numeric. Even if you consider ",
                "an ordinal outcome, 1 should represent the lowest ",
                "category."))
  }
  ## 3) If no path to model definition is given a default one is used
  if (is.null(path_to_jags_model)) {
    get_default_jags_model(outcome_variable = ifelse(ordinal_scale,
                                                     "ordinal", "continuous"),
                           residuals = ifelse(heterogeneous_residuals,
                                              "heterogeneous", "homogeneous"),
                           subsections = !is.null(id_section))
    path_to_jags_model <- paste0(getwd(), "/", "default_jags_model.txt")
    if (!quiet)
      print("Default model is used (check get_default_jags_model function!).")
  }
  ## 4) If ordinal_scale, do we have a number of points on the scale?
  if (ordinal_scale) {
    if (is.null(point_scale)) stop(paste0("If you want to operate on an ",
                                          "ordinal scale, please specify the ",
                                          "number of points on the scale, in ",
                                          "point_scale."))
  }

  ## 5) The default overdispersed initial values can only be provided with two
  # chains
  if (inits_type == "overdispersed" & is.null(initial_values) & n_chains != 4) {
    stop(paste0("The default overdispersed initial values can only be provided",
                " with two chains at the moment. Please provide your own ",
                "initial values using parameter initial_values."))
  }
  ## 6) If initial values are provided, make sure they are provided for all
  # chains (if length of initial values larger one).
  if (is.null(initial_values) & (length(initial_values) > 1) &
      (n_chains == length(initial_values))) {
    stop(paste0("You need to provide a list of ", n_chains, " initial values ",
                "because your model is run over ", n_chains, " chains."))
  }
  ## 7) variables_to_sample should be either "default" or "specified". If
  # "specified", names_variables_to_sample has to be not null.
  if (!variables_to_sample %in% c("default", "specified")) {
    stop(paste0("The parameter variables_to_sample should be either 'default' ",
                "or 'specified', and not ", variables_to_sample, "."))
  } else {
    if (variables_to_sample != "default" & is.null(names_variables_to_sample)) {
      stop(paste0("You announced that the variables to sample would be ",
                  "specified but forgot to specify them in ",
                  "names_variables_to_sample."))
    }
  }

  ## Prepare data for jags:
  #########################

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

  ## Preparation of the list containing all the data for rjags:
  #############################################################
  # The numeric version of the outcome
  if (ordinal_scale){ # For the ordinal model dinterval() is used, which forces
    # the level of the ordinal scale to start at 0, not at 1.
    num_outcome <- data %>% dplyr::pull(get(grade_variable)) - 1
    overall_mean <- data %>%
      group_by(num_application) %>%
      summarise(av = mean(get(grade_variable) - 1, na.rm = TRUE)) %>%
      dplyr::pull(.data$av) %>%
      mean()
    } else {
      num_outcome <- data %>% dplyr::pull(get(grade_variable))
      overall_mean <- data %>%
        group_by(num_application) %>%
        summarise(av = mean(get(grade_variable), na.rm = TRUE)) %>%
        dplyr::pull(.data$av) %>%
        mean()
    }

  # The list with the data:
  data_for_jags <-
    list(n = nrow(data),
         n_application = length(unique(data$num_application)),
         n_voter = length(unique(data$num_voter)),
         grade = num_outcome,
         overall_mean = overall_mean,
         num_application = data$num_application,
         num_voter = data$num_voter)
  if (!is.null(id_section)) { # if we have a section effect
    data_for_jags <- c(data_for_jags,
                       list(n_section = length(unique(data$num_section)),
                            num_section = data$num_section))
  }
  if (heterogeneous_residuals){ # if the residuals are supposed to be heteroge.
    data_for_jags <- c(data_for_jags,
                       list(mean_application = data %>%
                              group_by(num_application) %>%
                              summarise(av = mean(get(grade_variable),
                                                  na.rm = TRUE)) %>%
                              dplyr::pull(.data$av)))
  }

  # Sample certain _variables_ from the model, depending on whether or not they
  # are specified.
  if (variables_to_sample == "default") {
    if (maximal_testing) {
      variables <- c(theta_name, tau_name, tau_voter_name, rank_theta_name,
                     voter_name)
      if (!heterogeneous_residuals){
        variables <- c(variables, sigma_name)
      }
      if (!is.null(id_section)) {
        variables <- c(variables, tau_section_name)
      }
      if (heterogeneous_residuals){
        variables <- c(variables, "tau_omega", "alpha", "beta")
      }
    } else {
      variables <- c(theta_name, tau_name, tau_voter_name, rank_theta_name)
      if (!heterogeneous_residuals){
        variables <- c(variables, sigma_name)
      }
      if (!is.null(id_section)) {
        variables <- c(variables, tau_section_name)
      }
    }
  } else variables <- names_variables_to_sample


  ## Preparation of initial values:
  #################################
  # Set the initial values, as well as the seed(s):
  # To do so, we first generate a seed for each chain.
  set.seed(seed) # Inititalise seed (this is an argument in the function)
  seeds <- sample(seq_len(10^6), size = n_chains)
                 # Initialise seeds for the RNG samplers
  if (inits_type == "random") seed_for_inits <- sample(seq_len(10^6), 1)
                 # Initialise seed for the initial values of the sampler
  samplers <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry",
                "base::Super-Duper", "base::Mersenne-Twister")
  if (n_chains != 4) samplers <- rep(samplers,
                                     ceiling(n_chains/4))[seq_len(n_chains)]
  if (inits_type == "random") {
    # Generate random starting values if none are specified, if numeric scale of
    # the grade and potential sub-panels are not merged:
    set.seed(seed_for_inits)
    inits <-
      lapply(seq_len(n_chains),
             function(x)
               list(application_intercept =
                      runif(data_for_jags$n_application, -2, 2),
                    voter_intercept =
                      matrix(runif(data_for_jags$n_voter*
                                     data_for_jags$n_application, -2, 2),
                             ncol = data_for_jags$n_voter,
                             nrow = data_for_jags$n_application),
                    nu = runif(data_for_jags$n_voter, -2, 2),
                    sigma = runif(1, 10^(-6), 2),
                    tau_application = runif(1, 10^(-6), 2),
                    tau_voter = runif(1, 10^(-6), 2),
                    # need this part to get reproducible results
                    .RNG.name = samplers[x],
                    .RNG.seed = seeds[x]))

    if (ordinal_scale){
      # if we are operating on an ordinal scale, we need additionally inits
      # for cut-off values, and the latent_trait
      inits <- lapply(inits,
                      function(I) c(I,
                                    list(cc = seq(.5, point_scale - 1, 1),
                                    latent_trait = data_for_jags$grade)))
    }
    if (!is.null(id_section)) {
      inits <- lapply(inits,
                      function(I)
                        c(I, list(tau_section = runif(1, 10^(-6), 2),
                                  section_intercept =
                                    runif(data_for_jags$n_section, -2, 2))))
    }
    if (heterogeneous_residuals) {
      # Then we do not want a starting value for sigma
      inits <-
        lapply(inits, function(I) {I["sigma"] <- NULL; I})
      # But for tau_omega, alpha and beta
      inits <- lapply(inits,
                      function(I) c(I,
                                    list(tau_omega = runif(1, 10^(-6), 10),
                                         alpha = rnorm(1, 10^(-6), .01),
                                         beta = rnorm(1, 10^(-6), .01))))
    }
  } else {
    # If overdispersed starting values are used:
    inits <-
      get_inits_overdispersed_four_chains(
        merging_sections = !is.null(id_section),
        ordinal_scale = ordinal_scale,
        point_scale = point_scale,
        heterogeneous_residuals = heterogeneous_residuals,
        n_applications = data_for_jags$n_application,
        n_voters = data_for_jags$n_voter,
        n_sections = data_for_jags$n_section,
        grades = data_for_jags$grade,
        seed = seed)
  }

  ## Build the jags model
  #######################
  mod1 <- jags.model(file = path_to_jags_model,
                     data = data_for_jags,
                     n.chains = n_chains, n.adapt = n_adapt,
                     inits = inits,
                     quiet = TRUE)

  update(mod1, n_burnin)

  samps1 <- coda.samples(mod1, variable.names = variables,
                         n.iter = n_iter, quiet = TRUE)

  # Get the Rhat values and see if max is lower than 1.01:
  # do not perform multivariate computation since this sometimes leads to
  # error messages and we do not need that result
  # use the whole series, not only the second half (autoburnin = FALSE)
  rhat <- gelman.diag(samps1, autoburnin = FALSE,
                      multivariate = FALSE)$psrf[ ,1]

  update <- multiplier <- 1
  multiplicative <- c(sapply(1:7, function(i) c(i - 1, i -1)))
  while (max(rhat, na.rm = TRUE) > 1.01 & # update <= 5 &
         (n_iter * (5 * 2^((1 + update)%%2) *
                    10 ^ multiplicative[update]) <= max_iter)) {
    multiplier <- 5 * 2^((1 + update)%%2) * 10 ^ multiplicative[update]
    # First we double the adaptation, burnin and inits:
    # (the first round double, then times 4, 6, 10 and 15 times)
    mod1 <- jags.model(file = path_to_jags_model,
                       data = data_for_jags,
                       n.chains = n_chains,
                       n.adapt = n_adapt * multiplier,
                       inits = inits,
                       quiet = TRUE)
    if (!quiet) {
      print(paste0("Max. rhat: ", round(max(rhat), 2), ". Chain(s) is (are) ",
                   "rerun with higher adaptation and burnin phases and ",
                   "more iterations. Update number ", update, "."))
    }
    update(mod1, n_burnin * multiplier)

    samps1 <- coda.samples(mod1, variable.names = variables,
                           n.iter = n_iter*multiplier, quiet = TRUE)

    # Get the Rhat values and check again (for the next loop) if max is lower
    # than 1.01:
    rhat <- gelman.diag(samps1, autoburnin = FALSE,
                        multivariate = FALSE)$psrf[ ,1]
    update <- update + 1
  }

  if (dont_bind) {
    mcmc_samples <- mcmc(samps1)
  } else mcmc_samples <- mcmc(do.call(rbind, samps1))

  if (max(rhat, na.rm = TRUE) > 1.01) {
    print(paste0(
      "Caution: Even after increasing the adaption phase to ",
      n_adapt * multiplier, " iterations, the burnin phase to ",
      n_burnin * multiplier,
      " and the number of iterations to ",
      n_iter * multiplier, " the max of the Rhat values is ",
      round(max(rhat, na.rm = TRUE), 2),
      " e.g. > 1.01. The problematic parameter(s) is (are): ",
      paste0(names(rhat[which(rhat > 1.01)]), collapse = ", "), "."))
  }

  if (compute_ess) {
    if (!dont_bind) {
      ess <- effectiveSize(mcmc_samples)
      sd <- apply(mcmc_samples, MARGIN = 2, FUN = sd)
      mcmc_error <- sd/sqrt(ess)
    } else {
      ess <- effectiveSize(mcmc(do.call(rbind, mcmc_samples)))
      sd <- apply(do.call(rbind, mcmc_samples), MARGIN = 2, FUN = sd)
      mcmc_error <- sd/sqrt(ess)
    }
  } else {
    ess <- "Effective sample size not computed"
    mcmc_error <- "MCMC error not computed"
  }

  return(list(samples = mcmc_samples,
              n_chains = n_chains,
              n_adapt = n_adapt * ifelse(update > 1, multiplier, 1),
              n_burnin = n_burnin * ifelse(update > 1, multiplier, 1),
              n_iter = n_iter * ifelse(update > 1, multiplier, 1),
              conv_status = ifelse(max(rhat, na.rm = TRUE) > 1.01, "Problem max rhat > 1.01",
                                   "Converged"),
              ess = ess,
              mcmc_error = mcmc_error))
}
