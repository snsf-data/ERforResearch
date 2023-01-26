# -------------------------------
# Bayesian hierarchical modelling
# -------------------------------

options(dplyr.summarise.inform = FALSE)

#' Write a default jags model
#'
#' This function writes a default JAGS model, given the parameters and stores it
#' as a. txt in a certain path.
#'
#' @param outcome_variable the nature of the outcome variable: continuous or
#' ordinal.  By default the grades are assumed to be `"continuous"`.
#' @param residuals the nature of the residuals in the model: homogeneous or
#' heterogeneous. By default the residuals are assumed to be `"homogeneous"`.
#' @param subpanels the individual votes come from different sub-panels that
#' need to be merged. By default this is set to `FALSE` meaning that rankings
#' are produced for each panel separately.
#' @param path where should it be stored, relative to getwd()? By default this
#' is set to `"jags_model.txt"`. Hence it is simply written to the working
#' directory.
#' @param quiet if set to `TRUE`, the function does not show any messages of
#' caution.
#' @param nine_point_scale_continuous boolean variable indicated whether or not
#' a nine-point continuous scale is used. By default is is set to `FALSE`, which
#' means that a six-point scale is used by default. Be aware that those
#' specifications are due to the usage of the package by the Swiss National
#' Science Foundation.
#'
#' @details
#' Note that many of the specific default parameters are specified for easy
#' usage and integration in the funding evaluation system at the Swiss National
#' Science Foundation and might not be applicable to other evaluation systems.
#'
#' The model defined here has a random component for the proposal and
#' the assessor. If the parameter subpanels is set to TRUE an additional
#' grouping for the panel is defined. The user is invited to write their own
#' model definition if more flexibility is needed.
#'
#' The path to a user-specified model can then be given as parameter to the
#' `get_mcmc_samples()` or the `get_er_from_jags()` functions. The user can also
#' simply decide between a continuous or ordinal outcome variable, homogeneous
#' or heterogeneous residuals, with or without subpanel merging (those options
#' are all integrated), write the model in a .txt file and adapt it from there.
#'
#' @examples
#' # The model definition .txt is stored in the file "default_jags_model.txt"
#' \dontrun{
#' get_default_jags_model()
#' }
#' @export
get_default_jags_model <- function(outcome_variable = "continuous",
                                   nine_point_scale_continuous = FALSE,
                                   residuals = "homogeneous",
                                   subpanels = FALSE,
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
      residuals == "homogeneous" & !subpanels){
    cat("model{
      # Likelihood:
      for (i in 1:n) { # i is not the proposal but the review
          grade[i] ~ dnorm(mu[i], inv_sigma2)
                # inv_sigma2 is precision (1 / variance)
          mu[i] <- overall_mean + proposal_intercept[num_proposal[i]] +
          assessor_intercept[num_proposal[i], num_assessor[i]]
          }
      # Ranks: (for the expected ranks)
      rank_theta[1:n_proposal] <- rank(-proposal_intercept[])
      # Priors:
      for (j in 1:n_proposal){
        proposal_intercept[j] ~ dnorm(0, inv_tau_proposal2)
      }
      for (l in 1:n_assessor){
        for(j in 1:n_proposal){
          assessor_intercept[j, l] ~ dnorm(nu[l], inv_tau_assessor2)
        }
      }
      for (l in 1:n_assessor){
        nu[l] ~ dnorm(0, 4) # 1/(0.5^2) = 4
      }
      sigma ~ dunif(0.000001, 2)
      inv_sigma2 <- pow(sigma, -2)
      inv_tau_proposal2 <- pow(tau_proposal, -2)
      tau_proposal ~ dunif(0.000001, 2)
      inv_tau_assessor2 <- pow(tau_assessor, -2)
      tau_assessor ~ dunif(0.000001, 2)
      }",
        file = paste0(getwd(), "/", path))
  }
  if (outcome_variable == "continuous" & nine_point_scale_continuous &
      residuals == "homogeneous" & !subpanels){
    cat("model{
      # Likelihood:
      for (i in 1:n) { # i is not the proposal but the review
          grade[i] ~ dnorm(mu[i], inv_sigma2)
                # inv_sigma2 is precision (1 / variance)
          mu[i] <- overall_mean + proposal_intercept[num_proposal[i]] +
          assessor_intercept[num_proposal[i], num_assessor[i]]
          }
      # Ranks: (for the expected ranks)
      rank_theta[1:n_proposal] <- rank(-proposal_intercept[])
      # Priors:
      for (j in 1:n_proposal){
        proposal_intercept[j] ~ dnorm(0, inv_tau_proposal2)
      }
      for (l in 1:n_assessor){
        for(j in 1:n_proposal){
          assessor_intercept[j, l] ~ dnorm(nu[l], inv_tau_assessor2)
        }
      }
      for (l in 1:n_assessor){
        nu[l] ~ dnorm(0, 1) # 1/(1^2) = 1
      }
      sigma ~ dunif(0.000001, 3)
      inv_sigma2 <- pow(sigma, -2)
      inv_tau_proposal2 <- pow(tau_proposal, -2)
      tau_proposal ~ dunif(0.000001, 3)
      inv_tau_assessor2 <- pow(tau_assessor, -2)
      tau_assessor ~ dunif(0.000001, 3)
      }",
        file = paste0(getwd(), "/", path))
  }
  if (outcome_variable == "ordinal" & residuals == "homogeneous" &
      !subpanels){
    cat("model{
      # Likelihood:
      for (i in 1:n) { # i is not the proposal but the review
          grade[i] ~ dinterval(latent_trait[i], c[])
          latent_trait[i] ~ dnorm(mu[i], inv_sigma2)
          mu[i] <- overall_mean + proposal_intercept[num_proposal[i]] +
            assessor_intercept[num_proposal[i], num_assessor[i]]
      }
      # Ranks:
      rank_theta[1:n_proposal] <- rank(-proposal_intercept[])
      # Priors:
      for (j in 1:n_proposal){
        proposal_intercept[j] ~ dnorm(0, inv_tau_proposal2)
      }
      for (l in 1:n_assessor){
        for(j in 1:n_proposal){
          assessor_intercept[j, l] ~ dnorm(nu[l], inv_tau_assessor2)
        }
      }
      for (l in 1:n_assessor){
        nu[l] ~ dnorm(0, 4) # 1/(0.5^2) = 4
      }

      for (k in 1:5){
        cc[k] ~ dunif(-1000, 1000)
      }
      c[1:5] <- sort(cc)

      sigma ~ dunif(0.000001, 2)
      inv_sigma2 <- pow(sigma, -2)
      inv_tau_proposal2 <- pow(tau_proposal, -2)
      tau_proposal ~ dunif(0.000001, 2)
      inv_tau_assessor2 <- pow(tau_assessor, -2)
      tau_assessor ~ dunif(0.000001, 2)
    }", file = paste0(getwd(), "/", path))
  }
  if (outcome_variable == "continuous" & !nine_point_scale_continuous &
      residuals == "heterogeneous" & !subpanels){
    cat("model{
     # Likelihood:
      for (i in 1:n) { # i is not the proposal but the review
      grade[i] ~ dnorm(mu[i], inv_sigma2[num_proposal[i]])
      # inv_sigma2 is precision (1 / variance)
      mu[i] <- overall_mean + proposal_intercept[num_proposal[i]] +
      assessor_intercept[num_proposal[i], num_assessor[i]]
      # + panel_intercept[num_panel[i]] # if needed
      }
      # Ranks:
      rank_theta[1:n_proposal] <- rank(-proposal_intercept[])
      # Priors:
      for (j in 1:n_proposal){
        proposal_intercept[j] ~ dnorm(0, inv_tau_proposal2)
        sigma2[j] = max(10^(-6),
                    exp(alpha + beta * log(mean_proposal[j]) + omega[j]))
        inv_sigma2[j] = 1/sigma2[j]
        omega[j] ~ dnorm(0, inv_tau_omega2)
      }
      for (l in 1:n_assessor){
        for(j in 1:n_proposal){
          assessor_intercept[j, l] ~ dnorm(nu[l], inv_tau_assessor2)
        }
      }
      for (l in 1:n_assessor){
        nu[l] ~ dnorm(0, 4) # 1/(0.5^2) = 4
      }

      inv_tau_proposal2 <- pow(tau_proposal, -2)
      inv_tau_omega2 <- pow(tau_omega, -2)
      inv_tau_assessor2 <- pow(tau_assessor, -2)
      tau_proposal ~ dunif(10^(-6), 2)
      tau_assessor ~ dunif(10^(-6), 2)
      tau_omega ~ dunif(10^(-6), 10)

      alpha ~ dnorm(0, 0.01) #T(0.001, 1)
      beta ~ dnorm(0, 0.01) #T(0.001, 1)
      }",
        file = paste0(getwd(), "/", path))
  }
  if (outcome_variable == "ordinal" & residuals == "heterogeneous" &
      !subpanels){
    cat("model{
          # Likelihood:
      for (i in 1:n) { # i is not the proposal but the review
          grade[i] ~ dinterval(latent_trait[i], c[])
          latent_trait[i] ~ dnorm(mu[i], inv_sigma2[num_proposal[i]])
          mu[i] <- overall_mean + proposal_intercept[num_proposal[i]] +
            assessor_intercept[num_proposal[i], num_assessor[i]]
      }
      # Ranks:
      rank_theta[1:n_proposal] <- rank(-proposal_intercept[])
      # Priors:
      for (j in 1:n_proposal){
        proposal_intercept[j] ~ dnorm(0, inv_tau_proposal2)
        sigma2[j] = max(10^(-6),
                    exp(alpha + beta * log(mean_proposal[j]) + omega[j]))
        inv_sigma2[j] = 1/sigma2[j]
        omega[j] ~ dnorm(0, inv_tau_omega2)
      }
      for (l in 1:n_assessor){
        for(j in 1:n_proposal){
          assessor_intercept[j, l] ~ dnorm(nu[l], inv_tau_assessor2)
        }
      }
      for (l in 1:n_assessor){
        nu[l] ~ dnorm(0, 4) # 1/(0.5^2) = 4
      }

      for (k in 1:5){
        cc[k] ~ dunif(-1000, 1000)
      }
      c[1:5] <- sort(cc)

      inv_tau_omega2 <- pow(tau_omega, -2)
      inv_tau_proposal2 <- pow(tau_proposal, -2)
      inv_tau_assessor2 <- pow(tau_assessor, -2)
      tau_proposal ~ dunif(10^(-6), 2)
      tau_assessor ~ dunif(10^(-6), 2)
      tau_omega ~ dunif(10^(-6), 10)

      alpha ~ dnorm(0, 0.01) #T(0.001, 1)
      beta ~ dnorm(0, 0.01) #T(0.001, 1)
    }", file = paste0(getwd(), "/", path))
  }

  if (outcome_variable == "continuous" & !nine_point_scale_continuous &
      residuals == "homogeneous" & subpanels){
    cat("model{
      # Likelihood:
      for (i in 1:n) { # i is not the proposal but the review
          grade[i] ~ dnorm(mu[i], inv_sigma2)
                # inv_sigma2 is precision (1 / variance)
          mu[i] <- overall_mean + proposal_intercept[num_proposal[i]] +
                   assessor_intercept[num_proposal[i], num_assessor[i]] +
                   panel_intercept[num_panel[i]]
      }
      # Ranks: (for the expected ranks)
      rank_theta[1:n_proposal] <- rank(-proposal_intercept[])
      # Priors:
      for (j in 1:n_proposal){
        proposal_intercept[j] ~ dnorm(0, inv_tau_proposal2)
      }
      for (l in 1:n_assessor){
        for(j in 1:n_proposal){
          assessor_intercept[j, l] ~ dnorm(nu[l], inv_tau_assessor2)
        }
      }
      for (l in 1:n_assessor){
        nu[l] ~ dnorm(0, 4) # 1/(0.5^2) = 4
      }
      for (l in 1:n_panel){
        panel_intercept[l] ~ dnorm(0, inv_tau_panel2)
      }

      inv_sigma2 <- pow(sigma, -2)
      inv_tau_proposal2 <- pow(tau_proposal, -2)
      inv_tau_assessor2 <- pow(tau_assessor, -2)
      inv_tau_panel2 <- pow(tau_panel, -2)
      sigma ~ dunif(10^(-6), 2)
      tau_proposal ~ dunif(10^(-6), 2)
      tau_assessor ~ dunif(10^(-6), 2)
      tau_panel ~ dunif(10^(-6), 2)
      }",
        file = paste0(getwd(), "/", path))
  }
  if (outcome_variable == "continuous" & nine_point_scale_continuous &
      residuals == "homogeneous" & subpanels){
    cat("model{
      # Likelihood:
      for (i in 1:n) { # i is not the proposal but the review
          grade[i] ~ dnorm(mu[i], inv_sigma2)
                # inv_sigma2 is precision (1 / variance)
          mu[i] <- overall_mean + proposal_intercept[num_proposal[i]] +
                   assessor_intercept[num_proposal[i], num_assessor[i]] +
                   panel_intercept[num_panel[i]]
      }
      # Ranks: (for the expected ranks)
      rank_theta[1:n_proposal] <- rank(-proposal_intercept[])
      # Priors:
      for (j in 1:n_proposal){
        proposal_intercept[j] ~ dnorm(0, inv_tau_proposal2)
      }
      for (l in 1:n_assessor){
        for(j in 1:n_proposal){
          assessor_intercept[j, l] ~ dnorm(nu[l], inv_tau_assessor2)
        }
      }
      for (l in 1:n_assessor){
        nu[l] ~ dnorm(0, 1) # 1/(1^2) = 1
      }
      for (l in 1:n_panel){
        panel_intercept[l] ~ dnorm(0, inv_tau_panel2)
      }

      inv_sigma2 <- pow(sigma, -2)
      inv_tau_proposal2 <- pow(tau_proposal, -2)
      inv_tau_assessor2 <- pow(tau_assessor, -2)
      inv_tau_panel2 <- pow(tau_panel, -2)
      sigma ~ dunif(10^(-6), 3)
      tau_proposal ~ dunif(10^(-6), 3)
      tau_assessor ~ dunif(10^(-6), 3)
      tau_panel ~ dunif(10^(-6), 3)
      }",
        file = paste0(getwd(), "/", path))
  }
  if (outcome_variable == "ordinal" & residuals == "homogeneous" &
      subpanels){
    cat("model{
      # Likelihood:
      for (i in 1:n) { # i is not the proposal but the review
          grade[i] ~ dinterval(latent_trait[i], c[])
          latent_trait[i] ~ dnorm(mu[i], inv_sigma2)
          mu[i] <- overall_mean + proposal_intercept[num_proposal[i]] +
            assessor_intercept[num_proposal[i], num_assessor[i]] +
                   panel_intercept[num_panel[i]]
      }
      # Ranks:
      rank_theta[1:n_proposal] <- rank(-proposal_intercept[])
      # Priors:
      for (j in 1:n_proposal){
        proposal_intercept[j] ~ dnorm(0, inv_tau_proposal2)
      }
      for (l in 1:n_assessor){
        for(j in 1:n_proposal){
          assessor_intercept[j, l] ~ dnorm(nu[l], inv_tau_assessor2)
        }
      }
      for (l in 1:n_assessor){
        nu[l] ~ dnorm(0, 4) # 1/(0.5^2) = 4
      }
      for (l in 1:n_panel){
        panel_intercept[l] ~ dnorm(0, inv_tau_panel2)
      }

      for (k in 1:5){
        cc[k] ~ dunif(-1000, 1000)
      }
      c[1:5] <- sort(cc)

      sigma ~ dunif(0.000001, 2)
      inv_sigma2 <- pow(sigma, -2)
      inv_tau_proposal2 <- pow(tau_proposal, -2)
      tau_proposal ~ dunif(0.000001, 2)
      inv_tau_assessor2 <- pow(tau_assessor, -2)
      tau_assessor ~ dunif(0.000001, 2)
      inv_tau_panel2 <- pow(tau_panel, -2)
      tau_panel ~ dunif(0.000001, 2)
    }", file = paste0(getwd(), "/", path))
  }
  if (outcome_variable == "continuous" & !nine_point_scale_continuous &
      residuals == "heterogeneous" & subpanels){
    cat("model{
     # Likelihood:
      for (i in 1:n) { # i is not the proposal but the review
      grade[i] ~ dnorm(mu[i], inv_sigma2[num_proposal[i]])
      # inv_sigma2 is precision (1 / variance)
      mu[i] <- overall_mean + proposal_intercept[num_proposal[i]] +
               assessor_intercept[num_proposal[i], num_assessor[i]]+
               panel_intercept[num_panel[i]]
      }
      # Ranks:
      rank_theta[1:n_proposal] <- rank(-proposal_intercept[])
      # Priors:
      for (j in 1:n_proposal){
        proposal_intercept[j] ~ dnorm(0, inv_tau_proposal2)
        sigma2[j] = max(10^(-6),
                    exp(alpha + beta * log(mean_proposal[j]) + omega[j]))
        inv_sigma2[j] = 1/sigma2[j]
        omega[j] ~ dnorm(0, inv_tau_omega2)
      }
      for (l in 1:n_assessor){
        for(j in 1:n_proposal){
          assessor_intercept[j, l] ~ dnorm(nu[l], inv_tau_assessor2)
        }
      }
      for (l in 1:n_assessor){
        nu[l] ~ dnorm(0, 4) # 1/(0.5^2) = 4
      }
      for (l in 1:n_panel){
        panel_intercept[l] ~ dnorm(0, inv_tau_panel2)
      }

      inv_tau_proposal2 <- pow(tau_proposal, -2)
      inv_tau_omega2 <- pow(tau_omega, -2)
      inv_tau_assessor2 <- pow(tau_assessor, -2)
      tau_proposal ~ dunif(10^(-6), 2)
      tau_assessor ~ dunif(10^(-6), 2)
      tau_omega ~ dunif(10^(-6), 10)
      inv_tau_panel2 <- pow(tau_panel, -2)
      tau_panel ~ dunif(0.000001, 2)

      alpha ~ dnorm(0, 0.01) #T(0.001, 1)
      beta ~ dnorm(0, 0.01) #T(0.001, 1)
      }",
        file = paste0(getwd(), "/", path))
  }
  if (outcome_variable == "ordinal" & residuals == "heterogeneous" &
      subpanels){
    cat("model{
          # Likelihood:
      for (i in 1:n) { # i is not the proposal but the review
          grade[i] ~ dinterval(latent_trait[i], c[])
          latent_trait[i] ~ dnorm(mu[i], inv_sigma2[num_proposal[i]])
          mu[i] <- overall_mean + proposal_intercept[num_proposal[i]] +
                   assessor_intercept[num_proposal[i], num_assessor[i]] +
                   panel_intercept[num_panel[i]]
      }
      # Ranks:
      rank_theta[1:n_proposal] <- rank(-proposal_intercept[])
      # Priors:
      for (j in 1:n_proposal){
        proposal_intercept[j] ~ dnorm(0, inv_tau_proposal2)
        sigma2[j] = max(10^(-6),
                    exp(alpha + beta * log(mean_proposal[j]) + omega[j]))
        inv_sigma2[j] = 1/sigma2[j]
        omega[j] ~ dnorm(0, inv_tau_omega2)
      }
      for (l in 1:n_assessor){
        for(j in 1:n_proposal){
          assessor_intercept[j, l] ~ dnorm(nu[l], inv_tau_assessor2)
        }
      }
      for (l in 1:n_assessor){
        nu[l] ~ dnorm(0, 4) # 1/(0.5^2) = 4
      }
      for (l in 1:n_panel){
        panel_intercept[l] ~ dnorm(0, inv_tau_panel2)
      }


      for (k in 1:5){
        cc[k] ~ dunif(-1000, 1000)
      }
      c[1:5] <- sort(cc)

      inv_tau_omega2 <- pow(tau_omega, -2)
      inv_tau_proposal2 <- pow(tau_proposal, -2)
      inv_tau_assessor2 <- pow(tau_assessor, -2)
      inv_tau_panel2 <- pow(tau_panel, -2)
      tau_proposal ~ dunif(10^(-6), 2)
      tau_assessor ~ dunif(10^(-6), 2)
      tau_omega ~ dunif(10^(-6), 10)
      tau_panel ~ dunif(10^(-6), 2)

      alpha ~ dnorm(0, 0.01) #T(0.001, 1)
      beta ~ dnorm(0, 0.01) #T(0.001, 1)
    }", file = paste0(getwd(), "/", path))
  }
}


#' MCMC samples
#'
#' Helper function to get the mcmc samples. It is essentially a function wrapped
#' around the function `runjags::run.jags()`. The function also tests
#' convergence of the chains and extends them until convergence is achieved.
#' Find more in Details.
#'
#' @param data data frame, in long format, with all needed variables as
#' specified in the JAGS model defined in the text file with path given in
#' `path_to_jags_model`.
#' @param path_to_jags_model the path to text file including the JAGS model
#' definition. By default `= NULL`, and the function will use a default model as
#' implemented in the package; in `get_default_jags_model()`.
#' @param n_iter how many iterations should be used in the JAGS sampler? This is
#' the same as `sample` in the `runjags::run.jags()` function. It is set to
#' `10000` by default.
#' @param n_chains the number of chains for the JAGS sampler. The default number
#' of chains is set to four. This creates optimal conditions and should not be
#' changed. The same parameter in `runjags::run.jags()` is called `n.chains`.
#' @param n_adapt the number of adaptive iterations discarded for the adaptation
#' phase. By default it is set to `1000`. The same parameter in
#' `runjags::run.jags()` is called `adapt`.
#' @param n_burnin the number of burnin iterations which will not be included in
#' the adaptation phase. By default it is set to `4000` and the same parameter
#' in `runjags::run.jags()` is called `burnin`.
#' @param max_iter the maximum number of iteration. The JAGS sample will be
#' extended until convergence of the chains. To ensure that the sampler does not
#' run and extend forever a maximum number of iterations per chain can be
#' defined. Once this number of iterations is achieved, the sampler will not be
#' further extended. By default, the function allows up to `1000000` iterations
#' before stopping.
#' @param id_proposal the name of the variable in `data` that indicates the ID
#' of the evaluated proposal.
#' @param id_assessor the name of the variable in `data` that indicates the ID
#' of the assessor. The default `= NULL`, for the case where each assessor only
#' evaluates/grades one proposal.
#' @param id_panel the name of the variable in `data` that indicates the ID
#' of the panel. The default `= NULL`, for the case where all proposals were
#' evaluated in the same panel, or were each panel creates its own ranking.
#' In the other scenario, a ranking would be established combining or merging
#' all panels.
#' @param grade_variable the name of the variable in `data` with the outcome
#' variable, i.e. the grade or score.
#' @param theta_name the name of the proposal intercept in the JAGS model.
#' The default that also goes with the default JAGS model build in the package
#' is `proposal_intercept`.
#' @param tau_name_proposal the name of tau in the JAGS model, being the
#' standard error of the proposal effects. The default that also goes with the
#' default JAGS model build in the package is `tau_proposal`.
#' @param tau_name_assessor name of the standard error of the assessor effect in
#' the JAGS model.  The default that also goes with the default JAGS model build
#' in the package is `tau_assessor`.
#' @param rank_theta_name the name of the rank of theta in the JAGS model. The
#' default that also goes with the default JAGS model build in the package is
#' `rank_theta`.
#' @param assessor_name the name of the assessor intercept in the JAGS model.
#' The default that also goes with the default JAGS model build in the package
#' is `assessor_intercept`.
#' @param assessor_behavior_name the name of the parameter in the JAGS model
#' indicating the voter behavior. By default it is set to `nu` as this is the
#' name in the default JAGS model.
#' @param tau_name_panel the name of the standard error of the panel effect, if
#' needed. The default that also goes with the default JAGS model build in the
#' package is `tau_panel`. This is only needed if a ranking has to be
#' established combining or merging all panels, and therefore only important if
#' `id_panel` is not `NULL`.
#' @param sigma_name name of the standard deviation of the full model. The
#' default that also goes with the default JAGS model build in the package is
#' `sigma`.
#' @param ordinal_scale dummy variable informing us on whether or not the
#' outcome is on an ordinal scale. By default, we assume a numeric scale and
#' this parameter is set to `FALSE`.
#' @param point_scale integer informing us on the number of points of the
#' ordinal scale; not needed for continuous scale. By default, we assume a
#' numeric scale and do not need this information and the  parameter is set to
#' `NULL`.
#' @param heterogeneous_residuals dummy variable informing us on whether or not
#' the residuals should be heterogeneous. By default the residuals are assumed
#' homogeneous and this parameter is set to `FALSE`.
#' @param seed the seed for the JAGS model (default = `1991`). This seed will
#' generate the seeds for the JAGS samplers, which ensures reproducibility; see
#' also Details.
#' @param quiet if the default model is used this function generates a warning.
#' if `quiet = TRUE`, the warning is not shown.
#' @param dont_bind setting this parameter to `TRUE` will pool all the chains
#' together before returning the MCMC. By default it is however set to `FALSE`.
#' @param inits_type the type of the initial values. By default the initial
#' values are randomly selected, i.e. `inits_type = "random"`.
#' Alternatively, if four chains are used, the initial values can also be
#' `"overdispersed"`.
#' @param names_variables_to_sample the variables to sample can be specified,
#' writin their names here, as a character-vector. The default is `NULL` and
#' the default variables are used.
#' @param initial_values The list of initial values for the jags sampler can be
#' provided directly. Otherwise `get_inits_overdispersed_four_chains` for the
#' overdispersed version is used, or they are randomly selected. Always using a
#' seed to ensure computational reproducibility.
#' @param rhat_threshold the threshold for rhat to decide whether or not the
#' chains converged. Gelman suggested 1.1, but the smaller the better. Hence
#' this functions threshold is set to `1.01` by default.
#' @param runjags_method the method with which to call JAGS (from
#' `runjags::run.jags()` with the default being set to `parallel`).
#' @param minimal_testing should only the most important model parameters be
#' tested for convergence, e.g. the ranks and the variances? By default this is
#' set to `FALSE`.
#'
#' @import runjags
#' @import coda
#' @importFrom dplyr pull select distinct mutate group_by summarise left_join filter n
#' @importFrom stats rnorm runif
#' @importFrom magrittr %>%
#'
#' @return The function returns a list with: the MCMC samples in `samples` for
#' all parameters defined in the model, more information on the number of
#' chains, and the length of the adaptation, iteration and burnin phase, and
#' an indicator on whether or not the chains converged (according to all Rhats
#' being smaller than `rhat_threshold`). Additionally a summary report matrix
#' from `runjags::run.jags` is included (in `summary`) for each sampled
#' parameter with, among others, the effective sample size, MCerror, and rhat
#' values.
#'
#' @details
#' A note on convergence:
#'
#'#' Note that a convergence test is applied in this function: If with
#' the specified length of adaptation and burnin phase together with the number
#' of specified iterations not all Rhat values are below `rhat_threshold`, the
#' latter Ns (n_adapt, n_burnin, and n_iter) are multiplied by multiplied by 5,
#' 10, 5x10, 10x10, 5x10^2, 10x10^2, etc, until either all Rhat values are
#' below `rhat_threshold`or the number of iteration would exceed max_iter. If
#' the Rhat values are still not all small enough a warning message is printed.
#'
#' A note on reproducibility:
#'
#' To ensure reproducibility of the chains, all parameters need explicit
#' starting values and each chain needs an explicitly selected random samplers
#' (`.RNG.name`) and an explicitly selected seed (`.RNG.seed`). For four chains
#' the samplers are chosen among `base::Wichmann-Hill`,
#' `base::Marsaglia-Multicarry`, `base::Super-Duper`, `base::Mersenne-Twister`.
#' For more, or less than four chains a sample (with or without replacement) of
#' these samplers is selected.
#'
#' @export
#' @examples
#' data_panel1 <- get_mock_data() %>%
#'      filter(panel == "p1")
#' \dontrun{
#' mcmc_samples <- get_mcmc_samples(data = data_panel1,
#'                                  id_proposal = "proposal",
#'                                  id_assessor = "assessor",
#'                                  grade_variable = "num_grade")
#'                                  }
get_mcmc_samples <- function(data, id_proposal, id_assessor,
                             grade_variable,
                             path_to_jags_model = NULL,
                             n_chains = 4, n_iter = 10000,
                             n_burnin = 4000, n_adapt = 1000,
                             id_panel = NULL, max_iter = 1000000,
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
                             seed = 1991, quiet = FALSE,
                             dont_bind = FALSE,
                             inits_type = "random", # or "overdispersed"
                             names_variables_to_sample = NULL,
                             initial_values = NULL,
                             rhat_threshold = 1.01,
                             runjags_method = "parallel",
                             minimal_testing = FALSE) {

  ## Tests:
  #########
  ## 1) are all the relevant variables in the data?
  presence_of_variables <- !c(id_proposal, id_assessor, grade_variable,
                              id_panel) %in% names(data)
  if (any(presence_of_variables)) {
    stop(paste0(c(id_proposal, id_assessor, grade_variable, id_panel)[
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
                           subpanels = !is.null(id_panel))
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
                " with four chains at the moment. Please provide your own ",
                "initial values using parameter initial_values."))
  }
  ## 6) If initial values are provided, make sure they are provided for all
  # chains (if length of initial values larger one).
  if (is.null(initial_values) & (length(initial_values) > 1) &
      (n_chains == length(initial_values))) {
    stop(paste0("You need to provide a list of ", n_chains, " initial values ",
                "because your model is run over ", n_chains, " chains."))
  }

  ## Prepare data for jags:
  #########################

  # We need to add integer/count-like numeric ids for the proposals and the
  # assessors in the data for the computation of the JAGS model:
  num_proposal <- data %>%
    select(id_proposal) %>%
    distinct() %>%
    mutate(num_proposal = seq_len(n()))
  num_assessor <- data %>%
    select(id_assessor) %>%
    distinct() %>%
    mutate(num_assessor = seq_len(n()))
  if (!is.null(id_panel)) {
    num_panel <- data %>%
      select(id_panel) %>%
      distinct() %>%
      mutate(num_panel = seq_len(n()))
  }
  # Pasting everything together
  data <- data %>%
    left_join(num_proposal, by = id_proposal) %>%
    left_join(num_assessor, by = id_assessor)
  if (!is.null(id_panel)) {
    data <- data %>%
      left_join(num_panel, by = id_panel)
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
      group_by(num_proposal) %>%
      summarise(av = mean(get(grade_variable) - 1, na.rm = TRUE)) %>%
      dplyr::pull(.data$av) %>%
      mean()
  } else {
    num_outcome <- data %>% dplyr::pull(get(grade_variable))
    overall_mean <- data %>%
      group_by(num_proposal) %>%
      summarise(av = mean(get(grade_variable), na.rm = TRUE)) %>%
      dplyr::pull(.data$av) %>%
      mean()
  }

  # The list with the data:
  data_for_jags <-
    list(n = nrow(data),
         n_proposal = length(unique(data$num_proposal)),
         n_assessor = length(unique(data$num_assessor)),
         grade = num_outcome,
         overall_mean = overall_mean,
         num_proposal = data$num_proposal,
         num_assessor = data$num_assessor)
  if (!is.null(id_panel)) { # if we have a section effect
    data_for_jags <- c(data_for_jags,
                       list(n_panel = length(unique(data$num_panel)),
                            num_panel = data$num_panel))
  }
  if (heterogeneous_residuals){ # if the residuals are supposed to be heteroge.
    data_for_jags <- c(data_for_jags,
                       list(mean_proposal = data %>%
                              group_by(num_proposal) %>%
                              summarise(av = mean(get(grade_variable),
                                                  na.rm = TRUE)) %>%
                              dplyr::pull(.data$av)))
  }

  # Sample certain _variables_ from the model, depending on whether or not they
  # are specified.
  if (is.null(names_variables_to_sample)) {
    if (minimal_testing){
      variables <- c(tau_name_proposal, tau_name_assessor, rank_theta_name)
    } else{
      variables <- c(theta_name, tau_name_proposal, tau_name_assessor,
                     rank_theta_name, assessor_name, assessor_behavior_name)
    }
    if (!heterogeneous_residuals){
      variables <- c(variables, sigma_name)
    }
    if (!is.null(id_panel)) {
      variables <- c(variables, tau_name_panel)
    }
    if (heterogeneous_residuals){
      variables <- c(variables, "tau_omega", "alpha", "beta")
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
               list(proposal_intercept =
                      runif(data_for_jags$n_proposal, -2, 2),
                    assessor_intercept =
                      matrix(runif(data_for_jags$n_assessor*
                                     data_for_jags$n_proposal, -2, 2),
                             ncol = data_for_jags$n_assessor,
                             nrow = data_for_jags$n_proposal),
                    nu = runif(data_for_jags$n_assessor, -2, 2),
                    sigma = runif(1, 10^(-6), 2),
                    tau_proposal = runif(1, 10^(-6), 2),
                    tau_assessor = runif(1, 10^(-6), 2),
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
    if (!is.null(id_panel)) {
      inits <- lapply(inits,
                      function(I)
                        c(I, list(tau_panel = runif(1, 10^(-6), 2),
                                  panel_intercept =
                                    runif(data_for_jags$n_panel, -2, 2))))
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
        merging_panels = !is.null(id_panel),
        ordinal_scale = ordinal_scale,
        point_scale = point_scale,
        heterogeneous_residuals = heterogeneous_residuals,
        n_proposals = data_for_jags$n_proposal,
        n_assessors = data_for_jags$n_assessor,
        n_panels = data_for_jags$n_panel,
        grades = data_for_jags$grade,
        seed = seed)
  }

  ## Build the jags model
  #######################
  runjags.options(force.summary = TRUE)
  samps1 <-
    run.jags(path_to_jags_model,
             data = data_for_jags, n.chains = n_chains, adapt = n_adapt,
             burnin = n_burnin, inits = inits, monitor = variables,
             sample = n_iter, method = runjags_method, silent.jags = TRUE)


  # Extend the jags model if needed
  counter <- 1
  while (any(samps1$psrf$psrf[, "Point est."] > rhat_threshold) &
         (counter + 1) * n_iter <= max_iter){
    print(paste0("Extension JAGS samples number ", counter, "."))
    counter <- counter + 1
    samps1 <- extend.jags(samps1, sample = n_iter, method = runjags_method)
  }

  rhats <- samps1$psrf$psrf[, "Point est."]

  if (dont_bind) {
    mcmc_samples <- mcmc(samps1)
  } else mcmc_samples <- mcmc(do.call(rbind, samps1$mcmc))

  if (any(rhats > rhat_threshold)) {
    print(paste0(
      "Caution: Even after extending the JAGS iterations to ",
      counter * n_iter, " the max of the Rhat values is ",
      round(max(rhats, na.rm = TRUE), 3),
      " e.g. > ", rhat_threshold, ". The problematic parameter(s) is (are): ",
      paste0(names(rhats[which(rhats > rhat_threshold)]),
             collapse = ", "), "."))
  }


  return(list(samples = mcmc_samples,
              n_chains = n_chains,
              n_adapt = n_adapt,
              n_burnin = n_burnin,
              n_iter = n_iter * counter,
              conv_status =
                ifelse(max(rhats, na.rm = TRUE) > rhat_threshold,
                       paste0("Problem max rhat > ", rhat_threshold),
                       paste0("Converged (with threshold set to ",
                              rhat_threshold, ")")),
              summary = summary(samps1)))
}
