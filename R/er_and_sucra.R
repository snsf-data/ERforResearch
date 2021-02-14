# ----------------------------------
# Expected Rank and SUCRA quantities
# ----------------------------------



#' The Expected Rank from a JAGS model
#'
#' This function computes the ER via JAGS
#' @param data long data frame with all information as in the jags model
#' defined below.
#' @param id_application the name of the application variable in the data
#' @param id_voter the name of the voter variable in the data (default = NULL)
#' @param grade_variable the name of the outcome variable. This variable has to
#' be numeric and a higher grade means a better quality evaluation.
#' Extensions for non-linear models will be provided at a later stage.
#' @param path_to_jags_model the path to the jags txt file, if null, the
#' default model is used. (default = NULL)
#' @param n_burnin number of burnin iterations
#' @param n_iter how many iterations used in the JAGS sampler? (default = 5000)
#' @param n_chains number of chains for the JAGS sampler. (default = 4)
#' @param id_section name of the section
#' @param theta_name the name of the application identifier in the JAGS model.
#' (default = application_intercept")
#' @param tau_name name of the tau in the jags model, being the standard error
#' of the application effects. This variable is needed for the computation of
#' the rankability. (default = tau_application)
#' @param tau_name name of the tau in the jags model, being the standard error
#' of the application effects. This variable is needed for the computation of
#' the rankability. (default = tau_application)
#' @param tau_voter_name name of the standard error of the voter effect. This
#' variable is needed for the computation of the rankability.
#' (default = tau_voter)
#' @param tau_section_name name of the standard error of the section effect, if
#' needed (default = NULL).
#' @param sigma_name name of the standard deviation of the full model.
#' (default = sigma)
#' @param other_variables are there other variables we would like to extract
#' from the JAGS model samples? (NULL by default)
#' @param rank_theta_name the name of the rank of theta in the JAGS model
#' (default = rank_theta)
#' @param mcmc_samples if the mcmc sample has already been run (default = NULL).
#' @param seed set a seed for the JAGS model (default = 1991)
#' @param quiet if the default model is used this function generates a warning.
#' if quiet = TRUE, this warning is not shown
#' @import rjags
#' @import dplyr
#' @return the result is a list with the 1) ranked applications with their
#' expected rank and pcer, 2) the rankability and 3) the estimates of the
#' variances.
#' @export
#' @examples
#' data_panel1 <- get_mock_data() %>%
#'          filter(panel == "p1")
#' ER_results <- get_er_from_jags(data = data_panel1,
#'                                id_application = "application",
#'                                id_voter = "voter",
#'                                grade_variable = "num_grade",
#'                                path_to_jags_model = NULL)
get_er_from_jags <-  function(data, id_application,
                              id_voter = NULL,
                              grade_variable,
                              path_to_jags_model = NULL,
                              n_chains = 3, n_iter = 5000,
                              n_burnin = 1000, id_section = NULL,
                              theta_name = "application_intercept",
                              tau_name = "tau_application",
                              tau_voter_name = "tau_voter",
                              tau_section_name = NULL,
                              sigma_name = "sigma",
                              rank_theta_name = "rank_theta",
                              other_variables = NULL,
                              mcmc_samples = NULL,
                              seed = 1991,
                              quiet = FALSE) {

  # Tests:
  ## 1) If no mcmc samples are provided, name of the voter variables is needed:
  if (is.null(id_voter) & is.null(mcmc_samples)) {
    stop(paste0("Provide id_voter to compute MCMC samples. If you seperately",
                " computed MCMC samples of the model, provide them instead!"))
  }
  ## 2) The grade_variable has to be numeric:
  if ((data %>%
       dplyr::pull(grade_variable) %>%
       class()) != "numeric") {
    stop(paste0("The grade_variable has to be numeric. Extensions for ",
                "non-linear models will be provided at a later stage."))
  }
  ## 3) All variables needed have to be present in the data:
  presence_of_variables <- !c(id_application, id_voter, grade_variable,
                              id_section) %in% names(data)
  if (any(presence_of_variables)) {
    stop(paste0(c(id_application, id_voter, grade_variable, id_section)[
      which(presence_of_variables)], " needed, but not present in dataset."))
  }

  # Number of applications and overall mean:
  n_application <- data %>%
    dplyr::pull(get(id_application)) %>%
    unique() %>%
    length()
  overall_mean <- data %>%
    group_by(get(id_application)) %>%
    mutate(num_grade = get(grade_variable)) %>%
    summarise(av = mean(.data$num_grade, na.rm = TRUE)) %>%
    dplyr::pull(.data$av) %>%
    mean()

  # If not MCMC samples are provided, they are computed here:
  if (is.null(mcmc_samples)) {
    mcmc_samples <- get_mcmc_samples(data = data,
                                     id_application = id_application,
                                     id_voter = id_voter,
                                     grade_variable = grade_variable,
                                     path_to_jags_model = path_to_jags_model,
                                     n_chains = n_chains, n_iter = n_iter,
                                     n_burnin = n_burnin,
                                     id_section = id_section,
                                     theta_name = theta_name,
                                     tau_name = tau_name,
                                     tau_voter_name = tau_voter_name,
                                     tau_section_name = tau_section_name,
                                     sigma_name = sigma_name,
                                     other_variables = other_variables,
                                     rank_theta_name = rank_theta_name,
                                     seed = seed, quiet = quiet)
  }


  # Extract theta samples for the ranking based on the posterior means:
  colnames_theta <- paste0(theta_name, "[", seq_len(n_application), "]")
  # mcmc_samples_thetas <- mcmc_samples[, colnames_theta]
  mcmc_samples_thetas <- mcmc_samples[, colnames_theta]

  # Samples of the ranks of the thetas:
  colnames_ranks <- paste0(rank_theta_name, "[", seq_len(n_application), "]")
  # mcmc_samples_ranks <- mcmc_samples[, colnames_ranks]
  mcmc_samples_ranks <- mcmc_samples[, colnames_ranks]


  # The expected ranks, as the posterior expectation of the ranks:
  ers <- colMeans(mcmc_samples_ranks)

  # The results matrix
  results_rank_posterior_mean <-
    tibble(id_application = data %>%
             dplyr::pull(get(id_application)) %>%
             unique(),
           # The posterior means of the thetas
           pm = colMeans(mcmc_samples_thetas)) %>%
    mutate(rank_pm = rank(-(overall_mean + .data$pm))) # Rank based on the pm
  results_rank_posterior_mean$er <- ers # Adding the er to results matrix

  rankings_all <- data %>%
    mutate(num_grade = get(grade_variable)) %>%
    group_by(!!as.name(id_application)) %>%
    # Computation of the average grade
    summarise(avg_grade = mean(.data$num_grade, na.rm = TRUE)) %>%
    mutate(rank = rank(-.data$avg_grade), # Rank based on the average grade
           # `Rename` the application variable
           id_application = get(id_application)) %>%
    left_join(results_rank_posterior_mean,
              # Joining the posterior mean and ER results
              by = "id_application") %>%
    # Computation of the percentiles based on ER
    mutate(pcer = 100 * (.data$er - 0.5) / n_application) %>%
    select(.data$id_application, .data$rank, .data$rank_pm, .data$er,
           .data$avg_grade, .data$pcer)

  # Variance of the application effects
  tau2 <-
    mean(mcmc_samples[, which(colnames(mcmc_samples) == tau_name)])**2
  # Variance of the voter effects
  tau_voter2 <-
    mean(mcmc_samples[, which(colnames(mcmc_samples) == tau_voter_name)])**2
  # Model variance
  sigma2 <-
    mean(mcmc_samples[, which(colnames(mcmc_samples) == sigma_name)])**2

  # Computation of the Rankability
  rankability <- tau2 / (tau2 + tau_voter2 + sigma2)
  if (!is.null(id_section)){
    # Variance of the section effect, if needed
    tau_section2 <-
      mean(mcmc_samples[, which(colnames(mcmc_samples) == tau_section_name)])**2
    rankability <- tau2 / (tau2 + tau_voter2 + sigma2 + tau_section2)
  }

  variances <- list(tau2 = tau2, tau_voter2 = tau_voter2,
                    sigma2 = sigma2)
  return(list(rankings = rankings_all, rankability = rankability,
              variances = variances))
}




#' The Surface Under the Cumulative RAnking (SUCRA)
#'
#' This function computes the SUCRA
#' @param data long data frame with all information as in the jags model
#' defined below.
#' @param id_application the name of the application variable in the data
#' @param id_voter the name of the voter variable in the data (default = NULL)
#' @param grade_variable the name of the outcome variable
#' @param path_to_jags_model the path to the jags txt file, if null, the
#' default model is used. (default = NULL)
#' @param n_burnin number of burnin iterations
#' @param n_iter how many iterations used in the JAGS sampler? (default = 5000)
#' @param n_chains number of chains for the JAGS sampler. (default = 4)
#' @param id_section name of the section
#' @param theta_name the name of the application identifier. (default =
#' application_intercept")
#' @param tau_name name of the the sqrt of tau, being the precision of the
#' random effects, in the jags model. (default = sd_application)
#' @param tau_voter_name name of the (default = tau_voter)
#' @param tau_section_name name of the standard error of the section effect, if
#' needed (default = NULL).
#' @param sigma_name name of the standard deviation of the full model.
#' (default = sigma)
#' @param other_variables are there other variables we would like to extract?
#' (NULL by default)
#' @param rank_theta_name the name of the rank of theta in the JAGS model
#' (default = rank_theta)
#' @param mcmc_samples if the mcmc sample has already been run (default = NULL).
#' @param seed set a seed for the JAGS model (default = 1991)
#' @import rjags
#' @return the result is a names vector with the SUCRA of all applications
#' @examples
#' data_panel1 <- get_mock_data() %>%
#'          filter(panel == "p1")
#' SUCRA_results <- get_sucra(data = data_panel1,
#'                            id_application = "application",
#'                            id_voter = "voter", grade_variable = "num_grade")
#' @export
get_sucra <- function(data, id_application, id_voter, grade_variable,
                      path_to_jags_model = NULL,
                      n_chains = 3, n_iter = 5000,
                      n_burnin = 1000, id_section = NULL,
                      theta_name = "application_intercept",
                      tau_name = "tau_application",
                      tau_voter_name = "tau_voter",
                      tau_section_name = NULL,
                      sigma_name = "sigma",
                      other_variables = NULL,
                      rank_theta_name = "rank_theta",
                      mcmc_samples = NULL,
                      seed = 1991) {

  # Tests:
  ## 1) If no mcmc samples are provided, name of the voter variables is needed:
  if (is.null(id_voter) & is.null(mcmc_samples)) {
    stop(paste0("Provide id_voter to compute MCMC samples. If you seperately",
                " computed MCMC samples of the model, provide them instead!"))
  }
  ## 2) The grade_variable has to be numeric:
  if ((data %>%
       dplyr::pull(grade_variable) %>%
       class()) != "numeric") {
    stop(paste0("The grade_variable has to be numeric. Extensions for ",
                "non-linear models will be provided at a later stage."))
  }
  ## 3) All variables needed have to be present in the data:
  presence_of_variables <- !c(id_application, id_voter, grade_variable,
                              id_section) %in% names(data)
  if (any(presence_of_variables)) {
    stop(paste0(c(id_application, id_voter, grade_variable, id_section)[
      which(presence_of_variables)], " needed, but not present in dataset."))
  }

  # Number of applications and overall mean:
  n_application <- data %>%
    dplyr::pull(get(id_application)) %>%
    unique() %>%
    length()
  overall_mean <- data %>%
    group_by(get(id_application)) %>%
    summarise(av = mean(get(grade_variable), na.rm = TRUE)) %>%
    dplyr::pull(.data$av) %>%
    mean()

  # If not MCMC samples are provided, they are computed here:
  if (is.null(mcmc_samples)) {
    mcmc_samples <-
      get_mcmc_samples(data = data, id_application = id_application,
                       id_voter = id_voter, grade_variable = grade_variable,
                       path_to_jags_model = path_to_jags_model,
                       n_chains = n_chains, n_iter = n_iter,
                       n_burnin = n_burnin, id_section = id_section,
                       theta_name = theta_name, tau_name = tau_name,
                       tau_voter_name = tau_voter_name,
                       tau_section_name = tau_section_name,
                       sigma_name = sigma_name,
                       seed = seed, other_variables = other_variables,
                       rank_theta_name = rank_theta_name)
  }

  # Extract samples of ranks of the thetas:
  colnames_ranks <- paste0(rank_theta_name, "[", seq_len(n_application), "]")
  mcmc_samples_ranks <- mcmc_samples[, colnames_ranks]

  # Calculate the P(j = b) for the SUCRA:
  p_j_b <- matrix(NA, nrow = n_application, ncol = n_application)

  for (i in seq_len(n_application)) {
    for (j in seq_len(n_application)) {
      p_j_b[i, j] <- mean(mcmc_samples_ranks[, i] == j)
    }
  }
  # Compute the SUCRA
  sucra <- sapply(seq_len(nrow(p_j_b)), function(i) {
    mean(cumsum(p_j_b[i, -nrow(p_j_b)]))
  })

  # Add the names of the applications as names of the sucra-vector
  names(sucra) <- data %>%
    dplyr::pull(get(id_application)) %>%
    unique()

  return(sucra)
}
