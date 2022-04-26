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
#' @param n_iter how many iterations used in the JAGS sampler?
#'  (default = 5000)
#' @param n_chains number of chains for the JAGS sampler. (default = 2)
#' @param n_adapt number of iterations discarded for the adaptation phase.
#'  (default = 1000)
#' @param n_burnin number of burnin iterations discarded. (default = 1000)
#' @param max_iter maximum number of iteration (default = 1 million)
#' @param id_section name of the section
#' @param theta_name the name of the application identifier in the JAGS model.
#' (default = application_intercept").
#' @param rank_theta_name the name of the rank of theta in the JAGS model
#' (default = rank_theta).
#' @param voter_name the name of the voter intercept in the JAGS model (default
#' = voter_intercept).
#' @param tau_name name of the tau in the jags model, being the standard error
#' of the application effects (default = tau_application).
#' @param tau_name name of the tau in the jags model, being the standard error
#' of the application effects (default = tau_application).
#' @param tau_voter_name name of the standard error of the voter effect
#' (default = tau_voter).
#' @param tau_section_name name of the standard error of the section effect, if
#' needed (default = NULL).
#' @param sigma_name name of the standard deviation of the full model.
#' (default = sigma)
#' @param rank_pm should the rank based on the posterior mean by computed?
#' default = TRUE
#' @param ordinal_scale dummy variable informing us on whether or not the
#' outcome is on an ordinal scale (default = FALSE)
#' @param point_scale integer informing us on the number of points of the
#' ordinal scale; not needed for continuous scale (default = NULL)
#' @param heterogeneous_residuals dummy variable informing us on whether or not
#' the residuals should be heterogeneous (in this case you have to update the
#' JAGS model too, default = FALSE)
#' @param mcmc_samples if the mcmc sample has already been run. This should be
#' the direct output from get_mcmc_samples. (default = NULL).
#' @param inits_type type of the initial values, default is "random", but if two
#' chains are used, the initial values can also be  "overdispersed"
#' @param initial_values The list of initial values for the jags sampler can be
#' provided directly
#' @param variables_to_sample should the default variables be samples, or will
#' they be "specified" (in names_variables_to_sample), default is "default".
#' @param names_variables_to_sample if variables to sample are specified, write
#' their names here, as a character-vector, default is NULL.
#' @param seed set a seed for the JAGS model (default = 1991)
#' @param quiet if the default model is used this function generates a warning.
#' if quiet = TRUE, this warning is not shown.
#' @param compute_ess Should the effective sample size and the mcmc errors be
#' calculated? (default = FALSE).
#'
#' @import rjags
#' @import dplyr
#' @return the result is a list with the
#'
#' 1) a table with the ranked proposals:
#' id_application is the unique identifier of the proposal/application. rank is
#' the simplistic rank based on the average of the individual votes, avg_grade.
#' er is the expected rank. rank_pm is the rank of the posterior mean and pcer
#' is the percentile based on er.
#'
#' 2) the number of chains (n_chains), the number of adaptive iterations
#' (n_adapt), the number of burnin iterations (n_burnin), and the final number
#' of iterations actually samples (n_iter).
#'
#' 3) the effective sample size (ess) of all relevant parameters and the
#' MCMC error (mcmc_error) of the same parameters.
#'
#' @export
#' @examples
#' data_panel1 <- get_mock_data() %>%
#'          filter(panel == "p1")
#' \dontrun{
#' ER_results <- get_er_from_jags(data = data_panel1,
#'                                id_application = "application",
#'                                id_voter = "voter",
#'                                grade_variable = "num_grade",
#'                                path_to_jags_model = NULL)
#' # OR, by giving an mcmc object into the function
#' mcmc_samples <- get_mcmc_samples(data = data_panel1,
#'                                  id_application = "application",
#'                                  id_voter = "voter",
#'                                  grade_variable = "num_grade")
#' ER_results <- get_er_from_jags(data = data_panel1,
#'                               id_application = "application",
#'                               id_voter = "voter",
#'                               grade_variable = "num_grade",
#'                               mcmc_samples = mcmc_samples)
#' }
get_er_from_jags <-  function(data, id_application,
                              id_voter = NULL,
                              grade_variable,
                              path_to_jags_model = NULL,
                              n_chains = 2, n_iter = 5000,
                              n_burnin = 1000, n_adapt = 1000,
                              max_iter = 1000000,
                              id_section = NULL,
                              theta_name = "application_intercept",
                              voter_name = "voter_intercept",
                              tau_name = "tau_application",
                              tau_voter_name = "tau_voter",
                              tau_section_name = NULL,
                              sigma_name = "sigma",
                              rank_theta_name = "rank_theta",
                              rank_pm = TRUE,
                              ordinal_scale = FALSE,
                              heterogeneous_residuals = FALSE,
                              point_scale = NULL,
                              mcmc_samples = NULL,
                              inits_type = "random",
                              initial_values = NULL,
                              variables_to_sample = "default",
                              names_variables_to_sample = NULL,
                              seed = 1991,
                              quiet = FALSE,
                              compute_ess = FALSE) {

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

  ## 4) If ordinal_scale, do we have a number of points on the scale?
  if (ordinal_scale) {
    if (is.null(point_scale)) stop(paste0("If you want to operate on an ",
                                          "ordinal scale, please specify the ",
                                          "number of points on the scale, in ",
                                          "point_scale."))
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
                                     n_adapt = n_adapt, n_burnin = n_burnin,
                                     max_iter = max_iter,
                                     id_section = id_section,
                                     theta_name = theta_name,
                                     rank_theta_name = rank_theta_name,
                                     voter_name = voter_name,
                                     tau_name = tau_name,
                                     tau_voter_name = tau_voter_name,
                                     tau_section_name = tau_section_name,
                                     sigma_name = sigma_name,
                                     ordinal_scale = ordinal_scale,
                                     point_scale = point_scale,
                                     heterogeneous_residuals =
                                       heterogeneous_residuals,
                                     inits_type = inits_type,
                                     initial_values = initial_values,
                                     variables_to_sample = variables_to_sample,
                                     names_variables_to_sample =
                                       names_variables_to_sample,
                                     seed = seed, quiet = quiet,
                                     compute_ess = compute_ess)
  } else {
    if (length(mcmc_samples) != 8) {
      stop(paste0("Make sure that the object given to mcmc_samples is an ",
                  "object that was build with get_mcmc_samples()."))
    }
  }
  # Get information out of the mcmc_samples object:
  final_n_chains <- mcmc_samples$n_chains
  final_n_adapt <- mcmc_samples$n_adapt
  final_n_burnin <- mcmc_samples$n_burnin
  final_n_iter <- mcmc_samples$n_iter
  ess <- mcmc_samples$ess
  mcmc_error <- mcmc_samples$mcmc_error
  if (is.list(mcmc_samples$samples)) {
    mcmc_samples <- do.call(rbind, mcmc_samples$samples)
  } else mcmc_samples <- mcmc_samples$samples


  # Extract theta samples for the ranking based on the posterior means:
  if (rank_pm) {
    colnames_theta <- paste0(theta_name, "[", seq_len(n_application), "]")
    # mcmc_samples_thetas <- mcmc_samples[, colnames_theta]
    mcmc_samples_thetas <- mcmc_samples[, colnames_theta]
  }

  # Samples of the ranks of the thetas:
  colnames_ranks <- paste0(rank_theta_name, "[", seq_len(n_application), "]")
  # mcmc_samples_ranks <- mcmc_samples[, colnames_ranks]
  mcmc_samples_ranks <- mcmc_samples[, colnames_ranks]

  # The expected ranks, as the posterior expectation of the ranks:
  ers <- colMeans(mcmc_samples_ranks)

  # The results matrix
  if (rank_pm){
    results_rank <-
      tibble(id_application = data %>%
               dplyr::pull(get(id_application)) %>%
               unique(),
             # The posterior means of the thetas and the ers
             pm = colMeans(mcmc_samples_thetas),
             er = ers) %>%
      mutate(rank_pm = rank(-(overall_mean + .data$pm))) %>%
              # Rank based on the pm
      select(-.data$pm)
  } else {
    results_rank <-
      tibble(id_application = data %>%
               dplyr::pull(get(id_application)) %>%
               unique(),
             # Adding the er to results matrix
             er = ers)
  }

  rankings_all <- data %>%
    mutate(num_grade = get(grade_variable)) %>%
    group_by(!!as.name(id_application)) %>%
    # Computation of the average grade
    summarise(avg_grade = mean(.data$num_grade, na.rm = TRUE)) %>%
    mutate(rank = rank(-.data$avg_grade), # Rank based on the average grade
           # `Rename` the application variable
           id_application = get(id_application)) %>%
    select(.data$id_application, .data$rank,
           .data$avg_grade) %>%
    left_join(results_rank,
              # Joining the posterior mean and ER results
              by = "id_application") %>%
    # Computation of the percentiles based on ER
    mutate(pcer = 100 * (.data$er - 0.5) / n_application)

  return(list(rankings = rankings_all,
              n_chains = final_n_chains,
              n_adapt = final_n_adapt,
              n_burnin = final_n_burnin,
              n_iter = final_n_iter,
              ess = ess,
              mcmc_error = mcmc_error))
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
#' @param n_iter how many iterations used in the JAGS sampler?
#'  (default = 5000)
#' @param n_chains number of chains for the JAGS sampler. (default = 2)
#' @param n_adapt number of iterations discarded for the adaptation phase.
#'  (default = 1000)
#' @param n_burnin number of burnin iterations discarded. (default = 1000)
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
#' @param rank_theta_name the name of the rank of theta in the JAGS model
#' (default = rank_theta)
#' @param ordinal_scale dummy variable informing us on whether or not the
#' outcome is on an ordinal scale (default = FALSE)
#' @param point_scale integer informing us on the number of points of the
#' ordinal scale; not needed for continuous scale (default = NULL)
#' @param heterogeneous_residuals dummy variable informing us on whether or not
#' the residuals should be heterogeneous (in this case you have to update the
#' JAGS model too, default = FALSE)
#' @param mcmc_samples if the mcmc sample has already been run (default = NULL).
#' @param inits_type type of the initial values, default is "random", but if two
#' chains are used, the initial values can also be  "overdispersed"
#' @param initial_values The list of initial values for the jags sampler can be
#' provided directly
#' @param seed set a seed for the JAGS model (default = 1991)
#' @param quiet if the default model is used this function generates a warning.
#' if quiet = TRUE, this warning is not shown
#' @import rjags
#' @return the result is a names vector with the SUCRA of all applications
#' @examples
#' data_panel1 <- get_mock_data() %>%
#'          filter(panel == "p1")
#' \dontrun{
#' SUCRA_results <- get_sucra(data = data_panel1,
#'                            id_application = "application",
#'                            id_voter = "voter", grade_variable = "num_grade")
#'                            }
#' @export
get_sucra <- function(data, id_application, id_voter, grade_variable,
                      path_to_jags_model = NULL,
                      n_chains = 3, n_iter = 5000,
                      n_burnin = 1000, n_adapt = 1000,
                      id_section = NULL,
                      theta_name = "application_intercept",
                      tau_name = "tau_application",
                      tau_voter_name = "tau_voter",
                      tau_section_name = NULL,
                      sigma_name = "sigma",
                      ordinal_scale = FALSE,
                      heterogeneous_residuals = FALSE,
                      point_scale = NULL,
                      rank_theta_name = "rank_theta",
                      inits_type = "random",
                      initial_values = NULL,
                      mcmc_samples = NULL,
                      seed = 1991, quiet = FALSE) {

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
    mcmc_samples <- get_mcmc_samples(data = data,
                                     id_application = id_application,
                                     id_voter = id_voter,
                                     grade_variable = grade_variable,
                                     path_to_jags_model = path_to_jags_model,
                                     n_chains = n_chains, n_iter = n_iter,
                                     n_adapt = n_adapt, n_burnin = n_burnin,
                                     id_section = id_section,
                                     theta_name = theta_name,
                                     tau_name = tau_name,
                                     tau_voter_name = tau_voter_name,
                                     tau_section_name = tau_section_name,
                                     sigma_name = sigma_name,
                                     rank_theta_name = rank_theta_name,
                                     ordinal_scale = ordinal_scale,
                                     point_scale = point_scale,
                                     heterogeneous_residuals =
                                       heterogeneous_residuals,
                                     inits_type = inits_type,
                                     initial_values = initial_values,
                                     seed = seed, quiet = quiet)
  } else {
    if (length(mcmc_samples) != 8) {
      stop(paste0("Make sure that the object given to mcmc_samples is an ",
                  "object that was build with get_mcmc_samples()."))
    }
  }
  # Get information out of the mcmc_samples object:
  final_n_chains <- mcmc_samples$n_chains
  final_n_adapt <- mcmc_samples$n_adapt
  final_n_burnin <- mcmc_samples$n_burnin
  final_n_iter <- mcmc_samples$n_iter
  if (is.list(mcmc_samples$samples)) {
    mcmc_samples <- do.call(rbind, mcmc_samples$samples)
  } else mcmc_samples <- mcmc_samples$samples

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

  return(list(sucra = sucra,
              n_chains = final_n_chains,
              n_adapt = final_n_adapt,
              n_burnin = final_n_burnin,
              n_iter = final_n_iter))
}
