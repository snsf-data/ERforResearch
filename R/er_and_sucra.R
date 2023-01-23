# ----------------------------------
# Expected Rank and SUCRA quantities
# ----------------------------------



#' The Expected Rank from a JAGS model
#'
#' This function computes the ER via JAGS
#' @param data data frame, in long format, with all needed variables as
#' specified in the JAGS model defined in the text file with path given in
#' `path_to_jags_model`.
#' @param path_to_jags_model the path to text file including the JAGS model
#' definition. By default `= NULL`, and the function will use a default model as
#' implemented in the package; in `get_default_jags_model()`.
#' @param n_iter how many iterations should be used in the JAGS sampler? This is
#' the same as `sample` in the `runjags::run.jags()` function. It is set to
#' `10000` by default.
#' @param mcmc_samples if the mcmc sample has already been run. This should be
#' the direct output from the `get_mcmc_samples()`. By default, it is set to
#' `NULL` and the sampler will be run. If mcmc samples are provided here, all
#' further sampling information below, e.g. number of chains and
#' iterations will be disgarded.
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
#' @param rank_pm should the rank based on the posterior mean by computed and
#' presented in the Figure? By default this parameter is set to `TRUE`.
#' @param assessor_name the name of the assessor intercept in the JAGS model.
#' The default that also goes with the default JAGS model build in the package
#' is `assessor_intercept`.
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
#' @param inits_type the type of the initial values. By default the initial
#' values are randomly selected, i.e. `inits_type = "random"`.
#' Alternatively, if four chains are used, the initial values can also be
#' `"overdispersed"`.
#' @param initial_values The list of initial values for the jags sampler can be
#' provided directly. Otherwise `get_inits_overdispersed_four_chains` for the
#' overdispersed version is used, or they are randomly selected. Always using a
#' seed to ensure computational reproducibility.
#' @param names_variables_to_sample if variables to sample are specified, write
#' their names here, as a character-vector, default is NULL.
#' @param seed the seed for the JAGS model (default = `1991`). This seed will
#' generate the seeds for the JAGS samplers, which ensures reproducibility; see
#' also Details.
#' @param quiet if the default model is used this function generates a warning.
#' if `quiet = TRUE`, the warning is not shown.
#' @param rhat_threshold the threshold for rhat to decide whether or not the
#' chains converged. Gelman suggested 1.1, but the smaller the better. Hence
#' this functions threshold is set to `1.01` by default.
#' @param runjags_method the method with which to call JAGS (from
#' `runjags::run.jags()` with the default being set to `parallel`).
#'
#' @import runjags
#' @importFrom tibble tibble
#' @return the result is a list with the
#'
#' 1) a table with the ranked proposals:
#' id_proposal is the unique identifier of the proposal. rank is
#' the simplistic rank based on the average of the individual votes, avg_grade.
#' er is the expected rank. rank_pm is the rank of the posterior mean and pcer
#' is the percentile based on er.
#'
#' 2) the number of chains (n_chains), the number of adaptive iterations
#' (n_adapt), the number of burnin iterations (n_burnin), and the final number
#' of iterations actually samples (n_iter).
#'
#' 3) the MCMC summary and the convergence status.
#'
#' @export
#' @examples
#' data_panel1 <- get_mock_data() %>%
#'          filter(panel == "p1")
#' \dontrun{
#' ER_results <- get_er_from_jags(data = data_panel1,
#'                                id_proposal = "proposal",
#'                                id_assessor = "assessor",
#'                                grade_variable = "num_grade",
#'                                path_to_jags_model = NULL)
#' # OR, by giving an mcmc object into the function
#' mcmc_samples <- get_mcmc_samples(data = data_panel1,
#'                                  id_proposal = "proposal",
#'                                  id_assessor = "assessor",
#'                                  grade_variable = "num_grade")
#' ER_results <- get_er_from_jags(data = data_panel1,
#'                               id_proposal = "proposal",
#'                               id_assessor = "assessor",
#'                               grade_variable = "num_grade",
#'                               mcmc_samples = mcmc_samples)
#' }
get_er_from_jags <-  function(data, id_proposal,
                              id_assessor = NULL,
                              grade_variable,
                              path_to_jags_model = NULL,
                              n_chains = 4, n_iter = 10000,
                              n_burnin = 4000, n_adapt = 1000,
                              max_iter = 1000000,
                              id_panel = NULL,
                              theta_name = "proposal_intercept",
                              assessor_name = "assessor_intercept",
                              tau_name_proposal = "tau_proposal",
                              tau_name_assessor = "tau_assessor",
                              tau_name_panel = NULL,
                              sigma_name = "sigma",
                              rank_theta_name = "rank_theta",
                              rank_pm = TRUE,
                              ordinal_scale = FALSE,
                              heterogeneous_residuals = FALSE,
                              point_scale = NULL,
                              mcmc_samples = NULL,
                              inits_type = "random",
                              initial_values = NULL,
                              names_variables_to_sample = NULL,
                              seed = 1991,
                              quiet = FALSE,
                              rhat_threshold = 1.01,
                              runjags_method = "parallel") {

  # Tests:
  ## 1) If no mcmc samples are provided, name of the voter variables is needed:
  if (is.null(id_assessor) & is.null(mcmc_samples)) {
    stop(paste0("Provide id_assessor to compute MCMC samples. If you seperately",
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
  presence_of_variables <- !c(id_proposal, id_assessor, grade_variable,
                              id_panel) %in% names(data)
  if (any(presence_of_variables)) {
    stop(paste0(c(id_proposal, id_assessor, grade_variable, id_panel)[
      which(presence_of_variables)], " needed, but not present in dataset."))
  }

  ## 4) If ordinal_scale, do we have a number of points on the scale?
  if (ordinal_scale) {
    if (is.null(point_scale)) stop(paste0("If you want to operate on an ",
                                          "ordinal scale, please specify the ",
                                          "number of points on the scale, in ",
                                          "point_scale."))
  }

  # Number of proposals and overall mean:
  n_proposal <- data %>%
    dplyr::pull(get(id_proposal)) %>%
    unique() %>%
    length()
  overall_mean <- data %>%
    group_by(get(id_proposal)) %>%
    mutate(num_grade = get(grade_variable)) %>%
    summarise(av = mean(.data$num_grade, na.rm = TRUE)) %>%
    dplyr::pull(.data$av) %>%
    mean()

  # If not MCMC samples are provided, they are computed here:
  if (is.null(mcmc_samples)) {
    mcmc_samples <- get_mcmc_samples(data = data,
                                     id_proposal = id_proposal,
                                     id_assessor = id_assessor,
                                     grade_variable = grade_variable,
                                     path_to_jags_model = path_to_jags_model,
                                     n_chains = n_chains, n_iter = n_iter,
                                     n_adapt = n_adapt, n_burnin = n_burnin,
                                     max_iter = max_iter,
                                     id_panel = id_panel,
                                     theta_name = theta_name,
                                     rank_theta_name = rank_theta_name,
                                     assessor_name = assessor_name,
                                     tau_name_proposal = tau_name_proposal,
                                     tau_name_assessor = tau_name_assessor,
                                     tau_name_panel = tau_name_panel,
                                     sigma_name = sigma_name,
                                     ordinal_scale = ordinal_scale,
                                     point_scale = point_scale,
                                     heterogeneous_residuals =
                                       heterogeneous_residuals,
                                     inits_type = inits_type,
                                     initial_values = initial_values,
                                     names_variables_to_sample =
                                       names_variables_to_sample,
                                     seed = seed, quiet = quiet,
                                     rhat_threshold = rhat_threshold,
                                     runjags_method = runjags_method)
  } else {
    if (length(mcmc_samples) != 7) {
      stop(paste0("Make sure that the object given to mcmc_samples is an ",
                  "object that was build with get_mcmc_samples()."))
    }
  }
  # Get information out of the mcmc_samples object:
  final_n_chains <- mcmc_samples$n_chains
  final_n_adapt <- mcmc_samples$n_adapt
  final_n_burnin <- mcmc_samples$n_burnin
  final_n_iter <- mcmc_samples$n_iter
  mcmc_summary <- mcmc_samples$summary
  conv_status <- mcmc_samples$conv_status
  if (is.list(mcmc_samples$samples)) {
    mcmc_samples <- do.call(rbind, mcmc_samples$samples$mcmc)
  } else mcmc_samples <- mcmc_samples$samples


  # Extract theta samples for the ranking based on the posterior means:
  if (rank_pm) {
    colnames_theta <- paste0(theta_name, "[", seq_len(n_proposal), "]")
    # mcmc_samples_thetas <- mcmc_samples[, colnames_theta]
    mcmc_samples_thetas <- mcmc_samples[, colnames_theta]
  }

  # Samples of the ranks of the thetas:
  colnames_ranks <- paste0(rank_theta_name, "[", seq_len(n_proposal), "]")
  # mcmc_samples_ranks <- mcmc_samples[, colnames_ranks]
  mcmc_samples_ranks <- mcmc_samples[, colnames_ranks]

  # The expected ranks, as the posterior expectation of the ranks:
  ers <- colMeans(mcmc_samples_ranks)

  # The results matrix
  if (rank_pm){
    results_rank <-
      tibble(id_proposal = data %>%
               dplyr::pull(get(id_proposal)) %>%
               unique(),
             # The posterior means of the thetas and the ers
             pm = colMeans(mcmc_samples_thetas),
             er = ers) %>%
      mutate(rank_pm = rank(-(overall_mean + .data$pm))) %>%
              # Rank based on the pm
      select(-.data$pm)
  } else {
    results_rank <-
      tibble(id_proposal = data %>%
               dplyr::pull(get(id_proposal)) %>%
               unique(),
             # Adding the er to results matrix
             er = ers)
  }

  rankings_all <- data %>%
    mutate(num_grade = get(grade_variable)) %>%
    group_by(!!as.name(id_proposal)) %>%
    # Computation of the average grade
    summarise(avg_grade = mean(.data$num_grade, na.rm = TRUE)) %>%
    mutate(rank = rank(-.data$avg_grade), # Rank based on the average grade
           # `Rename` the proposal variable
           id_proposal = get(id_proposal)) %>%
    select(.data$id_proposal, .data$rank,
           .data$avg_grade) %>%
    left_join(results_rank,
              # Joining the posterior mean and ER results
              by = "id_proposal") %>%
    # Computation of the percentiles based on ER
    mutate(pcer = 100 * (.data$er - 0.5) / n_proposal)

  return(list(rankings = rankings_all,
              n_chains = final_n_chains,
              n_adapt = final_n_adapt,
              n_burnin = final_n_burnin,
              n_iter = final_n_iter,
              mcmc_summary = mcmc_summary,
              conv_status = conv_status))
  }




#' The Surface Under the Cumulative RAnking (SUCRA)
#'
#' This function computes the SUCRA.
#'
#' @param data data frame, in long format, with all needed variables as
#' specified in the JAGS model defined in the text file with path given in
#' `path_to_jags_model`.
#' @param path_to_jags_model the path to text file including the JAGS model
#' definition. By default `= NULL`, and the function will use a default model as
#' implemented in the package; in `get_default_jags_model()`.
#' @param mcmc_samples if the mcmc sample has already been run. This should be
#' the direct output from the `get_mcmc_samples()`. By default, it is set to
#' `NULL` and the sampler will be run. If mcmc samples are provided here, all
#' further sampling information below, e.g. number of chains and
#' iterations will be disgarded.
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
#'
#' @import runjags
#' @export
#'
#' @return the result is a names vector with the SUCRA of all proposals
#'
#' @examples
#' data_panel1 <- get_mock_data() %>%
#'          filter(panel == "p1")
#' \dontrun{
#' SUCRA_results <- get_sucra(data = data_panel1,
#'                            id_proposal = "proposal",
#'                            id_assessor = "assessor",
#'                             grade_variable = "num_grade")
#'                            }
get_sucra <- function(data, id_proposal, id_assessor,
                      grade_variable,
                      path_to_jags_model = NULL,
                      mcmc_samples = NULL,
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
                      ordinal_scale = FALSE,
                      point_scale = NULL,
                      heterogeneous_residuals = FALSE,
                      seed = 1991, quiet = FALSE,
                      dont_bind = FALSE,
                      inits_type = "random", # or "overdispersed"
                      names_variables_to_sample = NULL,
                      initial_values = NULL,
                      rhat_threshold = 1.01,
                      runjags_method = "parallel") {

  # Tests:
  ## 1) If no mcmc samples are provided, name of the voter variables is needed:
  if (is.null(id_assessor) & is.null(mcmc_samples)) {
    stop(paste0("Provide id_assessor to compute MCMC samples. If you seperately",
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
  presence_of_variables <- !c(id_proposal, id_assessor, grade_variable,
                              id_panel) %in% names(data)
  if (any(presence_of_variables)) {
    stop(paste0(c(id_proposal, id_assessor, grade_variable, id_panel)[
      which(presence_of_variables)], " needed, but not present in dataset."))
  }

  # Number of proposals and overall mean:
  n_proposal <- data %>%
    dplyr::pull(get(id_proposal)) %>%
    unique() %>%
    length()
  overall_mean <- data %>%
    group_by(get(id_proposal)) %>%
    summarise(av = mean(get(grade_variable), na.rm = TRUE)) %>%
    dplyr::pull(.data$av) %>%
    mean()

  # If not MCMC samples are provided, they are computed here:
  if (is.null(mcmc_samples)) {
    mcmc_samples <- get_mcmc_samples(data = data,
                                     id_proposal = id_proposal,
                                     id_assessor = id_assessor,
                                     grade_variable = grade_variable,
                                     path_to_jags_model = path_to_jags_model,
                                     n_chains = n_chains, n_iter = n_iter,
                                     n_adapt = n_adapt, n_burnin = n_burnin,
                                     max_iter = max_iter,
                                     id_panel = id_panel,
                                     theta_name = theta_name,
                                     rank_theta_name = rank_theta_name,
                                     assessor_name = assessor_name,
                                     tau_name_proposal = tau_name_proposal,
                                     tau_name_assessor = tau_name_assessor,
                                     tau_name_panel = tau_name_panel,
                                     sigma_name = sigma_name,
                                     ordinal_scale = ordinal_scale,
                                     point_scale = point_scale,
                                     heterogeneous_residuals =
                                       heterogeneous_residuals,
                                     inits_type = inits_type,
                                     initial_values = initial_values,
                                     names_variables_to_sample =
                                       names_variables_to_sample,
                                     seed = seed, quiet = quiet,
                                     rhat_threshold = rhat_threshold,
                                     runjags_method = runjags_method)
  } else {
    if (length(mcmc_samples) != 7) {
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
    mcmc_samples <- do.call(rbind, mcmc_samples$samples$mcmc)
  } else mcmc_samples <- mcmc_samples$samples$mcmc

  # Extract samples of ranks of the thetas:
  colnames_ranks <- paste0(rank_theta_name, "[", seq_len(n_proposal), "]")
  mcmc_samples_ranks <- mcmc_samples[, colnames_ranks]

  # Calculate the P(j = b) for the SUCRA:
  p_j_b <- matrix(NA, nrow = n_proposal, ncol = n_proposal)

  for (i in seq_len(n_proposal)) {
    for (j in seq_len(n_proposal)) {
      p_j_b[i, j] <- mean(mcmc_samples_ranks[, i] == j)
    }
  }
  # Compute the SUCRA
  sucra <- sapply(seq_len(nrow(p_j_b)), function(i) {
    mean(cumsum(p_j_b[i, -nrow(p_j_b)]))
  })

  # Add the names of the proposals as names of the sucra-vector
  names(sucra) <- data %>%
    dplyr::pull(get(id_proposal)) %>%
    unique()

  return(list(sucra = sucra,
              n_chains = final_n_chains,
              n_adapt = final_n_adapt,
              n_burnin = final_n_burnin,
              n_iter = final_n_iter))
}
