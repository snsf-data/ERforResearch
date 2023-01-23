# ------------------
# Plotting functions
# ------------------


#' The Plot of the Expected Rank from a JAGS model
#'
#' This function plot the results form the JAGS-ER
#' @param er_results the resulting object from the `get_er_from_jags()` function
#' @param how_many_fundable number of proposals that can be funded?
#' (default = NULL) If Null, the graph will be black and white only
#' @param title beginning of the plot title (default = " ")
#' @param id_proposal the name of the variable giving the distinct
#' application identifiers. (default = id_proposal)
#' @param draw_funding_line should the funding line be drawn? (default = TRUE)
#' @param x_expand make funding line longer on both sides (default = .3)
#' @param ordering_increasing should the order of the rank be increasing, from
#' up to dowm: from rank 1 on top to rank n on the bottom (default = TRUE)
#' @param result_show boolean telling us whether the application number/id
#' should be added next to the ER points (default = FALSE).
#' @param easy_numbering should the ER results just be numbered?
#' (default = TRUE)
#' @param no_pm_available has the posterior mean not been computed? (default =
#' FALSE, e.g. it has been computed and is in the object)
#' @param pt_size size of points (default = 1)
#' @param line_size size of the lines (default = 0.5)
#' @param grep_size the size of the labels on the points (default = .25).
#' @param title beginning of the plot title (default = " ").
#' @param nudge_x element of ggrepel.
#' @param colors a color pallette that can be changes.
#' @param padding_right for ggrepel
#' @param padding_left for ggrepel
#' @param alpha_line ...
#' @param alpha_pt ...
#' @param line_type_fl ... fl = funding line
#' @param alpha_fl ...
#' @param color_fl ...
#' @param size_fl ...
#' @param min_segment_length ...
#' @param segment_curvature ...
#' @param segment_angle ...
#' @param segment_ncp ...
#' @param segment_size ...
#' @param hjust_y_axis ...
#'
#' @return the result is a plot
#'
#' @importFrom ggrepel geom_text_repel
#' @importFrom tidyr gather
#' @import ggplot2
#'
#' @examples
#' dat <- get_mock_data() %>%
#' filter(panel == "p1")
#' \dontrun{
#' ER_results <- get_er_from_jags(data = dat,
#'                   id_proposal = "proposal",
#'                   id_assessor = "assessor",
#'                   grade_variable = "num_grade",
#'                   n_chains = 2, n_iter = 1000,
#'                   n_burnin = 1000)
#' plotting_er_results(ER_results, title = "Panel 1", how_many_fundable = 5)
#' }
#' @export
plotting_er_results <- function(er_results,
                                id_proposal = "id_proposal",
                                how_many_fundable = NULL,
                                title = "",
                                ordering_increasing = TRUE,
                                draw_funding_line = TRUE,
                                result_show = TRUE,
                                easy_numbering = TRUE,
                                no_pm_available = FALSE,
                                colors = NULL,
                                pt_size = 1, line_size = .5,
                                alpha_line = 1, alpha_pt = 1,
                                line_type_fl = "longdash",
                                x_expand = .6,
                                alpha_fl = .5,  color_fl = "darkgray",
                                size_fl = 2,
                                padding_right = .3,
                                padding_left = 0.1,
                                grep_size = 2,
                                nudge_x = .5,
                                min_segment_length = 0,
                                segment_curvature = -0.1,
                                segment_angle = 20,
                                segment_ncp = 3,
                                segment_size = 0.15,
                                hjust_y_axis = 1.5) {

  # Colorblind palette
  if (is.null(colors)) colors <- c("#E69F00", "#56B4E9",
                                   "#009E73", "#0072B2",
                                   "#D55E00", "#CC79A7")

  # Gather the data needed for the plot:
  # -----------------------------------
  # If we know how many more are fundable, we have to add a variable for the
  # coloring.

  if (!is.null(how_many_fundable)){
    dat_for_plot <- er_results$rankings %>%
      arrange(.data$er) %>%
      mutate(funded = c(rep("Funded", how_many_fundable),
                        rep("Rejected", n() - how_many_fundable))) %>%
      mutate(numbering = 1:n())
  } else dat_for_plot <- er_results$rankings %>%
      arrange(.data$er) %>%
      mutate(numbering = 1:n())

  if (!no_pm_available){
    dat_for_plot <- dat_for_plot %>%
      gather("rank", "rank_pm", "er",
             key = "which_rank", value = "rank") %>%
      mutate(which_rank = case_when(.data$which_rank == "rank" ~ "Fixed",
                                    .data$which_rank == "rank_pm" ~
                                      "Posterior Mean",
                                    .data$which_rank == "er" ~ "ER"),
             which_rank = factor(.data$which_rank,
                                 levels = c("Fixed",
                                            "Posterior Mean", "ER")))
  } else {
    dat_for_plot <- dat_for_plot %>%
      gather("rank", "er",
             key = "which_rank", value = "rank")  %>%
      mutate(which_rank = case_when(.data$which_rank == "rank" ~ "Fixed",
                                    .data$which_rank == "er" ~ "ER"),
             which_rank = factor(.data$which_rank,
                                 levels = c("Fixed", "ER")))
  }
  dat_for_plot <- dat_for_plot %>%
    rename(proposal_number = eval(id_proposal))

  start_plot <- dat_for_plot %>%
    ggplot(aes(x = .data$which_rank, y = .data$rank,
               group = .data$proposal_number))

  if (!is.null(how_many_fundable)) {
    start_plot <- dat_for_plot %>%
      ggplot(aes(x = .data$which_rank, y = .data$rank,
                 group = .data$proposal_number, color = .data$funded))
  }

  if (result_show) {
    start_plot <- start_plot +
      geom_text_repel(aes(label = ifelse(.data$which_rank == "ER" &
                                           !easy_numbering,
                                         .data$proposal_number,
                                         ifelse(.data$which_rank == "ER" &
                                                  easy_numbering,
                                                .data$numbering, ""))),
                      size = grep_size, nudge_x = nudge_x,
                      direction = "y", show.legend = FALSE,
                      min.segment.length = min_segment_length,
                      segment.curvature = segment_curvature,
                      segment.ncp = segment_ncp,
                      segment.angle = segment_angle,
                      segment.size = segment_size)
  }

  middle_plot <- start_plot +
    geom_point(size = pt_size, alpha = alpha_pt) +
    geom_line(size = line_size, alpha = alpha_line) +
    scale_color_manual(values = colors) +
    theme_minimal() +
    scale_x_discrete(expand = expansion(mult = c(padding_left,
                                                 padding_right))) +
    labs(x = "Method", y = "Ranks of Proposals") +
    theme(legend.title = element_blank())

  if (draw_funding_line) {
    # Get the last fundable ER and add .5:
    er_fl <- sort(er_results$rankings$er)[how_many_fundable] + .1
    middle_plot <- middle_plot +
      geom_line(data =
                  tibble(which_rank = c(0 + x_expand, 2, 4 - x_expand),
                         rank = c(rep(how_many_fundable + .1, 2), er_fl),
                         proposal_number = rep(1,3)),
                color = color_fl, linetype = line_type_fl, alpha = alpha_fl,
                size = size_fl)
  }

  if (ordering_increasing) {
    middle_plot <- middle_plot +
      scale_y_continuous(trans = "reverse",
                         breaks = unique(er_results$rankings$rank),
                         labels = unique(er_results$rankings$rank),
                         guide = guide_axis(check.overlap = TRUE))
  }

  finish_plot <- middle_plot +
    ggtitle(title)


  finish_plot +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(hjust = hjust_y_axis))
}


#' Rankogram
#'
#' This function produces a rankogram
#'
#' @param data data frame, in long format, with all needed variables as
#' specified in the JAGS model defined in the text file with path given in
#' `path_to_jags_model`.
#' @param cumulative_rank_prob should the cumulative ranking probabilities be
#' represented instead
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
#' @param mcmc_samples if the mcmc sample has already been run. This should be
#' the direct output from the `get_mcmc_samples()`. By default, it is set to
#' `NULL` and the sampler will be run. If mcmc samples are provided here, all
#' further sampling information below, e.g. number of chains and
#' iterations will be disgarded.
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
#' @importFrom tidyr gather
#' @importFrom dplyr as_tibble rename
#' @importFrom utils head
#'
#' @return the result is a plot of the rankogram (or cumulative ranking
#' probabilities)
#'
#' @export
plot_rankogram <- function(data,
                           cumulative_rank_prob = FALSE,
                           id_proposal, id_assessor,
                           mcmc_samples = NULL,
                           grade_variable = "num_grade",
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

  n_proposal <- data %>%
    dplyr::pull(get(id_proposal)) %>%
    unique() %>%
    length()

  overall_mean <- data %>%
    mutate(num_grade = get(grade_variable)) %>%
    group_by(get(id_proposal)) %>%
    summarise(av = mean(.data$num_grade, na.rm = TRUE)) %>%
    dplyr::pull(.data$av) %>%
    mean()

  # If no MCMC samples are provided, they are computed here:
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

  colnames_rank_theta <- paste0(rank_theta_name, "[",
                                seq_len(n_proposal), "]")
  if (is.list(mcmc_samples$samples)) {
    mcmc_samples_rank_thetas <-
      do.call(rbind, mcmc_samples$samples$mcmc)[, colnames_rank_theta]
  } else {
    mcmc_samples_rank_thetas <-
      mcmc_samples$samples[, colnames_rank_theta]
  }

  # Calculate the P(j = b) for the SUCRA:
  p_j_b <- matrix(NA, nrow = n_proposal, ncol = n_proposal)

  for (i in seq_len(n_proposal)) {
    for (j in seq_len(n_proposal)) {
      p_j_b[i, j] <- mean(mcmc_samples_rank_thetas[, i] == j)
    }
  }
  # Compute the SUCRA
  sucra <- sapply(seq_len(nrow(p_j_b)), function(i) {
    mean(cumsum(p_j_b[i, -nrow(p_j_b)]))
  })

  colnames(p_j_b) <- seq_len(ncol(p_j_b))
  rownames(p_j_b) <- data %>%
    dplyr::pull(get(id_proposal)) %>%
    unique()

  if (!cumulative_rank_prob) {
    p_j_b %>%
      as_tibble(rownames = "application") %>%
      gather("rank", "prob", paste0(1):paste0(ncol(p_j_b))) %>%
      mutate(rank = as.numeric(.data$rank)) %>%
      ggplot(aes(x = .data$rank, y = .data$prob)) +
      geom_line() +
      labs(x = "Rank", y = "Probability") +
      theme_minimal() +
      facet_wrap(~ .data$application)
  } else {
    sucra <- sapply(seq_len(nrow(p_j_b)), function(i) {
      mean(cumsum(p_j_b[i, -nrow(p_j_b)]))
    })
    names(sucra) <- data %>%
      dplyr::pull(get(id_proposal)) %>%
      unique()

    p_j_b %>%
      as_tibble(rownames = "application") %>%
      gather("rank", "prob", paste0(1):paste0(ncol(p_j_b))) %>%
      mutate(rank = as.numeric(.data$rank)) %>%
      group_by(.data$application) %>%
      arrange(rank) %>%
      mutate(cum_prob = cumsum(.data$prob),
             sucra = sucra[head(.data$application, 1)]) %>%
      ungroup() %>%
      ggplot(aes(x = .data$rank, y = .data$cum_prob)) +
      geom_line() +
      labs(x = "Rank", y = "Cumulative probability") +
      geom_text(aes(x = n_proposal - 3, y = .5, label =
                      paste0("SUCRA: ", round(sucra, 2))),
                size = 2) +
      theme_minimal() +
      facet_wrap(~ .data$application)
  }
}

#' Assessor behavior distributions
#'
#' This function plots the distibutions of the average voter behavior
#' (the $mu_j$'s in the model). Make sure that when using the get_mcmc_samples()
#' function, the parameter for the average voter behavior (default nu) is
#' sampled from the JAGS model.
#'
#' @param get_mcmc_samples_result the mcmc samples
#' @param n_assessors number of voters in the panel / call
#' @param name_mean the name of the parameter estimating the average voter
#' behavior (default = "nu").
#' @param names_assessors names of the voters to be written on the y-axis
#' ticks (default = "voter").
#' @param title title of the plot (default = NULL, no title).
#' @param xlim_min minimum of the x-axis (default = -1).
#' @param xlim_max maximum of the x-axis (default = 1).
#' @param scale for the `geom_density_ridges_gradient()` (default = 1.75).
#' @return the result is a plot of the posterior distributions of the average
#' voter behaviors.
#'
#' @importFrom ggridges geom_density_ridges_gradient
#' @importFrom dplyr ungroup
#' @importFrom stats median
#'
#' @export
assessor_behavior_distribution <- function(get_mcmc_samples_result, n_assessors,
                                           name_mean = "nu",
                                           names_assessors = "voter",
                                           title = NULL, xlim_min = -1,
                                           xlim_max = 1,
                                           scale = 1.75){

  x <- NULL


  if (is.list(get_mcmc_samples_result$samples)) {
    samples <- do.call(rbind, get_mcmc_samples_result$samples$mcmc)
  } else samples <- get_mcmc_samples_result$samples

  assessor_behavior_colnames <- paste0(name_mean, "[", seq_len(n_assessors), "]")
  assessor_behavior_samples <- samples[, assessor_behavior_colnames]


  colnames(assessor_behavior_samples) <- paste0(names_assessors, " ",
                                             seq_len(n_assessors))

  assessor_behavior_sample_for_plot <- assessor_behavior_samples %>%
    as_tibble() %>%
    pivot_longer(cols = starts_with(names_assessors),
                 names_to = "Referee", values_to = "weight") %>%
    group_by(.data$Referee) %>%
    mutate(median = median(.data$weight)) %>%
    ungroup() %>%
    arrange(.data$median)
  assessor_behavior_sample_for_plot %>%
    mutate(Referee = factor(.data$Referee,
                            levels = assessor_behavior_sample_for_plot %>%
                              select(.data$Referee, .data$median) %>%
                              distinct() %>%
                              dplyr::pull(.data$Referee))) %>%
    ggplot(aes(x = .data$weight, y = .data$Referee, fill = stat(x))) +
    geom_density_ridges_gradient(scale = scale, rel_min_height = 0.02,
                                 show.legend = FALSE,
                                 gradient_lwd = 0) +
    xlim(xlim_min, xlim_max) +
    labs(title = title) +
    labs(x = "", y = "") +
    scale_fill_viridis_c(name = paste(names_assessors, "Effect"), option = "C") +
    geom_vline(xintercept = 0) +
    theme_minimal() +
    theme(panel.grid.major.y = element_line(size = 0.25, linetype = "solid",
                                            color = "darkgray"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          title = element_text(size = 7),
          axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 6))
}


#' Expected Ranks with Credible intervals
#'
#' This function plots the expected ranks of all proposals together with
#' their xy% credible intervals.
#'
#' @param get_mcmc_samples_result the mcmc samples
#' @param n_proposals number of proposals in the panel / call
#' @param name_er_or_theta the name of the parameter estimating the ranks (or
#' the proposal effect) in the Bayesian Hierarchical model
#' (default = "rank_thetta")
#' @param er if TRUE then, the ER are plotted, otherwise the thetas (the
#' proposal effects) are plotted. (default = TRUE)
#' @param title title of the plot (default = NULL, no title).
#' @param names_proposals names of the proposals to be written on the x-axis
#' ticks. Note that the ordering has to be the same as the one they are given to
#' the `get_mcmc_samples()` function (default = NULL, they are simply numerated).
#' @param inner_credible percentage of the inner credible interval (default = .5)
#' @param outer_credible percentage of the outer credible interval (default = .9)
#' @param outer_show should the outer credible interval be shown (default = TRUE)
#' @param number_fundable number of proposals that can be funded
#' @param ylab The y axis label (default = "Expected Ranks")
#' (default = NULL, no FL drawn).
#' @param size_outer the size of the line representing the outer CrI
#'  (default = .5)
#' @param size_inner the size of the line representing the inner CrI
#' (default = 2)
#' @param size_pt the size of the point estimate (mean, ER) (default = 1.5)
#' @param alpha_inner alpha for the color of the inner CrI
#' @param alpha_outer alpha for the color of the inner CrI
#' @param proposal Word written infront of number (default = "Proposal")
#' @param use_outer_inner should the inner or outer CRI be used for funding
#' decisions (default = "inner")
#' @param change_random_selection_to Should the "random selection" group be
#' called something else; f.ex "tie group" (default = NULL)
#' @param change_rejected_to Should the "rejected" group be called something
#' else (default = NULL)
#' @param change_accepted_to Should the "accepted" group be called something
#' else (default = NULL)
#'
#' @return the result is a plot of the expected rank together with their
#' credible intervals
#'
#' @importFrom bayesplot mcmc_intervals_data
#' @importFrom dplyr arrange slice recode_factor
#'
#' @export
#'
#' @examples
#' data_panel1 <- get_mock_data() %>%
#'      filter(panel == "p1")
#' \dontrun{
#' mcmc_samples <- get_mcmc_samples(data = data_panel1,
#'                                  id_proposal = "application",
#'                                  id_assessor = "voter",
#'                                  grade_variable = "num_grade")
#' plot_er_distributions(mcmc_samples,
#'                       n_proposals = data_panel1 %>%
#'                            summarise(n = n_distinct(application)) %>%
#'                            pull(),
#'                       number_fundable = 5)
#'                       }
plot_er_distributions <- function(get_mcmc_samples_result, n_proposals,
                                  name_er_or_theta = "rank_theta",
                                  er = TRUE, title = NULL,
                                  names_proposals = NULL, inner_credible = .5,
                                  outer_credible = .9, ylab = "Expected Ranks",
                                  number_fundable = NULL, outer_show = TRUE,
                                  size_pt = 1.75, size_outer = .5,
                                  size_inner = 2, alpha_inner = .5,
                                  alpha_outer = 1,
                                  proposal = "Proposal ",
                                  use_outer_inner = "inner",
                                  change_random_selection_to = NULL,
                                  change_rejected_to = NULL,
                                  change_accepted_to = NULL){


  er_colnames <- paste0(name_er_or_theta, "[", seq_len(n_proposals), "]")
  if (is.list(get_mcmc_samples_result$samples)) {
    er_samples <- do.call(rbind, get_mcmc_samples_result$samples$mcmc)[, er_colnames]
  } else {
    er_samples <- get_mcmc_samples_result$samples[, er_colnames]
  }

  if (!is.null(names_proposals)){
    colnames(er_samples) <- names_proposals
  } else colnames(er_samples) <- paste0(proposal, seq_len(n_proposals))

  mcmc_intervals_data_mat <- mcmc_intervals_data(er_samples, point_est = "mean",
                                                 prob = inner_credible,
                                                 prob_outer = outer_credible)

  order <- mcmc_intervals_data_mat %>%
    arrange(.data$m) %>%
    dplyr::pull(.data$parameter)

  if (!er) { # for the thetas the higher means better
    order <- mcmc_intervals_data_mat %>%
      arrange(-.data$m) %>%
      dplyr::pull(.data$parameter)
  }

  if (!is.null(number_fundable)) {
    # If we draw a funding line, the provisional funding line is equal to the
    # ER of the last fundable proposal
    funding_line_at <- mcmc_intervals_data_mat %>%
      arrange(.data$m) %>%
      slice(number_fundable) %>%
      dplyr::pull(.data$m)

    # The names of the different recommendation groups; by default accepted,
    # rejected, and random selection groups.
    group_names <- c(accepted = ifelse(is.null(change_accepted_to), "accepted",
                                       change_accepted_to),
                     rejected = ifelse(is.null(change_rejected_to), "rejected",
                                       change_rejected_to),
                     "random selection" =
                       ifelse(is.null(change_random_selection_to),
                              "random selection", change_random_selection_to))
    # Distribute into the different recommendation groups, depending on whether
    # the decision should be taken based on the inner or on the outer CrI.
    if (use_outer_inner == "inner") {
      mcmc_intervals_data_mat <- mcmc_intervals_data_mat %>%
        mutate(rs = ifelse(((.data$l <= funding_line_at) &
                             (.data$h >= funding_line_at)) |
                             .data$m == funding_line_at,
                           # To ensure the special case where the ER of the last
                           # fundable is not in the CrI of the last fundable,
                           # because of the ER being the mean and the CrI
                           # quantiles.
                           group_names["random selection"],
                           ifelse((.data$l < funding_line_at) &
                                    (.data$h < funding_line_at),
                                  group_names["accepted"], group_names["rejected"])),
               rs = factor(.data$rs, group_names))
    }

    if (use_outer_inner == "outer") {
      mcmc_intervals_data_mat <- mcmc_intervals_data_mat %>%
        mutate(rs = ifelse((.data$ll <= funding_line_at) &
                             (.data$hh >= funding_line_at),
                           group_names["random selection"],
                           ifelse((.data$ll < funding_line_at) &
                                    (.data$hh < funding_line_at),
                                  group_names["accepted"],
                                  group_names["rejected"])),
               rs = factor(.data$rs, group_names))
    }


    if (!er) {  # for the thetas the higher means better
      funding_line_at <- mcmc_intervals_data_mat %>%
        arrange(-.data$m) %>%
        slice(number_fundable) %>%
        dplyr::pull(.data$m)
      mcmc_intervals_data_mat <- mcmc_intervals_data_mat %>%
        mutate(rs = ifelse((.data$l <= funding_line_at) &
                             (.data$h >= funding_line_at),
                           group_names["random selection"],
                           ifelse((.data$l < funding_line_at) &
                                    (.data$h < funding_line_at),
                                  group_names["rejected"],
                                  group_names["accepted"])),
               rs = factor(.data$rs, group_names))
    }

    # If all proposals in the random selection group can be founded, e.g. the
    # ones not in the rejected group, then, the ones in the random selection
    # group will be transfered to the accepted group.
    if (mcmc_intervals_data_mat %>%
        filter(.data$rs != group_names["rejected"]) %>%
        nrow() == number_fundable) {
      # to recode the factor, we will use a named vector:
      t <- c(group_names["accepted"])
      names(t) <- group_names["random selection"]

      mcmc_intervals_data_mat <- mcmc_intervals_data_mat %>%
        mutate(rs = recode_factor(.data$rs, !!!t))
    }
  }


  plot <- mcmc_intervals_data_mat %>%
    mutate(parameter = factor(.data$parameter, levels = order)) %>%
    ggplot()
  if (outer_show){
    if (use_outer_inner == "inner" & !is.null(number_fundable)) {
      plot <- plot +
        geom_segment(aes(y = .data$ll, yend = .data$hh,
                         x = .data$parameter, xend = .data$parameter),
                     size = size_outer, color = "darkgray")
    }
    if (use_outer_inner == "outer") {
      plot <- plot +
        geom_segment(aes(y = .data$ll, yend = .data$hh,
                         x = .data$parameter, xend = .data$parameter,
                         color = .data$rs),
                   size = size_outer, alpha = alpha_outer)
    }
  }


  if (!is.null(number_fundable)) {
    plot <- plot +
      geom_hline(yintercept = funding_line_at,
                 color = "darkblue", alpha = .75, linetype = "dashed") +
      geom_segment(aes(y = .data$l, yend = .data$h,
                       x = .data$parameter, xend = .data$parameter,
                       color = .data$rs),
                   size = size_inner, alpha = alpha_inner) +
      geom_point(aes(y = .data$m, x = .data$parameter, fill = .data$rs),
                 size = size_pt, pch = 21, color = "darkblue") +
      scale_color_manual(values = c("#E69F00", "#56B4E9",
                                    "#7f36c4")) +
      scale_fill_manual(values = c("#E69F00", "#56B4E9", "#7f36c4")) +
      labs(y = ylab, title = title) +
      theme_minimal() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.title.y = element_text(size = 9),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 5),
            legend.title = element_blank())
  } else {
    plot <- plot +
      geom_segment(aes(y = .data$l, yend = .data$h,
                       x = .data$parameter, xend = .data$parameter),
                   size = size_inner, alpha = alpha_inner, color = "darkblue") +
      geom_point(aes(y = .data$m, x = .data$parameter, fill = .data$rs),
                 size = size_pt, pch = 21, color = "darkblue") +
      labs(y = ylab, title = title) +
      theme_minimal() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.title.y = element_text(size = 9),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 5))
  }

  plot
}


