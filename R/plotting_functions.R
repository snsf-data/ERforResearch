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
#' @param id_application the name of the variable giving the distinct
#' application identifiers. (default = id_application)
#' @param draw_funding_line should the funding line be drawn? (default = TRUE)
#' @param x_expand make funding line longer on both sides (default = .3)
#' @param ordering_increasing should the order of the rank be increasing, from
#' up to dowm: from rank 1 on top to rank n on the bottom (default = TRUE)
#' @param result_show boolean telling us whether the application number/id
#' should be added next to the ER points (default = FALSE).
#' @param easy_numbering should the ER results just be numbered?
#' (default = TRUE)
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
#' @return the result is a plot
#' @import ggrepel
#' @import tidyr
#' @import ggplot2
#' @examples
#' dat <- get_mock_data() %>%
#' filter(panel == "p1")
#' \dontrun{
#' ER_results <- get_er_from_jags(data = dat,
#'                   id_application = "application",
#'                   id_voter = "voter",
#'                   grade_variable = "num_grade",
#'                   n_chains = 2, n_iter = 1000,
#'                   n_burnin = 1000)
#' plotting_er_results(ER_results, title = "Panel 1", how_many_fundable = 5)
#' }
#' @export
plotting_er_results <- function(er_results,
                                id_application = "id_application",
                                how_many_fundable = NULL,
                                title = "",
                                ordering_increasing = TRUE,
                                draw_funding_line = TRUE,
                                result_show = TRUE,
                                easy_numbering = TRUE,
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

  dat_for_plot <- dat_for_plot %>%
    gather("rank", "rank_pm", "er",
           key = "which_rank", value = "rank") %>%
    mutate(which_rank = case_when(.data$which_rank == "rank" ~ "Fixed",
                                  .data$which_rank == "rank_pm" ~
                                    "Posterior Mean",
                                  .data$which_rank == "er" ~ "ER"),
           which_rank = factor(.data$which_rank,
                               levels = c("Fixed",
                                          "Posterior Mean", "ER"))) %>%
    rename(application_number = eval(id_application))

  start_plot <- dat_for_plot %>%
    ggplot(aes(x = .data$which_rank, y = .data$rank,
               group = .data$application_number))

  if (!is.null(how_many_fundable)) {
    start_plot <- dat_for_plot %>%
      ggplot(aes(x = .data$which_rank, y = .data$rank,
                 group = .data$application_number, color = .data$funded))
  }

  if (result_show) {
    start_plot <- start_plot +
      geom_text_repel(aes(label = ifelse(.data$which_rank == "ER" &
                                           !easy_numbering,
                                         .data$application_number,
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
                         application_number = rep(1,3)),
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
#' @param data long data frame with all information as in the jags model
#' defined below.
#' @param id_application the name of the application variable in the data
#' @param id_voter the name of the voter variable in the data (default = NULL)
#' @param grade_variable the name of the outcome variable
#' (default = "num_grade")
#' @param path_to_jags_model the path to the jags txt file, if null, the
#' default model is used. (default = NULL)
#' @param n_iter how many iterations used in the JAGS sampler?
#'  (default = 5000)
#' @param n_chains number of chains for the JAGS sampler. (default = 2)
#' @param n_adapt number of iterations discarded for the adaptation phase.
#'  (default = 1000)
#' @param n_burnin number of burnin iterations discarded. (default = 1000)
#' @param id_section name of the section
#' @param rank_theta_name the name of the rank of theta in the Bayesian model.
#' (default = rank_theta")
#' @param theta_name the name of the application identifier. (default =
#' "application_intercept")
#' @param voter_name the name of the voter intercept in the JAGS model (default
#' = voter_intercept).
#' @param tau_name name of tau in the JAGS model, being the precision of the
#' random effects, in the jags model. (default = sd_application)
#' @param tau_voter_name name of the standard error of the voter effect.
#' (default = tau_voter)
#' @param tau_section_name name of the strandard error of the section effect, if
#' needed (default = NULL).
#' @param sigma_name name of the standard error of th term (default = sigma)
#' @param cumulative_rank_prob should the cumulative ranking probabilities be
#' represented instead
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
#' @import dplyr
#' @importFrom utils head
#'
#' @return the result is a plot of the rankogram (or cumulative ranking
#' probabilities)
#'
#' @export
plot_rankogram <- function(data, id_application, id_voter,
                           grade_variable = "num_grade",
                           path_to_jags_model = NULL,
                           n_chains = 2, n_iter = 5000,
                           n_burnin = 1000, n_adapt = 1000,
                           id_section = NULL,
                           rank_theta_name = "rank_theta",
                           theta_name = "application_intercept",
                           voter_name = "voter_intercept",
                           tau_name = "tau_application",
                           tau_voter_name = "tau_voter",
                           tau_section_name = NULL,
                           sigma_name = "sigma",
                           cumulative_rank_prob = FALSE,
                           ordinal_scale = FALSE,
                           heterogeneous_residuals = FALSE,
                           point_scale = NULL,
                           inits_type = "random",
                           initial_values = NULL,
                           mcmc_samples = NULL,
                           seed = 1991, quiet = FALSE) {

  n_application <- data %>%
    dplyr::pull(get(id_application)) %>%
    unique() %>%
    length()

  overall_mean <- data %>%
    mutate(num_grade = grade_variable) %>%
    group_by(get(id_application)) %>%
    summarise(av = mean(.data$num_grade, na.rm = TRUE)) %>%
    dplyr::pull(.data$av) %>%
    mean()

  # If no MCMC samples are provided, they are computed here:
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
                                     voter_name = voter_name,
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
                                     seed = seed, quiet = quiet,
                                     compute_ess = FALSE) # no need)
  } else {
    if (length(mcmc_samples) != 8) {
      stop(paste0("Make sure that the object given to mcmc_samples is an ",
                  "object that was build with get_mcmc_samples()."))
    }
  }

  colnames_rank_theta <- paste0(rank_theta_name, "[",
                                seq_len(n_application), "]")
  if (is.list(mcmc_samples$samples)) {
    mcmc_samples_rank_thetas <-
      do.call(rbind, mcmc_samples$samples)[, colnames_rank_theta]
  } else {
    mcmc_samples_rank_thetas <-
      mcmc_samples$samples[, colnames_rank_theta]
  }

  # Calculate the P(j = b) for the SUCRA:
  p_j_b <- matrix(NA, nrow = n_application, ncol = n_application)

  for (i in seq_len(n_application)) {
    for (j in seq_len(n_application)) {
      p_j_b[i, j] <- mean(mcmc_samples_rank_thetas[, i] == j)
    }
  }
  # Compute the SUCRA
  sucra <- sapply(seq_len(nrow(p_j_b)), function(i) {
    mean(cumsum(p_j_b[i, -nrow(p_j_b)]))
  })

  colnames(p_j_b) <- seq_len(ncol(p_j_b))
  rownames(p_j_b) <- data %>%
    dplyr::pull(get(id_application)) %>%
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
      dplyr::pull(get(id_application)) %>%
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
      geom_text(aes(x = n_application - 3, y = .5, label =
                      paste0("SUCRA: ", round(sucra, 2))),
                size = 2) +
      theme_minimal() +
      facet_wrap(~ .data$application)
  }
}

#' Voter behavior distributions
#'
#' This function plots the distibutions of the average voter behavior
#' (the $mu_j$'s in the model).
#'
#' @param get_mcmc_samples_result the mcmc samples
#' @param n_voters number of voters in the panel / call
#' @param name_mean the name of the parameter estimating the average voter
#' behavior (default = "nu").
#' @param names_voters names of the voters to be written on the y-axis
#' ticks (default = "voter").
#' @param title title of the plot (default = NULL, no title).
#' @param xlim_min minimum of the x-axis (default = -1).
#' @param xlim_max maximum of the x-axis (default = 1).
#' @param scale for the `geom_density_ridges_gradient()` (default = 1.75).
#' @return the result is a plot of the posterior distributions of the average
#' voter behaviors.
#' @import ggridges
#' @importFrom stats median
#' @export
voter_behavior_distribution <- function(get_mcmc_samples_result, n_voters,
                                        name_mean = "nu",
                                        names_voters = "voter",
                                        title = NULL, xlim_min = -1,
                                        xlim_max = 1,
                                        scale = 1.75){

  x <- NULL


  if (is.list(get_mcmc_samples_result$samples)) {
    samples <- do.call(rbind, get_mcmc_samples_result$samples)
  } else samples <- get_mcmc_samples_result$samples

  voter_behavior_colnames <- paste0(name_mean, "[", seq_len(n_voters), "]")
  voter_behavior_samples <- samples[, voter_behavior_colnames]


  colnames(voter_behavior_samples) <- paste0(names_voters, " ",
                                             seq_len(n_voters))

  voter_behavior_sample_for_plot <- voter_behavior_samples %>%
    as_tibble() %>%
    pivot_longer(cols = starts_with(names_voters),
                 names_to = "Referee", values_to = "weight") %>%
    group_by(.data$Referee) %>%
    mutate(median = median(.data$weight)) %>%
    ungroup() %>%
    arrange(.data$median)
  voter_behavior_sample_for_plot %>%
    mutate(Referee = factor(.data$Referee,
                            levels = voter_behavior_sample_for_plot %>%
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
    scale_fill_viridis_c(name = paste(names_voters, "Effect"), option = "C") +
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
#'
#' @return the result is a plot of the expected rank together with their
#' credible intervals
#'
#' @import bayesplot
#'
#'@export
#'
#'@examples
#' data_panel1 <- get_mock_data() %>%
#'      filter(panel == "p1")
#' \dontrun{
#' mcmc_samples <- get_mcmc_samples(data = data_panel1,
#'                                  id_application = "application",
#'                                  id_voter = "voter",
#'                                  grade_variable = "num_grade")
#' plot_er_distributions(mcmc_samples_object,
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
                                  size_pt = 1.5, size_outer = .5,
                                  size_inner = 2, alpha_inner = .5,
                                  alpha_outer = 1,
                                  proposal = "Proposal ",
                                  use_outer_inner = "inner"){


  er_colnames <- paste0(name_er_or_theta, "[", seq_len(n_proposals), "]")
  if (is.list(get_mcmc_samples_result$samples)) {
    er_samples <- do.call(rbind, get_mcmc_samples_result$samples)[, er_colnames]
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
    # If we draw a funding line
    funding_line_at <- mcmc_intervals_data_mat %>%
      arrange(.data$m) %>%
      slice(number_fundable) %>%
      dplyr::pull(.data$m)
    if (use_outer_inner == "inner") {
    mcmc_intervals_data_mat <- mcmc_intervals_data_mat %>%
      mutate(rs = ifelse((.data$l <= funding_line_at) &
                           (.data$h >= funding_line_at), "random selection",
                         ifelse((.data$l < funding_line_at) &
                                  (.data$h < funding_line_at),
                                "accepted", "rejected")),
             rs = factor(.data$rs, c("accepted", "rejected",
                                     "random selection")))
    }
    if (use_outer_inner == "outer") {
      mcmc_intervals_data_mat <- mcmc_intervals_data_mat %>%
        mutate(rs = ifelse((.data$ll <= funding_line_at) &
                             (.data$hh >= funding_line_at), "random selection",
                           ifelse((.data$ll < funding_line_at) &
                                    (.data$hh < funding_line_at),
                                  "accepted", "rejected")),
               rs = factor(.data$rs, c("accepted", "rejected",
                                       "random selection")))
    }


    if (!er) {  # for the thetas the higher means better
      funding_line_at <- mcmc_intervals_data_mat %>%
        arrange(-.data$m) %>%
        slice(number_fundable) %>%
        dplyr::pull(.data$m)
      mcmc_intervals_data_mat <- mcmc_intervals_data_mat %>%
        mutate(rs = ifelse((.data$l <= funding_line_at) &
                             (.data$h >= funding_line_at),
                           "random selection",
                           ifelse((.data$l < funding_line_at) &
                                    (.data$h < funding_line_at),
                                  "rejected", "accepted")),
               rs = factor(.data$rs, c("accepted", "rejected",
                                       "random selection")))
    }

    if (mcmc_intervals_data_mat %>%
        filter(.data$rs != "rejected") %>%
        nrow() == number_fundable) {
      mcmc_intervals_data_mat <- mcmc_intervals_data_mat %>%
        mutate(rs = recode_factor(.data$rs, "random selection" = "accepted"))
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
      geom_point(aes(y = .data$m, x = .data$parameter),
                 size = size_pt, color = "darkblue") +
      scale_color_manual(values = c("#E69F00", "#56B4E9",
                                    "#7f36c4")) + #, "#009E73")) +
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
      geom_point(aes(y = .data$m, x = .data$parameter),
                 size = size_pt, color = "darkblue") +
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


#' Sankey plot for funding groups
#'
#' @param get_mcmc_samples_result output from `get_mcmc_sample()`
#' @param n_proposals number of proposals in the panel / call
#' @param name_er_or_theta 	the name of the parameter estimating the ranks (or
#' the proposal effect) in the Bayesian Hierarchical model
#' (default = "rank_theta").
#' @param er if TRUE then, the ER are plotted, otherwise the thetas (the
#' proposal effects) are plotted. (default = TRUE)
#' @param title title of the plot (default = NULL, no title).
#' @param number_fundable number of proposals that can be funded.
#' @param raw_data raw long data with individual votes and numeric grade
#' @param num_grade_variable name of numeric grade in raw_data
#' @param id_proposal name of proposal / applicant identifier
#' @param colors vector with colors, default = color blind.
#'
#' @import ggalluvial
#' @import scales
#' @import forcats
#'
#' @return A plot (similar to Sankey plot)
#' @export
#'
get_sankey_plot_br <- function(get_mcmc_samples_result, n_proposals,
                               raw_data, num_grade_variable = "num_grade",
                               id_proposal,
                               name_er_or_theta = "rank_theta",
                               er = TRUE, title = NULL,
                               number_fundable = NULL, colors = NULL) {

  # Colorblind palette
  if (is.null(colors)) {
    colors <- c("#E69F00", "#56B4E9", "#009E73")
    names(colors) <- c("accepted", "rejected", "random selection")
    }
  # Get the correct data
  data_for_plot <- plot_er_distributions(get_mcmc_samples_result,
                                         n_proposals = n_proposals,
                                         name_er_or_theta = name_er_or_theta,
                                         names_proposals = NULL,
                                         number_fundable = number_fundable,
                                         proposal = "Proposal ")$data

  t <- distinct(raw_data[, id_proposal])[[1]]
  names(t) <- paste0("Proposal ", seq_len(n_proposals))
  fu <- function(name) {
    unique(names(t)[which(t == name)])
  }

  data_for_plot <- data_for_plot %>%
    select(.data$parameter, .data$m, .data$rs) %>%
    left_join(raw_data %>%
                rename(id_proposal = id_proposal) %>%
                group_by(id_proposal) %>%
                mutate(id_proposal = fu(id_proposal)) %>%
                summarise(avg = mean(.data$num_grade, na.rm = FALSE)) %>%
                ungroup() %>%
                arrange(-.data$avg) %>%
                mutate(rs_avg = c(rep("accepted", number_fundable),
                                  rep("rejected",
                                      n_proposals - number_fundable)),
                       # rs_avg = ifelse(id_proposal == "Proposal 15", "accepted", rs_avg),
                       rank_avg = 1:n_proposals) %>%
                select(-.data$avg),
              by = c("parameter" = "id_proposal")) %>%
    pivot_longer(c(.data$rs, .data$rs_avg),
                 values_to = "group", names_to = "stage")

  stratum_list <- data_for_plot %>%
    mutate(stage = factor(stage, levels = c("rs_avg", "rs")),
           group =
             fct_rev(factor(.data$group, levels = c("accepted",
                                                    "random selection",
                                                    "rejected")))) %>%
    count(stage, .data$group) %>%
    mutate(strat = paste0(.data$group, " (", n, ")"))

  data_for_plot %>%
    mutate(stage = factor(stage, levels = c("rs_avg", "rs")),
           group =
             factor(.data$group, levels = c("accepted", "random selection",
                                       "rejected"))) %>%
    ggplot(aes(x = stage, stratum = .data$group,
               alluvium = .data$parameter,
               fill = .data$group)) +
    geom_flow() +
    geom_stratum(width = 1/3, fill = "white", color = "grey") +
    geom_text(stat = "stratum", label = stratum_list$strat) +
    scale_x_discrete(labels = c("Rank based on Average",
                                "Bayesian Ranking"),
                     expand = c(0.15, 0.15)) +
    scale_fill_manual(values = colors) +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text.y = element_blank())  +
    labs(title = NULL, x = NULL, y = NULL)
}

