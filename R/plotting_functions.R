# ------------------
# Plotting functions
# ------------------


#' The Plot of the Expected Rank from a JAGS model
#'
#' This function plot the results form the JAGS-ER
#' @param er_results the resulting object from the `get_er_from_jags()` function
#' @param rankability Do we want the rankability written in the plot title
#' (default = TRUE)
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
#' ER_results <- get_er_from_jags(data = dat,
#'                   id_application = "application",
#'                   id_voter = "voter",
#'                   grade_variable = "num_grade",
#'                   n_chains = 2, n_iter = 1000,
#'                   n_burnin = 1000)
#' plotting_er_results(ER_results, title = "Panel 1", how_many_fundable = 5)
#'
#' @export
plotting_er_results <- function(er_results, rankability = TRUE,
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
  if (is.null(colors)) colors <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2",
                                   "#D55E00", "#CC79A7")

  # Gather the data needed for the plot:
  # -----------------------------------
  # If we know how many more are fundable, we have to add a variable for the
  # coloring.

  if (!is.null(how_many_fundable)){
    dat_for_plot <- er_results$rankings %>%
      arrange(er) %>%
      mutate(funded = c(rep("Funded", how_many_fundable),
                        rep("Rejected", n() - how_many_fundable))) %>%
      mutate(numbering = 1:n())
  } else dat_for_plot <- er_results$rankings %>%
      arrange(er) %>%
      mutate(numbering = 1:n())

  dat_for_plot <- dat_for_plot %>%
    gather("rank", "rank_pm", "er",
           key = which_rank, value = rank) %>%
    mutate(which_rank = case_when(which_rank == "rank" ~ "Fixed",
                                  which_rank == "rank_pm" ~
                                    "Posterior Mean",
                                  which_rank == "er" ~ "ER"),
           which_rank = factor(which_rank,
                               levels = c("Fixed",
                                          "Posterior Mean", "ER"))) %>%
    rename(application_number = eval(id_application))

  start_plot <- dat_for_plot %>%
    ggplot(aes(x = which_rank, y = rank, group = application_number))

  if (!is.null(how_many_fundable)) {
    start_plot <- dat_for_plot %>%
      ggplot(aes(x = which_rank, y = rank, group = application_number,
                 color = funded))
  }

  if (result_show) {
    start_plot <- start_plot +
      geom_text_repel(aes(label = ifelse(which_rank == "ER" & !easy_numbering,
                                         application_number,
                                         ifelse(which_rank == "ER" &
                                                  easy_numbering,
                                                numbering, ""))),
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
  if (rankability) {
    finish_plot <- middle_plot +
      ggtitle(paste0(title, ", rankability: ",
                     round(er_results$rankability, 2)))
  } else {
    finish_plot <- middle_plot +
      ggtitle(title)
  }

  finish_plot +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(hjust = hjust_y_axis))
}


#' The Plot of the Expected Rank with their confidence intervals
#'
#' The ER of each application are presented together with their 95% Wald CI
#' @param er_results the resulting object from the `get_er_from_jags()` function
#' @param id_application name of the identifier of the application
#' @param alpha alpha level for the CI-bands (default = 0.5)
#' @param color color of point and bands (default = "black")
#' @export
plotting_er_with_var <- function(er_results, id_application = "id_application",
                                 alpha = .5, color = 1) {
  dat <- er_results$rankings %>%
    arrange(er) %>%
    mutate(lower_bound = er - 1.96 * sqrt(er_var),
           lower_bound = if_else(lower_bound < 1, 1, lower_bound),
           upper_bound = er + 1.96 * sqrt(er_var),
           order = seq_len(nrow(er_results$rankings)))

  dat %>%
    ggplot(aes(x = order, y = er)) +
    geom_point(color = color) +
    geom_segment(aes(x = order, xend = order,
                     y = lower_bound, yend = upper_bound),
                 alpha = alpha, color = color) +
    labs(x = "", y = "Expected Rank") +
    scale_x_continuous(breaks = dat$order,
                       labels = dat[, id_application] %>% pull()) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))
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
#' @param n_burnin number of burnin iterations
#' @param n_iter how many iterations used in the JAGS sampler? (default = 5000)
#' @param n_chains number of chains for the JAGS sampler. (default = 4)
#' @param id_section name of the section
#' @param theta_name the name of the application identifier. (default =
#' application_intercept")
#' @param tau_name name of the the squareroot of tau, being the precision of the
#' random effects, in the jags model. (default = sd_application)
#' @param tau_voter_name name of the (default = tau_voter)
#' @param sigma_name name of the standard error of th term (default = sigma)
#' @param cumulative_rank_prob should the cumulative ranking probabilities be
#' represented instead
#' @param mcmc_samples if the mcmc sample has already been run (default = NULL).
#' @import rjags
#' @importFrom utils head
#' @return the result is a plot of the rankogram (or cumulative ranking
#' probabilities)
#' @export
plot_rankogram <- function(data, id_application, id_voter,
                           grade_variable = "num_grade",
                           path_to_jags_model = NULL,
                           n_chains = 3, n_iter = 5000,
                           n_burnin = 1000, id_section = NULL,
                           theta_name = "application_intercept",
                           tau_name = "tau_application",
                           tau_voter_name = "tau_voter",
                           sigma_name = "sigma",
                           cumulative_rank_prob = FALSE,
                           mcmc_samples = NULL) {

  n_application <- data %>%
    pull(get(id_application)) %>%
    unique() %>%
    length()

  overall_mean <- data %>%
    group_by(get(id_application)) %>%
    summarise(av = mean(num_grade, na.rm = TRUE)) %>%
    pull(av) %>%
    mean()

  if (is.null(mcmc_samples)) {
    mcmc_samples <-
      get_mcmc_samples(data = data, id_application = id_application,
                       id_voter = id_voter, grade_variable = grade_variable,
                       path_to_jags_model = path_to_jags_model,
                       n_chains = n_chains, n_iter = n_iter,
                       n_burnin = n_burnin, id_section = id_section,
                       theta_name = theta_name, tau_name = tau_name,
                       tau_voter_name = tau_voter_name, sigma_name = sigma_name)
  }

  colnames_theta <- paste0(theta_name, "[", seq_len(n_application), "]")
  mcmc_samples_thetas <- mcmc_samples[, colnames_theta]

  # Calculate the P(j = b) for the SUCRA:
  p_j_b <- matrix(NA, nrow = n_application, ncol = n_application)

  ranks <- apply(X = overall_mean + mcmc_samples_thetas,
                 MARGIN = 1, FUN = function(x) rank(-x))

  for (i in seq_len(n_application)) {
    for (j in seq_len(n_application)) {
      p_j_b[i, j] <- mean(ranks[i, ] == j)
    }
  }

  colnames(p_j_b) <- seq_len(ncol(p_j_b))
  rownames(p_j_b) <- data %>%
    pull(get(id_application)) %>%
    unique()

  if (!cumulative_rank_prob) {
    p_j_b %>%
      as_tibble(rownames = "application") %>%
      gather(rank, prob, paste0(1):paste0(ncol(p_j_b))) %>%
      mutate(rank = as.numeric(rank)) %>%
      ggplot(aes(x = rank, y = prob)) +
      geom_line() +
      labs(x = "Rank", y = "Probability") +
      theme_minimal() +
      facet_wrap(~ application)
  } else {
    sucra <- sapply(seq_len(nrow(p_j_b)), function(i) {
      mean(cumsum(p_j_b[i, -nrow(p_j_b)]))
    })
    names(sucra) <- data %>%
      pull(get(id_application)) %>%
      unique()

    p_j_b %>%
      as_tibble(rownames = "application") %>%
      gather(rank, prob, paste0(1):paste0(ncol(p_j_b))) %>%
      mutate(rank = as.numeric(rank)) %>%
      group_by(application) %>%
      arrange(rank) %>%
      mutate(cum_prob = cumsum(prob),
             sucra = sucra[head(application, 1)]) %>%
      ungroup() %>%
      ggplot(aes(x = rank, y = cum_prob)) +
      geom_line() +
      labs(x = "Rank", y = "Cumulative probability") +
      geom_text(aes(x = n_application - 3, y = .5, label =
                      paste0("SUCRA: ", round(sucra, 2))),
                size = 2) +
      theme_minimal() +
      facet_wrap(~ application)
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
#' ticks. Note that the ordering has to be the same as the one they are given to
#' the `get_mcmc_samples()` function (default = NULL, they are simply numerated).
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
                                        names_voters = NULL,
                                        title = NULL, xlim_min = -1,
                                        xlim_max = 1,
                                        scale = 1.75){
  voter_behavior_colnames <- paste0(name_mean, "[", seq_len(n_voters), "]")
  voter_behavior_samples <- get_mcmc_samples_result[, voter_behavior_colnames]

  colnames(voter_behavior_samples) <- paste0("voter ",
                                             seq_len(n_voters))

  voter_behavior_sample_for_plot <- voter_behavior_samples %>%
    as_tibble() %>%
    pivot_longer(cols = starts_with("voter"),
                 names_to = "Referee", values_to = "weight") %>%
    group_by(Referee) %>%
    mutate(median = median(weight)) %>%
    ungroup() %>%
    arrange(median)
  voter_behavior_sample_for_plot %>%
    mutate(Referee = factor(Referee, levels = voter_behavior_sample_for_plot %>%
                              select(Referee, median) %>%
                              distinct() %>%
                              pull(Referee))) %>%
    ggplot(aes(x = weight, y = Referee, fill = stat(x))) +
    geom_density_ridges_gradient(scale = scale, rel_min_height = 0.02,
                                 show.legend = FALSE,
                                 gradient_lwd = 0) +
    xlim(xlim_min, xlim_max) +
    labs(title = title) +
    labs(x = "", y = "") +
    scale_fill_viridis_c(name = "Voter Effect", option = "C") +
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
#' @param proposal Word written infront of number (default = "Proposal")
#' @return the result is a plot of the expected rank together with their
#' credible intervals
#' @import bayesplot
#' @export
plot_er_distributions <- function(get_mcmc_samples_result, n_proposals,
                                  name_er_or_theta = "rank_theta",
                                  er = TRUE, title = NULL,
                                  names_proposals = NULL, inner_credible = .5,
                                  outer_credible = .9, ylab = "Expected Ranks",
                                  number_fundable = NULL, outer_show = TRUE,
                                  size_pt = 1.5, size_outer = .5,
                                  size_inner = 2, alpha_inner = .5,
                                  proposal = "Proposal "){


  er_colnames <- paste0(name_er_or_theta, "[", seq_len(n_proposals), "]")
  er_samples <- get_mcmc_samples_result[, er_colnames]

  if (!is.null(names_proposals)){
    colnames(er_samples) <- names_proposals
  } else colnames(er_samples) <- paste0(proposal, seq_len(n_proposals))

  mcmc_intervals_data_mat <- mcmc_intervals_data(er_samples, point_est = "mean",
                                                 prob = inner_credible,
                                                 prob_outer = outer_credible)

  order <- mcmc_intervals_data_mat %>%
    arrange(m) %>%
    pull(parameter)

  if (!er) { # for the thetas the higher means better
    order <- mcmc_intervals_data_mat %>%
      arrange(-m) %>%
      pull(parameter)
  }

  if (!is.null(number_fundable)) {
    # If we draw a funding line
    funding_line_at <- mcmc_intervals_data_mat %>%
      arrange(m) %>%
      slice(number_fundable) %>%
      pull(m)
    mcmc_intervals_data_mat <- mcmc_intervals_data_mat %>%
      mutate(rs = ifelse((l <= funding_line_at) & (h >= funding_line_at),
                         "random selection",
                         ifelse((l < funding_line_at) & (h < funding_line_at),
                                "accepted", "rejected")),
             rs = factor(rs, c("accepted", "rejected",
                               "random selection")))
    if (!er) {  # for the thetas the higher means better
      funding_line_at <- mcmc_intervals_data_mat %>%
        arrange(-m) %>%
        slice(number_fundable) %>%
        pull(m)
      mcmc_intervals_data_mat <- mcmc_intervals_data_mat %>%
        mutate(rs = ifelse((l <= funding_line_at) & (h >= funding_line_at),
                           "random selection",
                           ifelse((l < funding_line_at) & (h < funding_line_at),
                                  "rejected", "accepted")),
               rs = factor(rs, c("accepted", "rejected",
                                 "random selection")))
    }
    if (mcmc_intervals_data_mat %>%
        filter(rs != "rejected") %>%
        nrow() == number_fundable) {
      mcmc_intervals_data_mat <- mcmc_intervals_data_mat %>%
        mutate(rs = recode_factor(rs, "random selection" = "accepted"))
    }
  }

  plot <- mcmc_intervals_data_mat %>%
    mutate(parameter = factor(parameter, level = order)) %>%
    ggplot()
  if (outer_show){
    plot <- plot +
      geom_segment(aes(y = ll, yend = hh, x = parameter, xend = parameter),
                   size = size_outer, color = "darkgray")
  }


  if (!is.null(number_fundable)) {
    plot <- plot +
      geom_hline(yintercept = funding_line_at,
                 color = "darkblue", alpha = .75, linetype = "dashed") +
      geom_segment(aes(y = l, yend = h, x = parameter, xend = parameter, color = rs),
                   size = size_inner, alpha = alpha_inner) +
      geom_point(aes(y = m, x = parameter), size = size_pt, color = "darkblue") +
      scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
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
      geom_segment(aes(y = l, yend = h, x = parameter, xend = parameter),
                   size = size_inner, alpha = alpha_inner, color = "darkblue") +
      geom_point(aes(y = m, x = parameter), size = size_pt, color = "darkblue") +
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
