#' Helper function to define overdispersed initial values
#'
#' Note that this function can only be applied when four chains are used,
#' otherwise the user has to provide the initial values themselves. It is
#' however recommended to use four chain. The helper function will also use the
#' four different random generators provided by rjags.
#' @param merging_panels should the sections be merged?, default is FALSE,
#' e.g. a ranking is done for each section/panel separately.
#' @param ordinal_scale are the evaluation scores on an ordinal scale?,
#' default is FALSE.
#' @param point_scale if on an ordinal scale, what is the number of points on
#' the scale?, default is NULL
#' @param heterogeneous_residuals dummy variable informing us on whether or not
#' the residuals should be heterogeneous, default = FALSE.
#' @param n_proposals number of applications / proposals considered.
#' @param n_assessors number of voters / panel members.
#' @param n_panels number of sections / panels, if merged, default is NULL.
#' @param grades the grades given to the proposals (the whole
#' n_proposals*n_assessors long vector)
#' @param seed seed used for the sampling
#' @details It is optimal to use four chains. This also allows the sampling to
#' use the four different samplers implemented in rjags (e.g. Wichmann-Hill,
#' Marsaglia-Multicarry, Super-Duper, Mersenne-Twister)
#'
#' @return a list of overdispersed initial values for the two chains
#' @export
get_inits_overdispersed_four_chains <- function(merging_panels = FALSE,
                                                ordinal_scale = FALSE,
                                                point_scale = NULL,
                                                heterogeneous_residuals = FALSE,
                                                n_proposals,
                                                n_assessors,
                                                n_panels = NULL,
                                                grades = NULL,
                                                seed){

  set.seed(seed)
  seeds <- sample(seq_len(10^6), size = 4)
  # Initialise them for the case where the results of the panels are NOT merged
  # and the scores are on a numeric scale:
  inits_chain1 <- # Inits for first chain
    list(proposal_intercept =
           rep.int(-4, times = n_proposals),
         assessor_intercept = matrix(rep(-4, times = n_assessors *
                                        n_proposals),
                                  ncol = n_assessors,
                                  nrow = n_proposals),
         nu = rep(-2, n_assessors),
         sigma = 10^{-4},
         tau_proposal =  10^{-4},
         tau_assessor = 10^{-6},
         # need this part to get reproducible results
         .RNG.name = "base::Wichmann-Hill",
         .RNG.seed = seeds[1])
  inits_chain2 <- # Inits for second chain
    list(proposal_intercept =
           rep.int(4, times = n_proposals),
         assessor_intercept = matrix(rep(4, times = n_assessors *
                                        n_proposals),
                                  ncol = n_assessors,
                                  nrow = n_proposals),
         nu = rep(2, n_assessors),
         sigma = 1,
         tau_proposal = 1.5,
         tau_assessor = 1,
         # need this part to get reproducible results
         .RNG.name = "base::Marsaglia-Multicarry",
         .RNG.seed = seeds[2])
  inits_chain3 <- # Inits for third chain
    list(proposal_intercept =
           rep.int(-2, times = n_proposals),
         assessor_intercept = matrix(rep(-2, times = n_assessors *
                                        n_proposals),
                                  ncol = n_assessors,
                                  nrow = n_proposals),
         nu = rep(-1, n_assessors),
         sigma = 10^{-2},
         tau_proposal =  10^{-2},
         tau_assessor = 10^{-3},
         # need this part to get reproducible results
         .RNG.name = "base::Super-Duper",
         .RNG.seed = seeds[3])
  inits_chain4 <- # Inits for fourth chain
    list(proposal_intercept =
           rep.int(2, times = n_proposals),
         assessor_intercept = matrix(rep(2, times = n_assessors *
                                        n_proposals),
                                  ncol = n_assessors,
                                  nrow = n_proposals),
         nu = rep(1, n_assessors),
         sigma = 2,
         tau_proposal = 2,
         tau_assessor = 2,
         # need this part to get reproducible results
         .RNG.name = "base::Mersenne-Twister",
         .RNG.seed = seeds[4])

  if (merging_panels) {
   inits_chain1 <- # Inits for first chain
     c(inits_chain1, list(tau_panel = 10^{-6},
                          panel_intercept = rep.int(-5, times = n_panels)))
    inits_chain2 <- # Inits for second chain
      c(inits_chain2, list(tau_panel = 1,
                           panel_intercept = rep.int(5, times = n_panels)))
    inits_chain3 <- # Inits for third chain
      c(inits_chain3, list(tau_panel = 10^{-3},
                           panel_intercept =rep.int(-3, times = n_panels)))
    inits_chain4 <- # Inits for fourth chain
      c(inits_chain4, list(tau_panel = 2,
                           panel_intercept = rep.int(3, times = n_panels)))
  }

  if (ordinal_scale) {
    inits_chain1 <- # Inits for first chain
      c(inits_chain1, list(cc = seq(.5, point_scale - 1, 1),
                           latent_trait = grades))
    inits_chain2 <- # Inits for second chain
      c(inits_chain2, list(cc = seq(.5, point_scale - 1, 1),
                           latent_trait = grades))
    inits_chain3 <- # Inits for third chain
      c(inits_chain3, list(cc = seq(.5, point_scale - 1, 1),
                           latent_trait = grades))
    inits_chain4 <- # Inits for fourth chain
      c(inits_chain4, list(cc = seq(.5, point_scale - 1, 1),
                           latent_trait = grades))
  }

  if (heterogeneous_residuals) {
    inits_chain1["sigma"] <- NULL
    inits_chain2["sigma"] <- NULL
    inits_chain3["sigma"] <- NULL
    inits_chain4["sigma"] <- NULL
    inits_chain1 <- # Inits for first chain
      c(inits_chain1, list(tau_omega = 10^{-6},
                           alpha = -.02,
                           beta = -.02))
    inits_chain2 <- # Inits for second chain
      c(inits_chain2, list(tau_omega = 1.5,
                           alpha = .02,
                           beta = .02))
    inits_chain3 <- # Inits for third chain
      c(inits_chain3, list(tau_omega = 10^{-6},
                           alpha = -.01,
                           beta = -.01))
    inits_chain4 <- # Inits for fourth chain
      c(inits_chain4, list(tau_omega = 2,
                           alpha = .01,
                           beta = .01))
  }

  inits <- list(inits_chain1, inits_chain2, inits_chain3, inits_chain4)

  return(inits)
}


