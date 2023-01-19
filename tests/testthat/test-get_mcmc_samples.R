test_that("Default Bayesian hierarchical model can be build", {
  # Test all model combinations:
  dat <- get_mock_data() %>%
    filter(panel == "p1")
  test <- get_mcmc_samples(data = dat, id_proposal = "proposal",
                           id_assessor =  "assessor",
                           grade_variable = "num_grade",
                           n_chains = 4, n_adapt = 10000, n_iter = 10000,
                           n_burnin = 10000, dont_bind = TRUE,
                           max_iter = 50000)
  expect_equal(class(test$samples), "mcmc")

  test <- get_mcmc_samples(data = get_mock_data(),
                           id_proposal = "proposal",
                           id_assessor = "assessor",
                           grade_variable = "num_grade",
                           id_panel = "panel",
                           inits_type = "overdispersed",
                           names_variables_to_sample =
                             c("tau_proposal", "tau_assessor", "sigma",
                               "rank_theta", "tau_panel"),
                           n_chains = 4, n_adapt = 10000, n_iter = 10000,
                           n_burnin = 10000, dont_bind = TRUE,
                           max_iter = 10000)
  expect_equal(class(test$samples), "mcmc")


  test <- get_mcmc_samples(data = dat, id_proposal = "proposal",
                           id_assessor = "assessor",
                           grade_variable = "num_grade",
                           inits_type = "overdispersed",
                           heterogeneous_residuals = TRUE,
                           n_chains = 4, n_adapt = 10000, n_iter = 10000,
                           n_burnin = 10000, max_iter = 10000)
  expect_equal(class(test$samples), "mcmc")

  test <- get_mcmc_samples(data = dat, id_proposal = "proposal",
                           id_assessor = "assessor",
                           grade_variable = "num_grade",
                           heterogeneous_residuals = TRUE,
                           n_chains = 4, n_adapt = 10000, n_iter = 10000,
                           n_burnin = 10000, max_iter = 10000)
  expect_equal(class(test$samples), "mcmc")

  test <- get_mcmc_samples(data = dat, id_proposal = "proposal",
                           id_assessor = "assessor",
                           grade_variable = "num_grade",
                           inits_type = "overdispersed",
                           n_chains = 4, n_adapt = 10000, n_iter = 10000,
                           n_burnin = 10000, max_iter = 10000)
  expect_equal(class(test$samples), "mcmc")

  test <- get_mcmc_samples(data = dat, id_proposal = "proposal",
                           id_assessor = "assessor",
                           grade_variable = "num_grade",
                           point_scale = 6,
                           ordinal_scale = TRUE,
                           n_chains = 4, n_adapt = 10000, n_iter = 10000,
                           n_burnin = 10000, max_iter = 10000)
  expect_equal(class(test$samples), "mcmc")

  test <- get_mcmc_samples(data = dat, id_proposal = "proposal",
                           id_assessor = "assessor",
                           grade_variable = "num_grade",
                           inits_type = "overdispersed",
                           point_scale = 6,
                           ordinal_scale = TRUE,
                           n_chains = 4, n_adapt = 10000, n_iter = 10000,
                           n_burnin = 10000, max_iter = 10000)
  expect_equal(class(test$samples), "mcmc")


  test <- get_mcmc_samples(data = dat, id_proposal = "proposal",
                           id_assessor = "assessor",
                           grade_variable = "num_grade",
                           inits_type = "overdispersed",
                           heterogeneous_residuals = TRUE,
                           point_scale = 6,
                           ordinal_scale = TRUE,
                           n_chains = 4, n_adapt = 10000, n_iter = 10000,
                           n_burnin = 10000, max_iter = 10000)
  expect_equal(class(test$samples), "mcmc")
})
