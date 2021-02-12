test_that("Default Bayesian hierarchical model can be build", {
  dat <- get_mock_data() %>%
    filter(panel == "p1")
  test <- get_mcmc_samples(data = dat, id_application = "application",
                           id_voter = "voter",
                           grade_variable = "num_grade",
                           n_chains = 2, n_iter = 1000,
                           n_burnin = 1000)
  expect_equal(class(test), "mcmc")
})
