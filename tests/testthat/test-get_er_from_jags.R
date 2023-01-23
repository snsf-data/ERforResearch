test_that("The function produces a ranked matrix", {
  dat <- get_mock_data() %>%
    filter(panel == "p1")
  n_proposal <- dat %>%
    pull(proposal) %>%
    unique() %>%
    length()
  test <- get_er_from_jags(data = dat,
                           id_proposal = "proposal",
                           id_assessor = "assessor",
                           grade_variable = "num_grade",
                           n_chains = 4, n_adapt = 10000, n_iter = 10000,
                           n_burnin = 10000)
  expect_equal(test$rankings %>%
                   nrow(), n_proposal)
})
