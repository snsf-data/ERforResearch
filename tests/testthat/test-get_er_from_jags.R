test_that("The function produces a ranked matrix", {
  dat <- get_mock_data() %>%
    filter(panel == "p1")
  n_application <- dat %>%
    pull(application) %>%
    unique() %>%
    length()
  test <- get_er_from_jags(data = dat,
                           id_application = "application",
                           id_voter = "voter",
                           grade_variable = "num_grade",
                           n_chains = 4, n_adapt = 10000, n_iter = 10000,
                           n_burnin = 10000)
  expect_equal(test$rankings %>%
                   nrow(), n_application)
})
