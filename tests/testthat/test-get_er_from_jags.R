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
                           n_chains = 2, n_iter = 1000,
                           n_burnin = 1000)
  expect_equal(test$rankings %>%
                   nrow(), n_application)
})
