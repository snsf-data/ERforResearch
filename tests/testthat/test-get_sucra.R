test_that("The Sucra function gives a named vector as output.", {
  dat <- get_mock_data() %>%
    filter(panel == "p1")
  n_proposal <- dat %>%
    pull(proposal) %>%
    unique() %>%
    length()
  test <- get_sucra(data = dat,
                    id_proposal = "proposal",
                    id_assessor = "assessor",
                    grade_variable = "num_grade",
                    n_chains = 4, n_iter = 10000, n_adapt = 10000,
                    n_burnin = 10000)
  expect_equal(length(test$sucra), n_proposal)
})
