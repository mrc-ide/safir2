test_that("getting mixing matrix works in R and C++", {

  R0 <- 2
  time_period <- 20
  tt_contact_matrix <- 0

  pop <- get_population("AFG")
  pop$n <- as.integer(pop$n / 1000)

  psq <- get_squire_parameters(
    iso3c = "AFG",
    population = pop$n,
    R0 = R0,
    time_period = time_period,
    tt_contact_matrix = tt_contact_matrix,
    max_age = NULL,
    dt = 0.2
  )

  get_contact_R <- make_get_contact_matrix(psq)
  get_contact_cpp = make_get_contact_matrix_rcpp(parameters = psq)
  expect_equal(get_contact_R(timestep = 1), eval_get_contact_matrix_rcpp(func = get_contact_cpp, timestep = 1))
  expect_equal(get_contact_R(timestep = 10), eval_get_contact_matrix_rcpp(func = get_contact_cpp, timestep = 10))

})

test_that("getting time varying vector works in R and C++", {

  pars <- list(
    x = rexp(n = 10),
    y = 5,
    zz = "5",
    dt = 0.5
  )

  expect_error(make_get_vector(parameters = pars, name = "z"))
  expect_error(make_get_vector_rcpp(parameters = pars, name = "z"))

  expect_error(make_get_vector(parameters = pars, name = "zz"))
  expect_error(make_get_vector_rcpp(parameters = pars, name = "zz"))

  expect_error(make_get_vector(parameters = pars, name = "dt"))
  expect_error(make_get_vector_rcpp(parameters = pars, name = "dt"))

  get_par <- make_get_vector(parameters = pars, name = "y")
  get_par_cpp <- make_get_vector_rcpp(parameters = pars, name = "y")

  expect_equal(get_par(10), eval_get_vector_fn_rcpp(func = get_par_cpp, timestep = 10))

  get_par <- make_get_vector(parameters = pars, name = "x")
  get_par_cpp <- make_get_vector_rcpp(parameters = pars, name = "x")

  expect_equal(get_par(5), pars$x[3])
  expect_equal(get_par(5), eval_get_vector_fn_rcpp(func = get_par_cpp, timestep = 5))

})


test_that("getting time varying age-structured transition probabilities works in R and C++", {

  pars <- list(
    N_age = 17L,
    time_period = 10,
    x = matrix(runif(n = 17*10), nrow = 17, ncol = 10),
    y = runif(n = 17),
    zz = "5",
    dt = 0.5
  )

  expect_error(make_get_age_probabilities(parameters = pars, name = "z"))
  expect_error(make_get_age_probabilities_rcpp(parameters = pars, name = "z"))

  expect_error(make_get_age_probabilities(parameters = pars, name = "zz"))
  expect_error(make_get_age_probabilities_rcpp(parameters = pars, name = "zz"))

  expect_error(make_get_age_probabilities(parameters = pars, name = "dt"))
  expect_error(make_get_age_probabilities_rcpp(parameters = pars, name = "dt"))

  get_par <- make_get_age_probabilities(parameters = pars, name = "y")
  get_par_cpp <- make_get_age_probabilities_rcpp(parameters = pars, name = "y")

  ages <- sample(x = 17, size = 20, replace = TRUE)

  expect_equal(get_par(10, ages), eval_get_age_probabilities_fn_rcpp(func = get_par_cpp, timestep = 10, ages = ages))

  get_par <- make_get_age_probabilities(parameters = pars, name = "x")
  get_par_cpp <- make_get_age_probabilities_rcpp(parameters = pars, name = "x")

  expect_equal(get_par(5, ages), pars$x[cbind(ages, 3)])
  expect_equal(get_par(5, ages), eval_get_age_probabilities_fn_rcpp(func = get_par_cpp, timestep = 5, ages = ages))

})


