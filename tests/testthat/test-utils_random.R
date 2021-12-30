test_that("Erlang sampling works", {
  expect_error(make_rerlang(mu = 0, dt = 1, shape = 2))
  expect_error(make_rerlang(mu = NaN, dt = 1, shape = 2))
  expect_error(make_rerlang(mu = 5, dt = 0, shape = 2))
  expect_error(make_rerlang(mu = 5, dt = NaN, shape = 2))
  expect_error(make_rerlang(mu = 5, dt = 1, shape = 0))
  expect_error(make_rerlang(mu = 5, dt = 1, shape = NaN))

  mu <- 50
  dt <- 0.01
  shape <- 2
  erlang <- make_rerlang(mu = mu, dt = dt, shape = shape)
  draw <- erlang(n = 1e4)

  r <- shape / mu
  shape / (r^2)

  expect_true(abs(mean(draw) - mu/dt) / (mu/dt) < 0.05)
  expect_true(abs(sqrt(shape / (r^2)) - sd(draw*dt)) / sqrt(shape / (r^2)) < 0.05)
})

test_that("exponential sampling works", {
  expect_error(make_rexp(mu = 0, dt = 1))
  expect_error(make_rexp(mu = NaN, dt = 1))
  expect_error(make_rexp(mu = 5, dt = 0))
  expect_error(make_rexp(mu = 5, dt = NaN))

  mu <- 50
  dt <- 0.01

  exponential <- make_rexp(mu = mu, dt = dt)
  draw <- exponential(n = 1e4)

  expect_true(abs(mean(draw) - mu/dt) / (mu/dt) < 0.05)
  expect_true(abs(sd(draw) - mu/dt) / (mu/dt) < 0.05)
})

