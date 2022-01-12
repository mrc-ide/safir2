test_that("basic event creation works", {

  pars <- get_squire_parameters("AFG", dt = 1, population = rep(10, 17))
  events <- create_events(parameters = pars)

  expect_equal(length(events), 17L)
  expect_true(all(vapply(X = events, FUN = function(x) {inherits(x, "TargetedEvent")}, FUN.VALUE = logical(1), USE.NAMES = FALSE)))

  pars <- list(a = 532)
  expect_error(create_events(parameters = pars))

})
