test_that("create_variables returns the correct output", {

  pop <- get_population("AFG")

  pop$n <- as.integer(pop$n/10000)
  theages <- create_variables(pop, get_squire_parameters("AFG", dt = 1, population = pop$n))
  expect_length(length(theages$discrete_age), 1)

  expect_length(theages$discrete_age$get_values(), sum(pop$n))

})
