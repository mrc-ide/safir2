library(individual)
library(mockery)

expect_targeted_listener <- function(listener, n, t, target) {
  mock_args <- mockery::mock_args(listener)
  expect_equal(mock_args[[n]][[1]], t)
  expect_equal(mock_args[[n]][[2]]$to_vector(), target)
}


test_that("test add_named_listener works with Event", {
  event <- Event$new()
  fn1 <- mockery::mock()
  expect_error(add_named_listener(event = event, name = 5L, listener = fn1))
  expect_error(add_named_listener(event = list(), name = 5L, listener = fn1))
  expect_error(add_named_listener(event = event, name = 5L, listener = 5))
  expect_error(add_named_listener(event = event, name = "fn1", listener = 5))

  add_named_listener(event = event, name = "fn1", listener = fn1)
  expect_equal(names(event$.listeners), "fn1")
  event$schedule(0)
  event$.process()
  event$.tick()
  expect_args(fn1, n = 1, t = 1)

  fn2 <- mockery::mock()
  expect_error(add_named_listener(event = event, name = "fn1", listener = fn2))
  add_named_listener(event = event, name = "fn2", listener = fn2)
  event$schedule(0)
  event$.process()
  event$.tick()
  expect_args(fn1, n = 1, t = 1)
  expect_args(fn1, n = 2, t = 2)
  expect_args(fn2, n = 1, t = 2)
  expect_equal(length(fn1), 2)
  expect_equal(length(fn2), 1)

})


test_that("test add_named_listener works with TargetedEvent", {
  event <- TargetedEvent$new(10)
  fn1 <- mockery::mock()
  expect_error(add_named_listener(event = event, name = 5L, listener = fn1))
  expect_error(add_named_listener(event = list(), name = 5L, listener = fn1))
  expect_error(add_named_listener(event = event, name = 5L, listener = 5))
  expect_error(add_named_listener(event = event, name = "fn1", listener = 5))

  add_named_listener(event = event, name = "fn1", listener = fn1)
  expect_equal(names(event$.listeners), "fn1")
  event$schedule(target = Bitset$new(10)$insert(c(2,4)), delay = 0)
  event$.process()
  event$.tick()
  expect_targeted_listener(fn1, n = 1, t = 1, target = c(2, 4))

  fn2 <- mockery::mock()
  expect_error(add_named_listener(event = event, name = "fn1", listener = fn2))
  add_named_listener(event = event, name = "fn2", listener = fn2)
  event$schedule(target = Bitset$new(10)$insert(c(8,9)), delay = 0)
  event$.process()
  event$.tick()
  expect_targeted_listener(fn1, n = 2, t = 2, target = c(8, 9))
  expect_targeted_listener(fn2, n = 1, t = 2, target = c(8, 9))
  expect_equal(length(fn1), 2)
  expect_equal(length(fn2), 1)

})


test_that("test create_event_scheduler_listener works", {

  event <- TargetedEvent$new(population_size = 10)
  draw <- make_rerlang

  expect_error(create_event_scheduler_listener(event = NULL, duration = 5, func = draw, shift = 1, dt = 1))
  expect_error(create_event_scheduler_listener(event = event, duration = -5, func = draw, shift = 1, dt = 1))
  expect_error(create_event_scheduler_listener(event = event, duration = NULL, func = draw, shift = 1, dt = 1))
  expect_error(create_event_scheduler_listener(event = event, duration = 5, func = NULL, shift = 1, dt = 1))
  expect_error(create_event_scheduler_listener(event = event, duration = 5, func = draw, shift = NULL, dt = 1))
  expect_error(create_event_scheduler_listener(event = event, duration = 5, func = draw, shift = 5, dt = NULL))

  sched <- create_event_scheduler_listener(event = event, duration = 5, func = draw, shift = 1, dt = 1)
  expect_true(inherits(sched, "function"))
})


test_that("test create_state_update_listener", {

  states <- CategoricalVariable$new(categories = c("A", "B"), initial_values = rep(c("A", "B"), each = 5))

  expect_error(create_state_update_listener(states = states, destination = "X"))
  expect_error(create_state_update_listener(states = states, destination = NULL))
  expect_error(create_state_update_listener(states = NULL, destination = "B"))

  listen <- create_state_update_listener(states = states, destination = "B")
  expect_true(inherits(listen, "function"))
})
