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


test_that("approx equality works", {
  expect_true(all(approx_equal(1:5, 1:5)))
  expect_true(all(approx_equal(as.numeric(1:5), as.numeric(1:5))))
  expect_false(approx_equal(5.0, 5.0 + sqrt(.Machine$double.eps)))
})


test_that("remove_non_numerics removes characters and characters of arrays", {
  actual <- remove_non_numerics(list(
    a = array(c('1', '2'), dim=c(1, 2)),
    b = c('1', '2'),
    c = c(1, 2)
  ))
  expect_equal(actual, list(c = c(1, 2)))
})

test_that("interpolating Rt works", {
  expect_error(interpolate_rt(dates = c(as.Date(x = "2/1/2020", format = "%m/%d/%Y"), as.Date(x = "1/1/2020", format = "%m/%d/%Y")),rt = c(1, 2)))
  expect_error(interpolate_rt(dates = c(as.Date(x = "2/1/2020", format = "%m/%d/%Y"), as.Date(x = "1/1/2020", format = "%m/%d/%Y")),rt = c(1, 2), max_date = as.Date(x = "6/1/2020", format = "%m/%d/%Y")))
  expect_error(interpolate_rt(dates = c(as.Date(x = "2/1/2020", format = "%m/%d/%Y"), as.Date(x = "4/1/2020", format = "%m/%d/%Y")),rt = c(1)))
  expect_error(interpolate_rt(dates = c(as.Date(x = "2/1/2020", format = "%m/%d/%Y"), as.Date(x = "4/1/2020", format = "%m/%d/%Y")),rt = c(1), max_date = as.Date(x = "6/1/2020", format = "%m/%d/%Y")))
  expect_error(interpolate_rt(dates = as.Date(x = "2/1/2020", format = "%m/%d/%Y"),rt = c(1, 2)))
  expect_error(interpolate_rt(dates = as.Date(x = "2/1/2020", format = "%m/%d/%Y"),rt = c(1, 2), max_date = as.Date(x = "6/1/2020", format = "%m/%d/%Y")))
  expect_error(interpolate_rt(dates = "2/1/2020",rt = c(1, 2)))
  expect_error(interpolate_rt(dates = c("2/1/2020", "4/1/2020"),rt = c(1, 2)))
  expect_error(interpolate_rt(dates = as.Date(c("2/1/2020", "4/1/2020"), format = "%m/%d/%Y") ,rt = c(NaN, 2)))
  expect_error(interpolate_rt(dates = as.Date(c("2/1/2020", "4/1/2020"), format = "%m/%d/%Y") ,rt = c(1, -5)))
  expect_error(interpolate_rt(dates = as.Date(c("2/1/2020", "4/1/2020"), format = "%m/%d/%Y") ,rt = c(1, NA)))
  expect_error(interpolate_rt(dates = as.Date(c("2/1/2020", "4/1/2020"), format = "%m/%d/%Y") ,rt = c("5", 2)))

  interp <- interpolate_rt(dates = as.Date(c("2/1/2020", "2/15/2020"), format = "%m/%d/%Y") ,rt = c(1, 2))
  expect_true(all(vapply(interp, length, integer(1)) == 15))
  expect_true(interp$Rt[1] == 1)
  expect_true(interp$Rt[15] == 2)
  expect_equal(interp$Rt_tt, 1:15)
})
