test_that("tests", {
  expect_equal(hello(), c("Hello, world!"))
  expect_equal(testcpp(5), rep(1,5))
})
