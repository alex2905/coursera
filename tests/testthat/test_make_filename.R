library(testthat)
context("Testing function make_filename")

test_that("make_filename output is correct",{
  # Expectation: output is a character string
  expect_is(make_filename(1), "character")
  # Expectation: Warning message due to input cannot be coerced to integer
  expect_warning(make_filename("hello"))
  # Expectation: Output is correct
  expect_equal(make_filename(2016), "inst/extdata/accident_2016.csv.bz2")
})

test_that("make_filename stops correctly",{
  # Expectation: functions stops and throws error if input cannot be coerced to
  # integer
  expect_error(make_filename(data.frame(c(1,2), c(3,4))))
})
