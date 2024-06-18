test_that("Empty environment returns NULL with a message", {
  expect_null(in_which("cyl"))
  expect_message(
    object = in_which("cyl"),
    regexp = "No matches.*"
  )
})


test_that("Invalid environment", {
  not_an_environment <- c(1, 2, 3)
  expect_error(
    object = in_which("cyl", not_an_environment), 
    regexp = "invalid 'envir' argument"
  )
})

test_that("No matches", {
  some_object <- c(1, 2, 3)
  expect_null(in_which("cyl", -1L))
})

test_that("Matches mtcars", {
  data(mtcars)
  expect_equal(
    object = in_which("cyl"), 
    expected = list(cyl = "mtcars")
  )
})
rm(list = ls())

test_that("Correct matches with multiple inputs", {
  data(mtcars)
  expect_equal(
    object = in_which(c("cyl", "not_in")), 
    expected = list(cyl = "mtcars", not_in = character(0))
  )
})
rm(list = ls())


test_that("Matches mtcars with multiple inputs", {
  data(mtcars)
  expect_equal(
    object = in_which(c("cyl", "disp")), 
    expected = list(cyl = "mtcars", disp = "mtcars")
  )
})
rm(list = ls())
