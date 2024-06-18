
data <- tibble::tribble(
  ~var1, ~var2, ~var3,
  "a",   1,     -99,
  "b",   2,     -99,
  "c",   3,     NA_real_,
  "a",   1,     -99,
  "b",   5,     -99,
  "c",   3,     NA_real_
)

test_that("test single variable", {
  
  result <- cnt(data, var1)
  
  expected_result <- tibble::tribble(
    ~var1, ~n,  ~n_cumulative,
    "a",    2L, 2L,
    "b",    2L, 4L,
    "c",    2L, 6L
  )
  
  expect_identical(
    object = result,
    expected = expected_result
  )
  
})

test_that("test prop and pct inclusion", {
  
  result <- cnt(data, var1, prop = TRUE, pct = TRUE)
  
  expect_named(
    object = result,
    expected = c("var1", "n", "prop", "pct", "n_cumulative")
  )
  
})


test_that("test multiple variables", {
  
  result <- cnt(data, var1, var2, var3)
  
  expected_result <- tibble::tribble(
    ~var1, ~var2, ~var3,    ~n,  ~n_cumulative,
    "a",   1,     -99,       2L, 2L,
    "b",   2,     -99,       1L, 3L,
    "b",   5,     -99,       1L, 4L,
    "c",   3,     NA_real_,  2L, 6L
  )
  
  expect_identical(
    object = result,
    expected = expected_result
  )
  
  
})


test_that("test with n_distinct", {
  
  result <- cnt(data, var1, n_distinct_vars = c(var2, var3))
  
  expected_result <- tibble::tribble(
    ~var1, ~n_var2, ~n_var3, ~n_var2_var3, ~n, ~n_cumulative,
    "a",   1L,      1L,      1L,           2L, 2L,
    "b",   2L,      1L,      2L,           2L, 4L,
    "c",   1L,      1L,      1L,           2L, 6L
  )
  
  expect_identical(
    object = result,
    expected = expected_result
  )
  
})

