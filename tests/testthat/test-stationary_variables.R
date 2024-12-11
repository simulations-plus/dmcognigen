library(dplyr)

data("dmcognigen_cov")

test_that("no change when empty", {
  expect_identical(
    data.frame(),
    data.frame() |>
      distinct_stationary_variables()
  )
})


test_that("no change when stationary", {
  expect_identical(
    dplyr::distinct(dmcognigen_cov, STUDYID),
    dplyr::select(dmcognigen_cov, STUDYID) |>
      distinct_stationary_variables()
  )
})

test_that("no change when stationary by group", {
  expect_identical(
    dmcognigen_cov,
    dmcognigen_cov |>
      distinct_stationary_variables(USUBJID)
  )
})

test_that("no groups means one or fewer rows", {
  expect_lte(
    dmcognigen_cov |>
      distinct_stationary_variables() |>
      nrow(),
    1L
  )
})

test_that("magrittr pipe lets us use dot", {
  expect_true(
    dmcognigen_dose %>%
      cnt(across(stationary_variables(.)), n_distinct_vars = USUBJID) %>% 
      is.data.frame()
  )
  
  # with a variable
  expect_true(
    dmcognigen_dose %>%
      cnt(across(stationary_variables(., TRT)), n_distinct_vars = USUBJID) %>% 
      is.data.frame()
  )
  
})
