set.seed(1234)

good_data <- tibble::tibble(
  ID = 1, 
  TIME = 1:5,
  DV = rnorm(5)
)

good_labels <- list(
  ID = "Subject ID",
  TIME = "Time (h)",
  DV = "Concentration (nM)"
)

test_that("test general use", {
  
  data_general <- set_labels(good_data, good_labels)
  
  expect_identical(
    object = sapply(data_general, attr, which = "label"),
    expected = c(
      ID = "Subject ID",
      TIME = "Time (h)",
      DV = "Concentration (nM)"
    )
  )
  
})

test_that("test general use with named vector", {
  
  data_general <- set_labels(good_data, unlist(good_labels))
  
  expect_identical(
    object = sapply(data_general, attr, which = "label"),
    expected = c(
      ID = "Subject ID",
      TIME = "Time (h)",
      DV = "Concentration (nM)"
    )
  )
  
})

test_that("test in-place use", {
  
  data_general <- good_data
  set_labels(data_general) <- good_labels
  
  expect_identical(
    object = sapply(data_general, attr, which = "label"),
    expected = c(
      ID = "Subject ID",
      TIME = "Time (h)",
      DV = "Concentration (nM)"
    )
  )
  
})


test_that("test bad inputs", {
  
  labels <- tibble::tribble(
    ~var, ~label,
    "ID", "Subject ID",
    "TIME", "Time (h)",
    "DV", "Concentration (nM)"
  )
  
  expect_error(
    object = set_labels(good_data, labels),
    regexp = "labels must be a list"
  )
  
  bad_data <- 1:5
  
  expect_error(
    object = set_labels(bad_data, good_labels),
    regexp = "data must be a data.frame"
  )
  
})


test_that("test with missing name", {
  
  labels <- list(
    ID = "Subject ID",
    TIME = "Time (h)",
    DV = "Concentration (nM)",
    "Unnamed label"
  )
  
  expect_error(
    object = set_labels(good_data, labels),
    regexp = "All labels must be named"
  )
  
})


test_that("test with duplicated name", {
  
  labels <- list(
    ID = "Subject ID",
    TIME = "Time (h)",
    DV = "Concentration (nM)",
    DV = "Second DV label"
  )
  
  expect_error(
    object = set_labels(good_data, labels),
    regexp = "Multiple labels provided"
  )
  
})


test_that("test with name not in dataset", {
  
  labels <- list(
    ID = "Subject ID",
    TIME = "Time (h)",
    DV = "Concentration (nM)",
    DV2 = "Second DV label"
  )
  
  labelled_data <- set_labels(good_data, labels)
  
  expect_identical(
    object = sapply(labelled_data, attr, which = "label"),
    expected = c(
      ID = "Subject ID",
      TIME = "Time (h)",
      DV = "Concentration (nM)"
    )
  )
  
})


test_that("test with label over 40 characters", {
  
  labels <- list(
    ID = "Subject ID",
    TIME = "Time (h)",
    DV = "A long label that exceeds 40 characters. Which is of course worthy of a warning."
  )
  
  expect_warning(
    object = set_labels(good_data, labels),
    regexp = "labels with character length greater"
  )
  
})
