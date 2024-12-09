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

test_that("test with no labels provided", {
  
  expect_identical(
    object = set_labels(dmcognigen_pk),
    expected = dmcognigen_pk
  )
  
})

test_that("test removing all labels", {
  
  expect_identical(
    object = unlist(sapply(set_labels(dmcognigen_pk, NULL), attr, which = "label")),
    expected = NULL
  )
  
  expect_identical(
    object = unlist(sapply(set_labels(dmcognigen_pk, NA), attr, which = "label")),
    expected = NULL
  )
  
  expect_identical(
    object = unlist(sapply(set_labels(dmcognigen_pk, ""), attr, which = "label")),
    expected = NULL
  )
  
  pk_no_labels <- dmcognigen_pk
  set_labels(pk_no_labels) <- NULL
  
  expect_identical(
    object = unlist(sapply(pk_no_labels, attr, which = "label")),
    expected = NULL
  )
  
})

test_that("test removing one label", {
  
  pk_remove_one_label <- set_labels(dmcognigen_pk, USUBJID = NULL)
  
  expect_identical(
    object = attr(pk_remove_one_label$USUBJID, "label"),
    expected = NULL
  )
  
  # other variable labels are not removed
  expect_type(
    object = attr(pk_remove_one_label$STUDYID, "label"),
    type = typeof(character())
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
  
  # with dots
  set_labels(data_general, ID = "ID") <- good_labels
  
  expect_identical(
    object = sapply(data_general, attr, which = "label"),
    expected = c(
      ID = "ID",
      TIME = "Time (h)",
      DV = "Concentration (nM)"
    )
  )
  
})

test_that("test data.frame input", {
  
  labels <- tibble::tribble(
    ~var, ~label,
    "ID", "Subject ID",
    "TIME", "Time (h)",
    "DV", "Concentration (nM)"
  )
  
  expect_message(
    object = set_labels(good_data, labels),
    regexp = "Inheriting labels from.*variables.*data.frame"
  )
  
})

test_that("test setting with all methods at once", {
  
  pk_none_then_all <- dmcognigen_pk %>% 
    set_labels(NULL) %>% 
    set_labels(
      ID = "ID", 
      USUBJID = dmcognigen_pk_requirements,
      STUDYID = dmcognigen_cov, 
      DNE = "dne", 
      DNCP = as.data.frame(dmcognigen_pk_requirements[, 1:2]),
      DOSE = 10
    )
  
  expect_identical(
    object = attr(pk_none_then_all$ID, "label"),
    expected = "ID"
  )
  
  expect_type(
    object = attr(pk_none_then_all$USUBJID, "label"),
    type = typeof(character())
  )
  
  expect_type(
    object = attr(pk_none_then_all$STUDYID, "label"),
    type = typeof(character())
  )
  
  expect_type(
    object = attr(pk_none_then_all$DNCP, "label"),
    type = typeof(character())
  )
  
  expect_type(
    object = attr(pk_none_then_all$DOSE, "label"),
    type = typeof(character())
  )
  
})

test_that("test bad inputs", {
  
  expect_error(
    object = set_labels(1:5, good_labels),
    regexp = "`data` must be a data.frame"
  )
  
  expect_error(
    # with requirements class removed
    object = set_labels(dmcognigen_pk, as.data.frame(dmcognigen_pk_requirements)),
    regexp = "No variable labels detected"
  )
  
  expect_error(
    object = set_labels(dmcognigen_pk, mean),
    regexp = "could not be interpreted"
  )
  
  expect_error(
    object = set_labels(dmcognigen_pk, dmcognigen_pk_requirements, USUBJID = mean),
    regexp = "could not be interpreted"
  )
  
  expect_error(
    object = set_labels(dmcognigen_pk, dmcognigen_pk_requirements, "orphan label"),
    regexp = "All elements.*should be named"
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
    regexp = "All labels should be named"
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
