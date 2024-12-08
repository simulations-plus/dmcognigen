
good_decodes <- tibble::tribble(
  ~var,     ~decode,
  # compact
  "SEXF",   "0=Male\n1=Female",                                                        
  # yes/no without single quotes
  "LGL",    "Y=Yes\nN=No",                                                             
  # yes/no with single quotes
  "LGLQ",   "'Y'=Yes\n'N'=No",                                                         
  # with spaces and numbers
  "AGEPED", "0=Neonate to 1 month\n1=Infant: 1 month to 2 years",                      
  # with parentheses in decode
  "MDV",    "0=PK or PD measure\n1=Dose or Other(EVID=2)",                             
  # with special characters and parentheses
  "RFCAT",  "1=Normal (>=90 mL/min)\n2=Mild (60-89 mL/min)\n3=Moderate (30-59 mL/min)"
)

# decode_tbl -------------------------------------------------------------

complete_decode_tbl <- data.frame(
  var = "SEXF",
  lvl = c("0", "1"),
  lbl = c("Male", "Female")
)

complete_decode_tbl_different_order <- data.frame(
  var = "SEXF",
  lvl = c(1, 0),
  lbl = c("Female", "Male")
)

bad_decode_tbl <- complete_decode_tbl
names(bad_decode_tbl)[[1]] <- "notvar"

test_that("decode_tbl() outputs a decode_tbl", {
  # confirm decode_tbl adds the class decode_tbl on a valid decode_tbl
  expect_true(inherits(decode_tbl(complete_decode_tbl), "decode_tbl"))
  # confirm decode_tbl fails on an invalid decode_tbl
  expect_error(decode_tbl(bad_decode_tbl), regexp = "columns are missing")
})

# validate_decode_tbl -----------------------------------------------------

rep_lvl_decode <- data.frame(
  var = "SEXF",
  lvl = c("0", "1", "1"),
  lbl = c("Male", "Female", "Other")
)

dup_row_decode <- data.frame(
  var = "SEXF",
  lvl = c("0", "1", "1"),
  lbl = c("Male", "Female", "Female")
)

rep_lbl_decode <- data.frame(
  var = "SEXF",
  lvl = c("0", "1", "2"),
  lbl = c("Male", "Female", "Female")
)

test_that("validate_decode_tbl() validates a data.frame for a decode_tbl", {
  # should return the data.frame of 2 row and 3 columns
  expect_equal(dim(validate_decode_tbl(complete_decode_tbl)), c(2,3))
  # should error for not having the var column.
  expect_error(validate_decode_tbl(bad_decode_tbl), regexp = "columns are missing")
  # should error if multiple of the same lvl: same lbl
  expect_error(validate_decode_tbl(dup_row_decode), regexp = "variables include duplicate decode levels")
  # should error if multiple of the same lvl: different lbl
  expect_error(validate_decode_tbl(rep_lvl_decode), regexp = "variables include duplicate decode levels")
  # should warn if same lbl for different lvls.
  expect_warning(validate_decode_tbl(rep_lbl_decode), regexp = "variables include duplicate decode labels")
})

# as_decode_tbl -----------------------------------------------------------

test_that("as_decode_tbl() validates and returns a data.frame of class decode_tbl", {
  # confirm decode_tbl adds the class decode_tbl on a valid decode_tbl
  expect_true(inherits(as_decode_tbl(complete_decode_tbl), "decode_tbl"))
  # should error for not having the var column.
  expect_error(as_decode_tbl(bad_decode_tbl), regexp = "columns are missing")
  # should error if multiple of the same lvl: same lbl
  expect_error(as_decode_tbl(dup_row_decode), regexp = "variables include duplicate decode levels")
  # should error if multiple of the same lvl: different lbl
  expect_error(as_decode_tbl(rep_lvl_decode), regexp = "variables include duplicate decode levels")
  # should warn if same lbl for different lvls.
  expect_warning(as_decode_tbl(rep_lbl_decode), regexp = "variables include duplicate decode labels")
})

# print.decode_tbl --------------------------------------------------------

test_that("print methods execute", {
  expect_snapshot(decode_tbl(complete_decode_tbl))
})

test_that("empty decode_tbl has no print output", {
  expect_snapshot(decode_tbl())
})

test_that("printed decode_tbl is consistently arranged", {
  expect_snapshot(decode_tbl(complete_decode_tbl))
  expect_snapshot(decode_tbl(complete_decode_tbl_different_order))
})
