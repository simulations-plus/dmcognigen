
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

# extract_decode_tbls -----------------------------------------------------

test_that("extract_decode_tbl() extracts the decodes with different formats/symbols", {
  
  # confirm first decode has 2 rows and 3 columns
  expect_equal(
    dim(extract_decode_tbl(
      variable_name = good_decodes$var[1],
      decode = good_decodes$decode[1]
    )), 
    c(2, 3)
  )
  
  # confirm second decode has 2 rows and 3 columns
  expect_equal(
    dim(extract_decode_tbl(
      variable_name = good_decodes$var[2],
      decode = good_decodes$decode[2]
    )), 
    c(2, 3)
  )
  
  # confirm third decode has 2 rows and 3 columns
  expect_equal(
    dim(extract_decode_tbl(
      variable_name = good_decodes$var[3],
      decode = good_decodes$decode[3]
    )), 
    c(2, 3)
  )
  
  # confirm fourth decode has 2 rows and 3 columns
  expect_equal(
    dim(extract_decode_tbl(
      variable_name = good_decodes$var[4],
      decode = good_decodes$decode[4]
    )), 
    c(2, 3)
  )
  
  # confirm fifth decode has 2 rows and 3 columns
  expect_equal(
    dim(extract_decode_tbl(
      variable_name = good_decodes$var[5],
      decode = good_decodes$decode[5]
    )), 
    c(2, 3)
  )
  
  # confirm sixth decode has 3 rows and 3 columns
  expect_equal(
    dim(extract_decode_tbl(
      variable_name = good_decodes$var[6],
      decode = good_decodes$decode[6]
    )), 
    c(3, 3)
  )
  
  expect_length(
    object = extract_decode_tbls(
      variable_name = good_decodes$var,
      decode = good_decodes$decode
    ),
    n = 6L
  )
  
})

test_that("extract_decode_tbl() matches expected structure and values", {
  expected_result <- tibble::tibble(
    var = "SEXF",
    lvl = c(0, 1), 
    lbl = c("Male", "Female")
  ) %>% 
    decode_tbl()
  
  expect_identical(
    object = extract_decode_tbl(
      good_decodes$var[1],
      good_decodes$decode[1]
    ),
    expected = expected_result
  )
})
