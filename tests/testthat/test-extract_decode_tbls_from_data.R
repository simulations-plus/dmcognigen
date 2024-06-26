test_that("extract_decode_tbls_from_data() matches expected structure", {
  
  expect_length(
    object = extract_decode_tbls_from_data(
      dmcognigen_cov,
      lvl_to_lbl = c(SEXF = "SEXFC")
    ),
    n = 1L
  )
  
  expect_length(
    object = extract_decode_tbls_from_data(
      dmcognigen_cov,
      lvl_to_lbl = c(SEXF = "SEXFC", ARMCD = "ARM")
    ),
    n = 2L
  )
  
})