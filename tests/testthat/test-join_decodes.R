
decodes <- extract_decode_tbls(
  variable_name = example_requirements$variable_name, 
  decode = example_requirements$format_decode
)


# join_decodes_labels -----------------------------------------------------

test_that("join_decodes_labels() works", {
  
  expect_named(
    object = dmcognigen_cov |>
      dplyr::select(RACEN, SEXF) |>
      join_decode_labels(
        decode_tbls = decodes,
        lvl_to_lbl = c("RACEN" = "RACE", "SEXF" = "SEXFC")
      ),
    expected = c("RACEN", "SEXF", "RACE", "SEXFC")
  )
  
})

test_that("join_decodes_labels() warns about variables that already exist", {
  
  expect_warning(
    object = dmcognigen_cov |>
      dplyr::select(RACEN, SEXF, SEXFC) |>
      join_decode_labels(
        decode_tbls = decodes,
        lvl_to_lbl = c("RACEN" = "RACE", "SEXF" = "SEXFC")
      ),
    regexp = "already exists"
  )
  
})


# join_decode_levels ------------------------------------------------------

test_that("join_decode_levels() works", {
  
  expect_named(
    object = dmcognigen_cov |>
      dplyr::select(SEXFC) |>
      join_decode_levels(
        decode_tbls = decodes,
        lvl_to_lbl = c("SEXF" = "SEXFC")
      ),
    expected = c("SEXFC", "SEXF")
  )
  
})

test_that("join_decode_levels() warns about variables that already exist", {
  
  expect_warning(
    object = dmcognigen_cov |>
      dplyr::select(SEXFC, SEXF) |>
      join_decode_levels(
        decode_tbls = decodes,
        lvl_to_lbl = c("SEXF" = "SEXFC")
      ),
    regexp = "already exists"
  )
  
})
