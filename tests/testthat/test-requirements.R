reqs_to_write <- example_requirements

asmbdat_directory <- file.path(tempdir(), "asmbdat")
dir.create(asmbdat_directory, showWarnings = FALSE)

openxlsx::write.xlsx(
  list(specs = reqs_to_write),
  file = file.path(asmbdat_directory, "data-requirements-2020-01-01.xlsx")
)

# include additional sheet (empty)
openxlsx::write.xlsx(
  list(specs = reqs_to_write, unit_conversions = data.frame()),
  file = file.path(asmbdat_directory, "data-requirements-2020-01-10.xlsx")
)

# copy of the requirements for QC purposes
openxlsx::write.xlsx(
  list(specs = reqs_to_write, unit_conversions = data.frame(), discussions = data.frame(), qc_findings = data.frame()),
  file = file.path(asmbdat_directory, "qc-data-requirements-2020-01-20.xlsx")
)

test_that("available_requirements_table() returns correct results", {
  
  expect_equal(
    object = available_requirements_table(
      path = asmbdat_directory,
      pattern = "req"
    ) |>
      nrow(),
    expected = 2L
  )
  
  expect_equal(
    object = available_requirements_table(
      path = asmbdat_directory,
      pattern = "req",
      drop_qc = FALSE
    ) |>
      nrow(),
    expected = 3L
  )
  
})

test_that("read_requirements() reads requirements and has attributes", {
  
  requirements <- read_requirements(asmbdat_directory)
  
  expect_s3_class(
    requirements,
    class = "requirements"
  )
  
  expect_s3_class(
    attr(requirements, "decode_tbls"),
    class = "decode_tbls"
  )
  
  expect_type(
    attr(requirements, "labels_named_list"),
    type = "list"
  )
  
  requirements_pk_mif <- read_requirements(
    asmbdat_directory,
    subset = pk_mif == "x"
  )
  
  expect_lte(
    nrow(requirements_pk_mif),
    nrow(requirements)
  )
  
})

test_that("join_decodes functions work with requirements", {
  
  requirements <- read_requirements(asmbdat_directory)
  
  cov <- dmcognigen_cov |>
    dplyr::select(USUBJID, RACEN) |> 
    join_decode_labels(requirements, lvl_to_lbl = list(RACEN = "RACEC"))
  
  expect_named(
    cov,
    c("USUBJID", "RACEN", "RACEC")
  )
  
  cov <- dmcognigen_cov |>
    dplyr::select(USUBJID, SEXFC) |> 
    join_decode_levels(requirements, lvl_to_lbl = list(SEXF = "SEXFC"))
  
  expect_named(
    cov,
    c("USUBJID", "SEXFC", "SEXF")
  )
  
})

test_that("set_labels() works with requirements", {
  
  requirements <- read_requirements(asmbdat_directory)
  
  empty_labels <- character(ncol(dmcognigen_cov)) |> 
    purrr::set_names(names(dmcognigen_cov))
  
  dmcognigen_cov_labels_removed <- dmcognigen_cov |> 
    set_labels(labels = empty_labels) |> 
    dplyr::select(STUDYID, USUBJID, RACEN)
  
  # apply the labels
  cov <- dmcognigen_cov_labels_removed |> 
    set_labels(labels = requirements)
  
  expect_equal(
    attr(cov$STUDYID, "label"),
    requirements$variable_label[requirements$variable_name == "STUDYID"]
  )
  
})
