library(dplyr)

child_data <- tibble::tibble(
  ID = seq(1, 5, length.out = 5),
  HTCM = seq(10, 50, length.out = 5),
  AGE = seq(3, 15, length.out = 5),
  SCR = c(.75, .9, 1.4, 1.2, 1.1),
  SEXF = c(0, 1, 0, 1, 0)
)

# test evaluate_calculation() -------------------------------------------

test_that("evaluate_calculation() returns numeric value", {
  returned_result <- dmcognigen_cov %>% 
    mutate(
      CRCL = evaluate_calculation(
        .var_name = "crcl",
        envir_list = list(
          age = AGE,
          wtkg = WTKG,
          scr = SCR,
          sexf = SEXF)
        
      )
    )
  
  expect_type(
    object = returned_result$CRCL,
    type = "double"
  )
})

test_that("evaluate_calculation() returns expected result", {
  crcl_expected <- dmcognigen_cov %>% 
    mutate(
      CRCL = (((140 - AGE) * WTKG) / (72 * SCR)) * ifelse(SEXF == 1, 0.85, 1)
    )
  
  crcl_calculated <- dmcognigen_cov %>% 
    mutate(
      CRCL = evaluate_calculation(
        .var_name = "crcl",
        envir_list = list(
          age = AGE,
          wtkg = WTKG,
          scr = SCR,
          sexf = SEXF)
      )
    )
  
  expect_equal(
    object = crcl_calculated$CRCL,
    expected = crcl_expected$CRCL,
    ignore_attr = TRUE
  )
})

test_that("evaluate_calculation() returns error", {
  
  expect_error(evaluate_calculation("wrong argument"))
  
})



# test get_variable_from_env() --------------------------------------------

test_that("get_variable_from_env() returns expected result", {
  
  expected_list <- dmcognigen_cov %>%
    select(AGE, SEXF) %>% 
    purrr::map(., function(x){
      x
    }) %>% purrr::set_names(nm = names(.))
  
  returned_list <- get_variable_from_env(c("AGE", "SEXF"), dmcognigen_cov)
  
  expect_equal(
    object = returned_list,
    expected = expected_list,
    ignore_attr = TRUE
  )
})

test_that("get_variable_from_env() detects missing variables from parent environment", {
  
  expected_crcl <- dmcognigen_cov %>%
    mutate(
      CRCL = calculate_crcl(age = AGE, wtkg = WTKG, scr = SCR, sexf = SEXF)
    ) %>% 
    select(USUBJID, AGE, WTKG, SCR, SEXF)
  
  crcl_from_detected_vars <- dmcognigen_cov %>%
    mutate(
      # get_variable_from_env() gets called by calculate_crcl()
      CRCL = calculate_crcl()
    ) %>% 
    select(USUBJID, AGE, WTKG, SCR, SEXF)
  
  expect_equal(
    object = crcl_from_detected_vars,
    expected = expected_crcl,
    ignore_attr = TRUE
  )
})

test_that("get_variable_from_env() returns error when finds more than match", {
  
  expect_error(
    dmcognigen_cov %>% 
      mutate(
        # create duplicate of AGE variable (age), adding 1 to it
        age = AGE + 1
      ) %>% 
      mutate(
        crcl = calculate_crcl()
      )
  )
  
})

test_that("get_variable_from_env() returns error when no match found", {
  
  expect_error(
    dmcognigen_cov %>% 
      select(-AGE) %>% 
      mutate(
        crcl = calculate_crcl()
      )
  )
  
})

# test get_from_calculations_table() --------------------------------------

test_that("get_from_calculations_table() returns expected result", {
  
  expected_value <- paste0("  Males: CRCL [mL/min] = ((140 - AGE [y]) × WTKG [kg]) ÷ (72 × SCR [mg/dL])\n  Females: CRCL [mL/min] = (((140 - AGE [y]) × WTKG [kg]) ÷ (72 × SCR [mg/dL])) × 0.85\n")
  
  returned_value <- get_from_calculations_table("crcl", "equation")
  
  expect_equal(
    object = returned_value,
    expected = expected_value,
    ignore_attr = TRUE
  )
  
})

test_that("get_from_calculations_table() is case-insensitive", {
  
  expected_result <- get_from_calculations_table("crcl", "equation")
  
  returned_result <- get_from_calculations_table("CrCl", "Equation")
  
  expect_equal(
    object = returned_result,
    expected = expected_result,
    ignore_attr = TRUE
  )
})

test_that("get_from_calculations_table() returns error when any argument is not available", {
  
  expect_error(get_from_calculations_table("wrong arg 1", "wrong arg 2"))
  expect_error(get_from_calculations_table())
  
})


# test check_args_missing() -----------------------------------------------

test_that("check_args_missing() returns expected result", {
  x <- c(10, 20 , 30)
  y <- c(2, 3, 4)
  
  test_check_args_missing_manual <- function(x, y, z){
    result <- list(
      x = if(!missing(x)) x else NA,
      y = if(!missing(y)) y else NA,
      z = if(!missing(z)) z else NA
    )
    
    result
  }
  
  expected_result <- test_check_args_missing_manual(x, y)
  
  test_check_args_missing_auto <- function(x, y, z){
    func_args <- as.list(environment())
    checked_args <- check_args_missing(func_args)
    checked_args
  }
  
  returned_result <- test_check_args_missing_auto(x, y)
  
  expect_equal(
    object = returned_result,
    expected = expected_result,
    ignore_attr = TRUE
  )
  
})


# test describe_param() ---------------------------------------------------

test_that("describe_param() is case-insensitive", {
  
  expected_result <- describe_param("ULN")
  
  returned_result <- describe_param("uln")
  
  expect_equal(
    object = returned_result,
    expected = expected_result,
    ignore_attr = TRUE
  )
})

test_that("describe_param() returns error when any argument is not available", {
  
  expect_error(describe_param("wrong arg"))
  expect_error(describe_param())
  
})


# test calculate_crcl() ---------------------------------------------------

test_that("calculate_crcl() returns numeric value", {
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      CRCL = calculate_crcl(age = AGE, wtkg = WTKG, scr = SCR, sexf = SEXF)
    ) %>% 
    select(USUBJID, AGE, WTKG, SCR, SEXF, CRCL)
  
  expect_type(
    object = returned_result$CRCL,
    type = "double"
  )
  
})

test_that("calculate_crcl() returns exact match", {
  
  crcl_expected <- dmcognigen_cov %>% 
    mutate(
      CRCL = (((140 - AGE) * WTKG) / (72 * SCR)) * ifelse(SEXF == 1, 0.85, 1)
    ) %>% 
    select(USUBJID, AGE, WTKG, SCR, SEXF, CRCL)
  
  crcl_calculated <- dmcognigen_cov %>% 
    mutate(
      CRCL = calculate_crcl(age = AGE, wtkg = WTKG, scr = SCR, sexf = SEXF)
    ) %>% 
    select(USUBJID, AGE, WTKG, SCR, SEXF, CRCL)
  
  expect_equal(
    object = crcl_calculated$CRCL,
    expected = crcl_expected$CRCL,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_crcl() returns error", {
  
  expect_error(calculate_crcl("wrong argument"))
  
})

test_that("calculate_crcl() works with rowwise method", {

  vectorized_result <- dmcognigen_cov %>%
    head(200) %>% 
    mutate(
      CRCL = calculate_crcl(age = AGE, wtkg = WTKG, scr = SCR, sexf = SEXF)
    ) %>% pull(CRCL)
  
  rowwise_result <- dmcognigen_cov %>%
    head(200) %>% 
    rowwise() %>% 
    mutate(
      CRCL = suppressMessages(calculate_crcl(age = AGE, wtkg = WTKG, scr = SCR, sexf = SEXF))
    ) %>% pull(CRCL)
  
  expect_equal(
    object = vectorized_result,
    expected = rowwise_result,
    ignore_attr = TRUE
  )
})


# test calculate_crcl_peck() ----------------------------------------------

test_that("calculate_crcl_peck() returns numeric value", {
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      CRCLP = calculate_crcl_peck(age = AGE, wtkg = WTKG, crcl = CRCL, ibw = IBW, scr = SCR, sexf = SEXF)
    ) %>% 
    select(USUBJID, AGE, WTKG, CRCL, IBW, SCR, SEXF, CRCLP)
  
  expect_type(
    object = returned_result$CRCLP,
    type = "double"
  )
  
})

test_that("calculate_crcl_peck() returns exact match", {
  
  expected_result <- dmcognigen_cov %>% 
    mutate(
      CRCLP = case_when(
        WTKG < 1.2 * IBW ~ CRCL,
        WTKG >= 1.2 * IBW ~ ((140 - AGE) * IBW) / (72 * SCR) * ifelse(SEXF == 1, 0.85, 1)
      )
    ) %>% 
    select(USUBJID, AGE, IBW, SCR, SEXF, CRCLP)
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      CRCLP = calculate_crcl_peck(age = AGE, wtkg = WTKG, crcl = CRCL, ibw = IBW, scr = SCR, sexf = SEXF)
    ) %>% 
    select(USUBJID, AGE, WTKG, CRCL, IBW, SCR, SEXF, CRCLP)
  
  expect_equal(
    object = returned_result$CRCLP,
    expected = expected_result$CRCLP,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_crcl_peck() returns error", {
  
  expect_error(calculate_crcl_peck("wrong argument"))
  
})


# test calculate_bmi() ----------------------------------------------------

test_that("calculate_bmi() returns numeric value", {
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      BMI = calculate_bmi(wtkg = WTKG, htcm = HTCM)
    ) %>% 
    select(USUBJID, WTKG, HTCM, BMI)
  
  expect_type(
    object = returned_result$BMI,
    type = "double"
  )
  
})

test_that("calculate_bmi() returns exact match", {
  
  expected_result <- dmcognigen_cov %>% 
    mutate(
      BMI = WTKG / (HTCM / 100) ^ 2
    ) %>% 
    select(USUBJID, WTKG, HTCM, BMI)
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      BMI = calculate_bmi(wtkg = WTKG, htcm = HTCM)
    ) %>% 
    select(USUBJID, WTKG, HTCM, BMI)
  
  expect_equal(
    object = returned_result$BMI,
    expected = expected_result$BMI,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_bmi() returns error", {
  
  expect_error(calculate_bmi("wrong argument"))
  
})


# test calculate_bsa() ----------------------------------------------------

test_that("calculate_bsa() returns numeric value", {
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      BSA = calculate_bsa(htcm = HTCM, wtkg = WTKG)
    ) %>% 
    select(USUBJID, WTKG, HTCM, BSA)
  
  expect_type(
    object = returned_result$BSA,
    type = "double"
  )
  
})

test_that("calculate_bsa() returns exact match", {
  
  expected_result <- dmcognigen_cov %>% 
    mutate(
      BSA = ((HTCM) ^ 0.42246 * (WTKG) ^ 0.51456) * 0.0235
    ) %>% 
    select(USUBJID, WTKG, HTCM, BSA)
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      BSA = calculate_bsa(htcm = HTCM, wtkg = WTKG)
    ) %>% 
    select(USUBJID, WTKG, HTCM, BSA)
  
  expect_equal(
    object = returned_result$BSA,
    expected = expected_result$BSA,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_bsa() works with missing variables", {
  
  expected_result <- dmcognigen_cov %>% 
    mutate(
      BSA = calculate_bsa(htcm = HTCM, wtkg = WTKG)
    ) %>% 
    select(USUBJID, WTKG, HTCM, BSA)
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      BSA = calculate_bsa()
    ) %>% 
    select(USUBJID, WTKG, HTCM, BSA)
  
  expect_equal(
    object = returned_result$BSA,
    expected = expected_result$BSA,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_bsa() returns error", {
  
  expect_error(calculate_bsa("wrong argument"))
  
})


# test calculate_lbm() ----------------------------------------------------

test_that("calculate_lbm() returns numeric value", {
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      LBM = calculate_lbm(sexf = SEXF, wtkg = WTKG, htcm = HTCM)
    ) %>% 
    select(USUBJID, SEXF, WTKG, HTCM, LBM)
  
  expect_type(
    object = returned_result$LBM,
    type = "double"
  )
  
})

test_that("calculate_lbm() returns exact match", {
  
  expected_result <- dmcognigen_cov %>% 
    mutate(
      LBM = ifelse(SEXF == 0, (1.10 * WTKG) - 128 * ((WTKG) ^ 2 / (HTCM) ^ 2),
                   (1.07 * WTKG) - 148 * ((WTKG) ^ 2 / (HTCM) ^ 2))
    ) %>% 
    select(USUBJID, SEXF, WTKG, HTCM, LBM)
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      LBM = calculate_lbm(sexf = SEXF, wtkg = WTKG, htcm = HTCM)
    ) %>% 
    select(USUBJID, SEXF, WTKG, HTCM, LBM)
  
  expect_equal(
    object = returned_result$LBM,
    expected = expected_result$LBM,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_lbm() works with missing variables", {
  
  expected_result <- dmcognigen_cov %>% 
    mutate(
      LBM = calculate_lbm(sexf = SEXF, wtkg = WTKG, htcm = HTCM)
    ) %>% 
    select(USUBJID, SEXF, WTKG, HTCM, LBM)
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      LBM = calculate_lbm()
    ) %>% 
    select(USUBJID, SEXF, WTKG, HTCM, LBM)
  
  expect_equal(
    object = returned_result$LBM,
    expected = expected_result$LBM,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_lbm() returns error", {
  
  expect_error(calculate_lbm("wrong argument"))
  
})


# test calculate_ibw() ----------------------------------------------------

test_that("calculate_ibw() returns numeric value", {
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      IBW = calculate_ibw(sexf = SEXF, htcm = HTCM)
    ) %>% 
    select(USUBJID, SEXF, WTKG, IBW)
  
  expect_type(
    object = returned_result$IBW,
    type = "double"
  )
  
})

test_that("calculate_ibw() returns exact match", {
  
  expected_result <- dmcognigen_cov %>% 
    mutate(
      IBW = ifelse(SEXF == 0, 51.65 + 1.85 * ((HTCM / 2.54) - 60),
                   48.67 + 1.65 * ((HTCM / 2.54) - 60))
    ) %>% 
    select(USUBJID, SEXF, WTKG, HTCM, IBW)
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      IBW = calculate_ibw(sexf = SEXF, htcm = HTCM)
    ) %>% 
    select(USUBJID, SEXF, HTCM, IBW)
  
  expect_equal(
    object = returned_result$IBW,
    expected = expected_result$IBW,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_ibw() works with missing variables", {
  
  expected_result <- dmcognigen_cov %>% 
    mutate(
      IBW = calculate_ibw(sexf = SEXF, htcm = HTCM)
    ) %>% 
    select(USUBJID, SEXF, HTCM, IBW)
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      IBW = calculate_ibw()
    ) %>% 
    select(USUBJID, SEXF, HTCM, IBW)
  
  expect_equal(
    object = returned_result$IBW,
    expected = expected_result$IBW,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_ibw() returns error", {
  
  expect_error(calculate_ibw("wrong argument"))
  
})


# test calculate_ibw_child() ----------------------------------------------

test_that("calculate_ibw_child() returns numeric value", {
  
  returned_result <- calculate_ibw_child(
    htcm = child_data$HTCM,
    age = child_data$AGE
  )
  
  expect_type(
    object = returned_result,
    type = "double"
  )
  
})

test_that("calculate_ibw_child() returns exact match", {
  
  expected_result <- ((child_data$HTCM) ^ 2 * 1.65) / 1000
  
  returned_result <- calculate_ibw_child(
    htcm = child_data$HTCM,
    age = child_data$AGE
  )
  
  expect_equal(
    object = returned_result,
    expected = expected_result,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_ibw_child() works with missing variables", {
  
  expected_result <- child_data %>%
    mutate(
      IBWCHILD = ((HTCM) ^ 2 * 1.65) / 1000
    )
  
  returned_result <- child_data %>% 
    mutate(
      IBWCHILD = calculate_ibw_child()
    )
  
  expect_equal(
    object = returned_result$IBWCHILD,
    expected = expected_result$IBWCHILD,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_ibw_child() returns error", {
  
  expect_error(calculate_ibw_child("wrong argument"))
  
})

test_that("calculate_ibw_child() warns about adults", {
  
  expect_warning(
    object = calculate_ibw_child(
      htcm = child_data$HTCM,
      age = 18.1
    ),
    regexp = "Cases where.*>.*"
  )
  
})


# test calculate_egfr() ---------------------------------------------------

test_that("calculate_egfr() returns numeric value", {
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      EGFR = calculate_egfr(scr = SCR, age = AGE, sexf = SEXF, racen = RACEN)
    ) %>% 
    select(USUBJID, SCR, AGE, SEXF, RACEN, EGFR)
  
  expect_type(
    object = returned_result$EGFR,
    type = "double"
  )
  
})

test_that("calculate_egfr() returns exact match", {
  
  expected_result <- dmcognigen_cov %>% 
    mutate(
      EGFR = 175 * (SCR ^ -1.154) * (AGE ^ -0.203) * ifelse(SEXF == 1, 0.742, 1) * ifelse(RACEN == 2, 1.212, 1)
    ) %>% 
    select(USUBJID, SCR, AGE, SEXF, RACEN, EGFR)
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      EGFR = calculate_egfr(scr = SCR, age = AGE, sexf = SEXF, racen = RACEN)
    ) %>% 
    select(USUBJID, SCR, AGE, SEXF, RACEN, EGFR)
  
  expect_equal(
    object = returned_result$EGFR,
    expected = expected_result$EGFR,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_egfr() works with missing variables", {
  
  expected_result <- dmcognigen_cov %>% 
    mutate(
      EGFR = calculate_egfr(scr = SCR, age = AGE, sexf = SEXF, racen = RACEN)
    ) %>% 
    select(USUBJID, SCR, AGE, SEXF, RACEN, EGFR)
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      EGFR = calculate_egfr()
    ) %>% 
    select(USUBJID, SCR, AGE, SEXF, RACEN, EGFR)
  
  expect_equal(
    object = returned_result$EGFR,
    expected = expected_result$EGFR,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_egfr() returns error", {
  
  expect_error(calculate_egfr("wrong argument"))
  
})


# test calculate_egfr_child() ---------------------------------------------

test_that("calculate_egfr_child() returns numeric value", {
  
  returned_result <- child_data %>% 
    mutate(
      EGFRSCHW = calculate_egfr_child(htcm = HTCM, scr = SCR, age = AGE, sexf = SEXF)
    ) %>% 
    select(ID, HTCM, SCR, AGE, SEXF, EGFRSCHW)
  
  expect_type(
    object = returned_result$EGFRSCHW,
    type = "double"
  )
  
})

test_that("calculate_egfr_child() returns exact match", {
  
  expected_result <- child_data %>% 
    mutate(
      k = dplyr::case_when(
        AGE <= 1 ~ 0.45,
        SEXF == 1 & (AGE > 1 & AGE <= 16) ~ 0.55,
        SEXF == 0 & (AGE > 1 & AGE < 13) ~ 0.55,
        SEXF == 0 & (AGE >= 13 & AGE <= 16) ~ 0.70,
        TRUE ~ NA_real_
      ),
      EGFRSCHW = (k * HTCM) / SCR
    ) %>% 
    select(ID, HTCM, SCR, AGE, SEXF, k, EGFRSCHW)
  
  returned_result <- child_data %>% 
    mutate(
      EGFRSCHW = calculate_egfr_child(htcm = HTCM, scr = SCR, age = AGE, sexf = SEXF)
    ) %>% 
    select(ID, HTCM, SCR, AGE, SEXF, EGFRSCHW)
  
  expect_equal(
    object = returned_result$EGFRSCHW,
    expected = expected_result$EGFRSCHW,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_egfr_child() works with missing variables", {
  
  expected_result <- child_data %>% 
    mutate(
      EGFRSCHW = calculate_egfr_child(htcm = HTCM, scr = SCR, age = AGE, sexf = SEXF)
    ) %>% 
    select(ID, HTCM, SCR, AGE, SEXF, EGFRSCHW)
  
  returned_result <- child_data %>% 
    mutate(
      EGFRSCHW = calculate_egfr_child()
    ) %>% 
    select(ID, HTCM, SCR, AGE, SEXF, EGFRSCHW)
  
  expect_equal(
    object = returned_result$EGFRSCHW,
    expected = expected_result$EGFRSCHW,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_egfr_child() returns error", {
  
  expect_error(calculate_egfr_child("wrong argument"))
  
})

test_that("calculate_egfr_child() warns about adults", {
  
  expect_warning(
    object = child_data %>%
      mutate(
        AGE = 18.1,
        EGFRSCHW = calculate_egfr_child(htcm = HTCM, scr = SCR, age = AGE, sexf = SEXF)
      ),
    regexp = "Cases where.*>.*"
  )
  
})


# test calculate_rfcat() --------------------------------------------------

test_that("calculate_rfcat() returns numeric value", {
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      RFCAT = calculate_rfcat(egfr = EGFR)
    ) %>% 
    select(USUBJID, EGFR, RFCAT)
  
  expect_type(
    object = returned_result$RFCAT,
    type = "double"
  )
  
})

test_that("calculate_rfcat() returns exact match", {
  
  expected_result <- dmcognigen_cov %>% 
    mutate(
      RFCAT = case_when(
        EGFR >= 90 ~ 1,
        EGFR >= 60 & EGFR <= 89 ~ 2,
        EGFR >= 30 & EGFR <= 59 ~ 3,
        EGFR >= 15 & EGFR <= 29 ~ 4,
        EGFR < 15 ~ 5
      )
    ) %>% 
    select(USUBJID, EGFR, RFCAT)
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      RFCAT = calculate_rfcat(egfr = EGFR)
    ) %>% 
    select(USUBJID, EGFR, RFCAT)
  
  expect_equal(
    object = returned_result$RFCAT,
    expected = expected_result$RFCAT,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_rfcat() works with missing variables", {
  
  expected_result <- dmcognigen_cov %>% 
    mutate(
      RFCAT = calculate_rfcat(egfr = EGFR)
    ) %>% 
    select(USUBJID, EGFR, RFCAT)
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      RFCAT = calculate_rfcat()
    ) %>% 
    select(USUBJID, EGFR, RFCAT)
  
  expect_equal(
    object = returned_result$RFCAT,
    expected = expected_result$RFCAT,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_rfcat() returns error", {
  
  expect_error(calculate_rfcat("wrong argument"))
  
})


# test calculate_tbilcat() ------------------------------------------------

test_that("calculate_tbilcat() returns numeric value", {
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      TBILCAT = calculate_tbilcat(tbil = TBIL, tbiluln = TBILULN)
    ) %>% 
    select(USUBJID, TBIL, TBILULN, TBILCAT)
  
  expect_type(
    object = returned_result$TBILCAT,
    type = "double"
  )
  
})

test_that("calculate_tbilcat() returns exact match", {
  
  expected_result <- dmcognigen_cov %>% 
    mutate(
      TBILCAT = case_when(
        TBIL <= TBILULN ~ 0,
        TBIL > TBILULN & TBIL <= (1.5 * TBILULN) ~ 1,
        TBIL > (TBILULN * 1.5) & TBIL <= (3 * TBILULN) ~ 2,
        TBIL > (3 * TBILULN) ~ 3
      )
    ) %>% 
    select(USUBJID, TBIL, TBILULN, TBILCAT)
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      TBILCAT = calculate_tbilcat(tbil = TBIL, tbiluln = TBILULN)
    ) %>% 
    select(USUBJID, TBIL, TBILULN, TBILCAT)
  
  expect_equal(
    object = returned_result$TBILCAT,
    expected = expected_result$TBILCAT,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_tbilcat() works with missing variables", {
  
  expected_result <- dmcognigen_cov %>% 
    mutate(
      TBILCAT = calculate_tbilcat(tbil = TBIL, tbiluln = TBILULN)
    ) %>% 
    select(USUBJID, TBIL, TBILULN, TBILCAT)
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      TBILCAT = calculate_tbilcat()
    ) %>% 
    select(USUBJID, TBIL, TBILULN, TBILCAT)
  
  expect_equal(
    object = returned_result$TBILCAT,
    expected = expected_result$TBILCAT,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_tbilcat() returns error", {
  
  expect_error(calculate_tbilcat("wrong argument"))
  
})


# test calculate_astcat() -------------------------------------------------

test_that("calculate_astcat() returns numeric value", {
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      ASTCAT = calculate_astcat(ast = AST, astuln = ASTULN)
    ) %>% 
    select(USUBJID, AST, ASTULN, ASTCAT)
  
  expect_type(
    object = returned_result$ASTCAT,
    type = "double"
  )
  
})

test_that("calculate_astcat() returns exact match", {
  
  expected_result <- dmcognigen_cov %>% 
    mutate(
      ASTCAT = case_when(
        AST <= ASTULN ~ 0,
        AST > ASTULN ~ 1
      )
    ) %>% 
    select(USUBJID, AST, ASTULN, ASTCAT)
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      ASTCAT = calculate_astcat(ast = AST, astuln = ASTULN)
    ) %>% 
    select(USUBJID, AST, ASTULN, ASTCAT)
  
  expect_equal(
    object = returned_result$ASTCAT,
    expected = expected_result$ASTCAT,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_astcat() works with missing variables", {
  
  expected_result <- dmcognigen_cov %>% 
    mutate(
      ASTCAT = calculate_astcat(ast = AST, astuln = ASTULN)
    ) %>% 
    select(USUBJID, AST, ASTULN, ASTCAT)
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      ASTCAT = calculate_astcat()
    ) %>% 
    select(USUBJID, AST, ASTULN, ASTCAT)
  
  expect_equal(
    object = returned_result$ASTCAT,
    expected = expected_result$ASTCAT,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_astcat() returns error", {
  
  expect_error(calculate_astcat("wrong argument"))
  
})


# test calculate_nciliv() -------------------------------------------------

test_that("calculate_nciliv() returns numeric value", {
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      NCILIV = calculate_nciliv(tbilcat = TBILCAT, astcat = ASTCAT)
    ) %>% 
    select(USUBJID, TBILCAT, ASTCAT, NCILIV)
  
  expect_type(
    object = returned_result$NCILIV,
    type = "double"
  )
  
})

test_that("calculate_nciliv() returns exact match", {
  
  expected_result <- dmcognigen_cov %>% 
    mutate(
      NCILIV = dplyr::case_when(
        TBILCAT == 0 & ASTCAT == 0 ~ 0,
        TBILCAT == 0 & ASTCAT == 1 ~ 1,
        TBILCAT == 1 ~ 2,
        TBILCAT == 2 ~ 3,
        TBILCAT == 3 ~ 4,
      )
    ) %>% 
    select(USUBJID, TBILCAT, ASTCAT, NCILIV)
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      NCILIV = calculate_nciliv(tbilcat = TBILCAT, astcat = ASTCAT)
    ) %>% 
    select(USUBJID, TBILCAT, ASTCAT, NCILIV)
  
  expect_equal(
    object = returned_result$NCILIV,
    expected = expected_result$NCILIV,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_nciliv() works with missing variables", {
  
  expected_result <- dmcognigen_cov %>% 
    mutate(
      NCILIV = calculate_nciliv(tbilcat = TBILCAT, astcat = ASTCAT)
    ) %>% 
    select(USUBJID, TBILCAT, ASTCAT, NCILIV)
  
  returned_result <- dmcognigen_cov %>% 
    mutate(
      NCILIV = calculate_nciliv()
    ) %>% 
    select(USUBJID, TBILCAT, ASTCAT, NCILIV)
  
  expect_equal(
    object = returned_result$NCILIV,
    expected = expected_result$NCILIV,
    ignore_attr = TRUE
  )
  
})

test_that("calculate_nciliv() returns error", {
  
  expect_error(calculate_nciliv("wrong argument"))
  
})

