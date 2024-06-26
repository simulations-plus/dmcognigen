
# equations table
calculations_table <- dplyr::tribble(
  ~var_name, ~expression, ~equation, ~reference, ~purpose,
  
  # first row
  "crcl",
  rlang::expr((((140 - age) * wtkg) / (72 * scr)) * ifelse(sexf == 1, 0.85, 1)),
  paste0(
    "  Males: CRCL [mL/min] = ((140 - AGE [y]) \u00d7 WTKG [kg]) \u00f7 (72 \u00d7 SCR [mg/dL])\n",
    "  Females: CRCL [mL/min] = (((140 - AGE [y]) \u00d7 WTKG [kg]) \u00f7 (72 \u00d7 SCR [mg/dL])) \u00d7 0.85\n"
  ),
  "Cockcroft DW, Gault MH. Prediction of creatinine clearance from serum creatinine. Nephron. 1976;16:31-41.",
  "Calculate Creatinine Clearance (CrCl), a measure of renal function based on age, weight, gender, and serum creatinine. This is the Cockcroft and Gault Formula.",
  
  "crcl_peck",
  rlang::expr(ifelse(wtkg < 1.2 * ibw, crcl, ((140 - age) * ibw) / (72 * scr) * ifelse(sexf == 1, 0.85, 1))),
  paste0(
    "  If WTKG [kg] < 120% of IBW [kg], CRCL [mL/min] = CRCL [mL/min] (Cockroft & Gault)\n",
    "  If WTKG [kg] >= 120% of IBW [kg]:\n",
    "    Males: CRCL [mL/min] = ((140 - AGE [y]) \u00d7 IBW [kg]) \u00f7 (72 \u00d7 SCR [mg/dL])\n",
    "    Females: CRCL [mL/min] = (((140 - AGE [y]) \u00d7 IBW [kg]) \u00f7 (72 \u00d7 SCR [mg/dL])) \u00d7 0.85\n"
  ),
  "Peck CC, Conner DP, Murphy MG. Simple Techniques for Individualizing Drug Therapy. Vancouver, WA. Applied Therapeutics, Inc. 1989.",
  "Calculate Creatinine Clearance (CrCl) using the Peck Formula, a measure of renal function based on age, ideal weight, gender, and serum creatinine",
  
  "bmi",
  rlang::expr(wtkg / (htcm / 100) ^ 2),
  paste0(
    "  BMI [kg/m^2] = WTKG [kg] \u00f7 (HTCM [cm] \u00f7 100) ^ 2\n"
  ),
  "[CDC](https://www.cdc.gov/healthyweight/assessing/bmi/adult_bmi/index.html)",
  "Calculate Body Mass Index (BMI). BMI is a measure of body fat based on height and weight",

  "bsa",
  rlang::expr(((htcm) ^ 0.42246 * (wtkg) ^ 0.51456) * 0.0235),
  paste0(
    "  BSA [m^2] = ((HTCM [cm]) ^ 0.42246 * (WTKG [kg]) ^ 0.51456) * 0.0235\n"
  ),
  "Gehan EA, George SL. Estimation of human body surface area from height and weight. Cancer Chemother Rep. 1970 Aug;54(4):225-35. PMID: 5527019",
  "Calculate Body Surface Area (BSA). Body Surface Area is a measure of body fat based on height and weight",
  
  "lbm",
  rlang::expr(ifelse(sexf == 0, (1.10 * wtkg) - 128 * ((wtkg) ^ 2 / (htcm) ^ 2),
                     (1.07 * wtkg) - 148 * ((wtkg) ^ 2 / (htcm) ^ 2))),
  paste0(
    "  Males: LBM [kg] = (1.10 \u00d7 WTKG [kg]) - 128 \u00d7 ((WTKG [kg]) ^ 2 \u00f7 (HTCM [cm]) ^ 2)\n",
    "  Females: LBM [kg] = (1.07 \u00d7 WTKG [kg]) - 148 \u00d7 ((WTKG [kg]) ^ 2 \u00f7 (HTCM [cm]) ^ 2)\n"
  ),
  paste0(
    "Hallynck TH, Soep HH, Thomis JA, et al. Should clearance be normalised to body surface or to lean body mass? Br J Clin Pharmacol. 1981;11:523-526\n\n",
    "James WPT. Research on obesity. London. Her Majesty's Stationery Office. 1976"
  ),
  "Calculate Lean Body Mass (LBM), an estimation of how much someone weighs without the body fat; how much a persons bones, organs and muscles weigh",
  
  "ibw",
  rlang::expr(ifelse(sexf == 0, 51.65 + 1.85 * ((htcm / 2.54) - 60),
                     48.67 + 1.65 * ((htcm / 2.54) - 60))),
  paste0(
    "  Males: IBW [kg] = 51.65 [kg] + 1.85 [kg] \u00d7 ((HTCM [cm] \u00f7 2.54) - 60)\n",
    "  Females: IBW [kg] = 48.67 [kg] + 1.65 [kg] \u00d7 ((HTCM [cm] \u00f7 2.54) - 60)\n"
  ),
  "Robinson JD, Lupkiewicz SM, Palenik L, Lopez LM, Ariet M. Determination of ideal body weight for drug dosage calculations. AmJHosp Pharm.  1983;40:1016-1019",
  "Calculate Ideal Body Weight (IBW), a measure of potential body fat based on height",
  
  "ibw_child",
  rlang::expr(((htcm) ^ 2 * 1.65) / 1000),
  paste0(
    "  IBW [kg] = [(HTCM [cm]) ^ 2 \u00d7 1.65] \u00f7 1000\n"
  ),
  "Traub SL and Johnson CE. Am J Hosp Pharm. 1980;37:195-201",
  "Calculate Ideal Body Weight (for children), a measure of potential body fat based on height",
  
  "egfr",
  # basically this is MDRD, but it should work for EGFRMDRT too, the scr and age need to be time varying
  # racen == 2 indicates 'African American', also formula from requirements template is used to write below formula, wiki page was not precise
  rlang::expr(175 * (scr ^ -1.154) * (age ^ -0.203) * ifelse(sexf == 1, 0.742, 1) * ifelse(racen == 2, 1.212, 1)),
  paste0(
    "  EGFR [mL/min/1.73m^2] = 175 \u00d7 (SCR [mg/dL] ^ -1.154) \u00d7 (AGE [y] ^ -0.203) \u00d7 (0.742 if female) \u00d7 (1.212 if African American)\n"
  ),
  "[Guidance for Industry: Pharmacokinetics in Patients with Impaired Renal Function \u2014 Study Design, Data Analysis, and Impact on Dosing and Labeling](https://www.fda.gov/media/78573/download)",
  "Calculate Estimated Glomerular Filtration Rate (eGFR) for adults",
  
  "egfr_child",
  # k coded inside calculate_egfr_child function
  rlang::expr((k * htcm) / scr),
  paste0(
    "  Children (AGE [y] <= 16), where:\n",
    "  EGFR = (k * HTCM [cm]) / SCR [mg/dL]\n",
    "  k = 0.45 for AGE [y] <= 1 (full term)\n",
    "  k = 0.55 for girls 1 < AGE [y] <= 16\n",
    "  k = 0.55 for boys 1 < AGE [y] < 13\n",
    "  k = 0.70 for boys 13 <= AGE [y] <= 16\n"
  ),
  "Schwartz GJ, Furth SL. Glomerular filtration rate measurement and estimation in chronic kidney disease. Pediatr Nephrol. 2007;22(11):1839-48.",
  "Calculate Estimated Glomerular Filtration Rate (eGFR) for Pedatrics using the Schwartz formula",
  
  "rfcat",
  rlang::expr(dplyr::case_when(
    egfr >= 90 ~ 1,
    egfr >= 60 & egfr <= 89 ~ 2,
    egfr >= 30 & egfr <= 59 ~ 3,
    egfr >= 15 & egfr <= 29 ~ 4,
    egfr < 15 ~ 5
  )),
  paste0(
    "  1 = Normal Function (>= 90 mL/min)\n",
    "  2 = Mild Impairment (60-89 mL/min)\n",
    "  3 = Moderate Impairment (30-59 mL/min)\n",
    "  4 = Severe Impairment (15-29 mL/min)\n",
    "  5 = End Stage Disease (< 15 mL/min or Dialysis)\n"
  ),
  "[Guidance for Industry: Pharmacokinetics in Patients with Impaired Renal Function \u2014 Study Design, Data Analysis, and Impact on Dosing and Labeling](https://www.fda.gov/media/78573/download)",
  "Calculate Renal Function Category (RFCAT)",
  
  "tbilcat",
  rlang::expr(dplyr::case_when(
    tbil <= tbiluln ~ 0,
    tbil > tbiluln & tbil <= (1.5 * tbiluln) ~ 1,
    tbil > (tbiluln * 1.5) & tbil <= (3 * tbiluln) ~ 2,
    tbil > (3 * tbiluln) ~ 3
  )),
  paste0(
    "  if TBIL [mg/dL] <= ULN [mg/dL] then TBILCAT = 0\n",
    "  if ULN [mg/dL] < TBIL [mg/dL] <= 1.5 \u00d7 ULN [mg/dL] then TBILCAT = 1\n",
    "  if 1.5 \u00d7 ULN [mg/dL] < TBIL [mg/dL] <= 3 \u00d7 ULN [mg/dL] then TBILCAT = 2\n",
    "  if TBIL [mg/dL] > 3 \u00d7 ULN [mg/dL] then TBILCAT = 3\n"
  ),
  paste0(
    "Ramanathan RK, Egorin MJ, Takimoto CHM, Remick SC, Doroshow JH, LoRusso PA, et al. Phase I and pharmacokinetic study of imatinib mesylate in patients with advanced malignancies and varying degrees of liver dysfunction: a study by the national cancer institute organ dysfunction working group. J Clin Oncol. 2008;26:563-9.\n\n",
    "Ramalingam SS, Kummar S, Sarantopoulos J, Shibata S, LoRusso P, Yerk M, et al. Phase I study of vorinostat in patients with advanced solid tumors and hepatic dysfunction: a National Cancer Institute Organ Dysfunction Working Group study. J Clin Oncol. 2010;28(29):4507-12."
  ),
  "Calculate Total Bilirubin Category",
  
  "astcat",
  rlang::expr(dplyr::case_when(
    ast <= astuln ~ 0,
    ast > astuln ~ 1
  )),
  paste0(
    "  if AST [U/L] <= ULN [U/L] then ASTCAT = 0\n  if ULN [U/L] < AST [U/L] then ASTCAT = 1\n"
  ),
  paste0(
    "Ramanathan RK, Egorin MJ, Takimoto CHM, Remick SC, Doroshow JH, LoRusso PA, et al. Phase I and pharmacokinetic study of imatinib mesylate in patients with advanced malignancies and varying degrees of liver dysfunction: a study by the national cancer institute organ dysfunction working group. J Clin Oncol. 2008;26:563-9.\n\n",
    "Ramalingam SS, Kummar S, Sarantopoulos J, Shibata S, LoRusso P, Yerk M, et al. Phase I study of vorinostat in patients with advanced solid tumors and hepatic dysfunction: a National Cancer Institute Organ Dysfunction Working Group study. J Clin Oncol. 2010;28(29):4507-12."
  ),
  "Calculate Aspartate Aminotransferase Category",
  
  "nciliv",
  rlang::expr(dplyr::case_when(
    tbilcat == 0 & astcat == 0 ~ 0,
    tbilcat == 0 & astcat == 1 ~ 1,
    tbilcat == 1 ~ 2,
    tbilcat == 2 ~ 3,
    tbilcat == 3 ~ 4,
  )),
  paste0(
    "  if TBILCAT = 0 and ASTCAT = 0 then NCILIV = 0\n",
    "  if TBILCAT = 0 and ASTCAT = 1 then NCILIV = 1\n",
    "  if TBILCAT = 1 then NCILIV = 2\n",
    "  if TBILCAT = 2 then NCILIV = 3\n",
    "  if TBILCAT = 3 then NCILIV = 4\n"
  ),
  paste0(
    "Ramanathan RK, Egorin MJ, Takimoto CHM, Remick SC, Doroshow JH, LoRusso PA, et al. Phase I and pharmacokinetic study of imatinib mesylate in patients with advanced malignancies and varying degrees of liver dysfunction: a study by the national cancer institute organ dysfunction working group. J Clin Oncol. 2008;26:563-9.\n\n",
    "Ramalingam SS, Kummar S, Sarantopoulos J, Shibata S, LoRusso P, Yerk M, et al. Phase I study of vorinostat in patients with advanced solid tumors and hepatic dysfunction: a National Cancer Institute Organ Dysfunction Working Group study. J Clin Oncol. 2010;28(29):4507-12."
  ),
  "Calculate NCI Liver Function Group (NCILIV)",
)

# params description table
params_table <- dplyr::tribble(
  ~param, ~description,
  
  "age",
  "Age (y). Numeric vector.",
  
  "wtkg",
  "Weight (kg). Numeric vector.",
  
  "htcm",
  "Height (cm). Numeric vector.",
  
  "scr",
  "Serum Creatinine (mg/dL). Numeric vector.",
  
  "sexf",
  "Sex. Numeric vector including values of 0 and/or 1. 0=Male; 1=Female.",
  
  "ibw",
  "Ideal Body Weight (kg). Numeric vector.",
  
  "racen",
  "Race. Numeric vector where the value 2 is expected to correspond to Black/African American.",
  
  "dv",
  "Dependent Variable. Numeric vector.",
  
  "egfr",
  "Estimated Glomerular Filtration Rate (mL/min/1.73m^2). Numeric vector.",
  
  "tbil",
  "Total Bilirubin (mg/dL). Numeric vector.",
  
  "ast",
  "Aspartate Aminotransferase (U/L). Numeric vector.",
  
  "uln",
  "Reference Range Upper Limit (same units as observed values). Numeric vector.",
  
  "tbilcat",
  "Total Bilirubin Category. Numeric vector.",
  
  "astcat",
  "Aspartate Aminotransferase Category. Numeric vector.",
  
  "crcl",
  "Creatinine Clearance (mL/min)",
  
  "return_num_vect",
  "This function returns a numeric vector the same length as its inputs"
)

#' pull details from calculations_table based on provided arguments
#' 
#' This function is mostly internal, but is used for documentation and is
#' therefore exported.
#'
#' @param .var_name variable name to extract a field from
#' @param .field field to extract
#'
#' @export
#' @keywords internal
get_from_calculations_table <- function(.var_name, .field){
  
  # set visible bindings
  var_name <- NULL
  
  # check inputs
  stopifnot(
    is.character(.var_name),
    length(.var_name) == 1,
    is.character(.field),
    length(.field) == 1
  )
  
  .field_found <- toupper(.field) %in% toupper(names(calculations_table))
  if(!.field_found){
    stop(glue::glue("`{.field}` variable not found in `calculations_table` \n
                    Available variables in `calculations_table`: {paste0(names(calculations_table), collapse = ', ')}"))
  }
  
  result <- calculations_table %>%
    dplyr::filter(
      toupper(var_name) == toupper(.var_name)
    ) %>%
    janitor::clean_names(case = "all_caps") %>% 
    dplyr::pull(!!toupper(.field))
  
  if(length(result) < 1) stop(glue::glue("`{.var_name}` didn't match any value, please check your inputs."))
  
  result
}

#' pull details from params table
#' 
#' This function is mostly internal, but is used for documentation and is
#' therefore exported.
#'
#' @param .param name of the parameter to describe
#'
#' @export
#' @keywords internal
describe_param <- function(.param){
  
  # set visible bindings
  param <- description <- NULL
  
  # check inputs
  stopifnot(
    is.character(.param),
    length(.param) == 1
  )
  
  result <- params_table %>%
    dplyr::filter(
      toupper(param) == toupper(.param)
    ) %>%
    dplyr::pull(description)
  
  if(length(result) < 1) stop(glue::glue("`{.param}` didn't match any value, please check your inputs."))
  
  result
}

# extract target evaluation from calculations table 
evaluate_calculation <- function(.var_name, envir_list){
  .expr <- get_from_calculations_table(.var_name, "expression")
  # calculation
  eval_result <- eval(.expr[[1]], envir = envir_list)
  
  # print equation
  report_calculation(.var_name)

  structure(eval_result, label = NULL)
}

# extract target evaluation from calculations table 
report_calculation <- function(.var_name){
  
  equation_msg <- "Formula to calculate "
  
  eq <- get_from_calculations_table(.var_name, "equation")
  
  message(
    equation_msg, toupper(.var_name), ": \n",
    eq
  )
}

# find variables from parent.frame
get_variable_from_env <- function(.vars, envir){

  # if the parent environment is the global environment, don't extract a default
  parent_is_global <- identical(globalenv(), envir)
  
  return_var <- NULL
  
  # set .vars to default if no argument is provided
  if(!parent_is_global) {
    return_var <- with(c(as.list(envir, all.names = TRUE), .vars = .vars), {
      # looping through missing arguments
      purrr::map(.vars, function(.var){
        # look up for matching variable in the environment
        envir_objects <- ls(envir, all.names = TRUE)
        .var_matches <- envir_objects[which(tolower(.var) == tolower(envir_objects))]
        if(length(.var_matches) < 1){
          stop(glue::glue("Following variables not found: {.var_matches}"))
        }else if(length(.var_matches) > 1){
          stop(glue::glue("More than one matches found for {.var}: {paste(.var_matches, collapse = ', ')}"))
        }else if(length(.var_matches) == 1){
          message(glue::glue("{.var_matches} variable found and used for the {.var} argument."))
          eval(as.name(.var_matches))
        }
        # set names to the return list
      }) %>% purrr::set_names(nm = .vars)
    })
  }
  
  return_var
}

# return NA if any argument is missing, return the passed argument otherwise.
check_args_missing <- function(args_in){
  
  # check missing on each arguments
  args_out <- purrr::map(args_in, function(arg){
    if(!rlang::is_missing(arg)) arg else NA
  })
  
  args_out
}

# function to warn about pediatric subjects being included in calculations for adults
warn_about_pediatrics <- function(age, age_min = 18) {
  if(any(age < age_min, na.rm = TRUE)) {
    cli::cli_warn("Cases where AGE < {age_min} were detected. This formula is intended for adults.")
  }
}

#' Calculate Creatinine Clearance
#'
#' @md 
#' @description `r get_from_calculations_table("crcl", "purpose")`
#' @details 
#' ```{r, echo=FALSE, comment=NA}
#' report_calculation("crcl")
#' ```
#' @references `r get_from_calculations_table("crcl", "reference")`
#' @param age `r describe_param("age")`
#' @param wtkg `r describe_param("wtkg")`
#' @param scr `r describe_param("scr")`
#' @param sexf `r describe_param("sexf")`
#'
#' @return `r describe_param("return_num_vect")`
#' @export
#' 
#' 
#' @seealso 
#' \code{\link{calculate_crcl_peck}} for Peck formula
#'
#' @examples
#' library(dplyr)
#' 
#' dmcognigen_cov %>% 
#'   mutate(CRCL = calculate_crcl(
#'     age = AGE, 
#'     wtkg = WTKG, 
#'     scr = SCR, 
#'     sexf = SEXF
#'   ))
#'
#' # Below will also work if the dataset contains expected variables
#' dmcognigen_cov %>% 
#'   mutate(CRCL = calculate_crcl())
#'
#' # Set a cap at some value, like 160
#' dmcognigen_cov %>% 
#'   mutate(CRCL = pmin(calculate_crcl(), 160))
calculate_crcl <- function(age, 
                           wtkg, 
                           scr, 
                           sexf){

  # get all arguments
  all_args <- as.list(environment())
  
  # check if any argument is missing
  args_list <- check_args_missing(all_args)
  
  missing_args <- names(args_list)[is.na(args_list)]
  
  # detect missing arguments
  detected_vars <- get_variable_from_env(missing_args, parent.frame()$.top_env)
  
  # combine provided and detected arguments
  args_list[is.na(args_list)] <- detected_vars
  
  # make sure all arguments are numeric
  list2env(args_list, envir = environment()) 
  admiraldev::assert_numeric_vector(age)
  admiraldev::assert_numeric_vector(wtkg)
  admiraldev::assert_numeric_vector(scr)
  admiraldev::assert_numeric_vector(sexf)
  
  warn_about_pediatrics(age)

  evaluate_calculation(
    "crcl",
    envir_list = args_list
  )
}

#' Calculate Creatinine Clearance using the Peck Formula
#'
#' @md 
#' @description `r get_from_calculations_table("crcl_peck", "purpose")`
#' @details 
#' ```{r, echo=FALSE, comment=NA}
#' report_calculation("crcl_peck")
#' ```
#' @references `r get_from_calculations_table("crcl_peck", "reference")`
#' @param age `r describe_param("age")`
#' @param wtkg `r describe_param("wtkg")`
#' @param crcl `r describe_param("crcl")`
#' @param ibw `r describe_param("ibw")`
#' @param scr `r describe_param("scr")`
#' @param sexf `r describe_param("sexf")`
#'
#' @return `r describe_param("return_num_vect")`
#' @export
#' 
#' @seealso 
#' \code{\link{calculate_crcl}} for Cockcroft and Gault formula
#'
#' @examples
#' library(dplyr)
#' 
#' dmcognigen_cov %>% 
#'   mutate(CRCLP = calculate_crcl_peck(
#'     age = AGE, 
#'     wtkg = WTKG, 
#'     crcl = CRCL, 
#'     ibw = IBW, 
#'     scr = SCR, 
#'     sexf = SEXF
#'   ))
#'
#' # Below will also work if the dataset contains expected variables
#' dmcognigen_cov %>% 
#'   mutate(CRCLP = calculate_crcl_peck())
calculate_crcl_peck <- function(age,
                                wtkg,
                                crcl,
                                ibw,
                                scr,
                                sexf){
  # get all arguments
  all_args <- as.list(environment())
  
  # check if any argument is missing
  args_list <- check_args_missing(all_args)
  missing_args <- names(args_list)[is.na(args_list)]
  
  # detect missing arguments
  detected_vars <- get_variable_from_env(missing_args, parent.frame()$.top_env)
  
  # combine provided and detected arguments
  args_list[is.na(args_list)] <- detected_vars
  
  # make sure all arguments are numeric
  list2env(args_list, envir = environment()) 
  admiraldev::assert_numeric_vector(age)
  admiraldev::assert_numeric_vector(wtkg)
  admiraldev::assert_numeric_vector(crcl)
  admiraldev::assert_numeric_vector(ibw)
  admiraldev::assert_numeric_vector(scr)
  admiraldev::assert_numeric_vector(sexf)
  
  warn_about_pediatrics(age)
  
  evaluate_calculation(
    "crcl_peck",
    envir_list = args_list
  )
}

#' Calculate Body Mass Index
#'
#' @md 
#' @description `r get_from_calculations_table("bmi", "purpose")`
#' @details 
#' ```{r, echo=FALSE, comment=NA}
#' report_calculation("bmi")
#' ```
#' @references `r get_from_calculations_table("bmi", "reference")`
#' @param wtkg `r describe_param("wtkg")`
#' @param htcm `r describe_param("htcm")`
#'
#' @return `r describe_param("return_num_vect")`
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' dmcognigen_cov %>% 
#'   mutate(BMI = calculate_bmi(wtkg = WTKG, htcm = HTCM))
#'
#' # Below will also work if the dataset contains expected variables
#' dmcognigen_cov %>% 
#'   mutate(BMI = calculate_bmi())
calculate_bmi <- function(wtkg, htcm){
  # get all arguments
  all_args <- as.list(environment())
  
  # check if any argument is missing
  args_list <- check_args_missing(all_args)
  missing_args <- names(args_list)[is.na(args_list)]
  
  # detect missing arguments
  detected_vars <- get_variable_from_env(missing_args, parent.frame()$.top_env)
  
  # combine provided and detected arguments
  args_list[is.na(args_list)] <- detected_vars
  
  # make sure all arguments are numeric
  list2env(args_list, envir = environment()) 
  admiraldev::assert_numeric_vector(wtkg)
  admiraldev::assert_numeric_vector(htcm)
  
  evaluate_calculation(
    "bmi",
    envir_list = args_list
  )
}

#' Calculate Body Surface Area
#'
#' @md 
#' @description `r get_from_calculations_table("bsa", "purpose")`
#' @details 
#' ```{r, echo=FALSE, comment=NA}
#' report_calculation("bsa")
#' ```
#' @references `r get_from_calculations_table("bsa", "reference")`
#' @param htcm `r describe_param("htcm")`
#' @param wtkg `r describe_param("wtkg")`
#'
#' @return `r describe_param("return_num_vect")`
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' dmcognigen_cov %>% 
#'   mutate(BSA = calculate_bsa(htcm = HTCM, wtkg = WTKG))
#'
#' # Below will also work if the dataset contains expected variables
#' dmcognigen_cov %>% 
#'   mutate(BSA = calculate_bsa())
calculate_bsa <- function(htcm, wtkg){
  # get all arguments
  all_args <- as.list(environment())
  
  # check if any argument is missing
  args_list <- check_args_missing(all_args)
  missing_args <- names(args_list)[is.na(args_list)]
  
  # detect missing arguments
  detected_vars <- get_variable_from_env(missing_args, parent.frame()$.top_env)
  
  # combine provided and detected arguments
  args_list[is.na(args_list)] <- detected_vars
  
  # make sure all arguments are numeric
  list2env(args_list, envir = environment()) 
  admiraldev::assert_numeric_vector(htcm)
  admiraldev::assert_numeric_vector(wtkg)
  
  evaluate_calculation(
    "bsa",
    envir_list = args_list
  )
}

#' Calculate Lean Body Mass
#'
#' @md 
#' @description `r get_from_calculations_table("lbm", "purpose")`
#' @details 
#' ```{r, echo=FALSE, comment=NA}
#' report_calculation("lbm")
#' ```
#' @references `r get_from_calculations_table("lbm", "reference")`
#' @param sexf `r describe_param("sexf")`
#' @param wtkg `r describe_param("wtkg")`
#' @param htcm `r describe_param("htcm")`
#'
#' @return `r describe_param("return_num_vect")`
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' dmcognigen_cov %>% 
#'   mutate(LBM = calculate_lbm(
#'     sexf = SEXF, 
#'     wtkg = WTKG, 
#'     htcm = HTCM
#'   ))
#'
#' # Below will also work if the dataset contains expected variables
#' dmcognigen_cov %>% 
#'   mutate(LBM = calculate_lbm())
calculate_lbm <- function(sexf, wtkg, htcm){
  # get all arguments
  all_args <- as.list(environment())
  
  # check if any argument is missing
  args_list <- check_args_missing(all_args)
  missing_args <- names(args_list)[is.na(args_list)]
  
  # detect missing arguments
  detected_vars <- get_variable_from_env(missing_args, parent.frame()$.top_env)
  
  # combine provided and detected arguments
  args_list[is.na(args_list)] <- detected_vars
  
  # make sure all arguments are numeric
  list2env(args_list, envir = environment()) 
  admiraldev::assert_numeric_vector(sexf)
  admiraldev::assert_numeric_vector(wtkg)
  admiraldev::assert_numeric_vector(htcm)
  
  evaluate_calculation(
    "lbm",
    envir_list = args_list
  )
}

#' Calculate Ideal Body Weight
#'
#' @md 
#' @description `r get_from_calculations_table("ibw", "purpose")`
#' @details 
#' ```{r, echo=FALSE, comment=NA}
#' report_calculation("ibw")
#' ```
#' @references `r get_from_calculations_table("ibw", "reference")`
#' @param sexf `r describe_param("sexf")`
#' @param htcm `r describe_param("htcm")`
#'
#' @return `r describe_param("return_num_vect")`
#' @export
#' 
#' @seealso 
#' \code{\link{calculate_ibw_child}}
#'
#' @examples
#' library(dplyr)
#' 
#' dmcognigen_cov %>% 
#'   mutate(IBW = calculate_ibw(sexf = SEXF, htcm = HTCM))
#'
#' # Below will also work if the dataset contains expected variables
#' dmcognigen_cov %>% 
#'   mutate(IBW = calculate_ibw())
calculate_ibw <- function(sexf, htcm){
  # get all arguments
  all_args <- as.list(environment())
  
  # check if any argument is missing
  args_list <- check_args_missing(all_args)
  missing_args <- names(args_list)[is.na(args_list)]
  
  # detect missing arguments
  detected_vars <- get_variable_from_env(missing_args, parent.frame()$.top_env)
  
  # combine provided and detected arguments
  args_list[is.na(args_list)] <- detected_vars
  
  # make sure all arguments are numeric
  list2env(args_list, envir = environment()) 
  admiraldev::assert_numeric_vector(sexf)
  admiraldev::assert_numeric_vector(htcm)
  
  evaluate_calculation(
    "ibw",
    envir_list = args_list
  )
}

#' Calculate Ideal Body Weight for Children
#'
#' @md 
#' @description `r get_from_calculations_table("ibw_child", "purpose")`
#' @details 
#' ```{r, echo=FALSE, comment=NA}
#' report_calculation("ibw_child")
#' ```
#' @references `r get_from_calculations_table("ibw_child", "reference")`
#' @param htcm `r describe_param("htcm")`
#'
#' @return `r describe_param("return_num_vect")`
#' @export
#' 
#' @seealso 
#' \code{\link{calculate_ibw}}
#'
#' @examples
#' library(dplyr)
#' 
#' dmcognigen_cov %>% 
#'   mutate(IBWCHILD = calculate_ibw_child(HTCM))
#'
#' # Below will also work if the dataset contains expected variables
#' dmcognigen_cov %>% 
#'   mutate(IBWCHILD = calculate_ibw_child())
calculate_ibw_child <- function(htcm){
  # get all arguments
  all_args <- as.list(environment())
  
  # check if any argument is missing
  args_list <- check_args_missing(all_args)
  missing_args <- names(args_list)[is.na(args_list)]
  
  # detect missing arguments
  detected_vars <- get_variable_from_env(missing_args, parent.frame()$.top_env)
  
  # combine provided and detected arguments
  args_list[is.na(args_list)] <- detected_vars
  
  # make sure all arguments are numeric
  list2env(args_list, envir = environment()) 
  admiraldev::assert_numeric_vector(htcm)
  
  evaluate_calculation(
    "ibw_child",
    envir_list = args_list
  )
}

#' Calculate Estimated Glomerular Filtration Rate (eGFR)
#'
#' @md 
#' @description `r get_from_calculations_table("egfr", "purpose")`
#' @details 
#' ```{r, echo=FALSE, comment=NA}
#' report_calculation("egfr")
#' ```
#' @references `r get_from_calculations_table("egfr", "reference")`
#' @param scr `r describe_param("scr")`
#' @param age `r describe_param("age")`
#' @param sexf `r describe_param("sexf")`
#' @param racen `r describe_param("racen")`
#'
#' @return `r describe_param("return_num_vect")`
#' @export
#' 
#' @seealso 
#' \code{\link{calculate_egfr_child}}
#'
#' @examples
#' library(dplyr)
#' 
#' dmcognigen_cov %>% 
#'   mutate(EGFR = calculate_egfr(
#'     scr = SCR, 
#'     age = AGE, 
#'     sexf = SEXF, 
#'     racen = RACEN
#'   ))
#'
#' # Below will also work if the dataset contains expected variables
#' dmcognigen_cov %>% 
#'   mutate(EGFR = calculate_egfr())
calculate_egfr <- function(scr,
                           age,
                           sexf,
                           racen){
  # get all arguments
  all_args <- as.list(environment())
  
  # check if any argument is missing
  args_list <- check_args_missing(all_args)
  missing_args <- names(args_list)[is.na(args_list)]
  
  # detect missing arguments
  detected_vars <- get_variable_from_env(missing_args, parent.frame()$.top_env)
  
  # combine provided and detected arguments
  args_list[is.na(args_list)] <- detected_vars
  
  # make sure all arguments are numeric
  list2env(args_list, envir = environment()) 
  admiraldev::assert_numeric_vector(scr)
  admiraldev::assert_numeric_vector(age)
  admiraldev::assert_numeric_vector(sexf)
  admiraldev::assert_numeric_vector(racen)
  
  warn_about_pediatrics(age, age_min = 16)
  
  evaluate_calculation(
    "egfr",
    envir_list = args_list
  )
}

#' Calculate Estimated Glomerular Filtration Rate for Children using the
#' Schwartz Formula
#'
#' @md 
#' @description `r get_from_calculations_table("egfr_child", "purpose")`
#' @details 
#' ```{r, echo=FALSE, comment=NA}
#' report_calculation("egfr_child")
#' ```
#' @references `r get_from_calculations_table("egfr_child", "reference")`
#' @param htcm `r describe_param("htcm")`
#' @param scr `r describe_param("scr")`
#' @param age `r describe_param("age")`
#' @param sexf `r describe_param("sexf")`
#'
#' @return `r describe_param("return_num_vect")`
#' @export
#' 
#' @seealso 
#' \code{\link{calculate_egfr}}
#'
#' @examples
#' library(dplyr)
#' 
#' dmcognigen_cov %>% 
#'   mutate(EGFRSCHW = calculate_egfr_child(
#'     htcm = HTCM, 
#'     scr = SCR, 
#'     age = AGE, 
#'     sexf = SEXF
#'   ))
#'
#' # Below will also work if the dataset contains expected variables
#' dmcognigen_cov %>% 
#'   mutate(EGFRSCHW = calculate_egfr_child())
calculate_egfr_child <- function(htcm,
                                 scr,
                                 age,
                                 sexf){
  # get all arguments
  all_args <- as.list(environment())
  
  # check if any argument is missing
  args_list <- check_args_missing(all_args)
  missing_args <- names(args_list)[is.na(args_list)]
  
  # detect missing arguments
  detected_vars <- get_variable_from_env(missing_args, parent.frame()$.top_env)
  
  # combine provided and detected arguments
  args_list[is.na(args_list)] <- detected_vars
  
  # make sure all arguments are numeric
  list2env(args_list, envir = environment()) 
  admiraldev::assert_numeric_vector(htcm)
  admiraldev::assert_numeric_vector(scr)
  admiraldev::assert_numeric_vector(age)
  admiraldev::assert_numeric_vector(sexf)
  
  # calculate k
  k <- dplyr::case_when(
    args_list$age <= 1 ~ 0.45,
    args_list$sexf == 1 & (args_list$age > 1 & args_list$age <= 16) ~ 0.55,
    args_list$sexf == 0 & (args_list$age > 1 & args_list$age < 13) ~ 0.55,
    args_list$sexf == 0 & (args_list$age >= 13 & args_list$age <= 16) ~ 0.70,
    TRUE ~ NA_real_
  )
  
  # add/keep only required arguments
  args_list <- args_list[-which(names(args_list) %in% c("age", "sexf"))]
  args_list$k <- k
  
  evaluate_calculation(
    "egfr_child",
    envir_list = args_list
  )
  
}


#' Calculate Renal Function Category
#'
#' @md 
#' @description `r get_from_calculations_table("rfcat", "purpose")`
#' @details 
#' ```{r, echo=FALSE, comment=NA}
#' report_calculation("rfcat")
#' ```
#' @references `r get_from_calculations_table("rfcat", "reference")`
#' @param egfr `r describe_param("egfr")`
#'
#' @return `r describe_param("return_num_vect")`
#' @export
#' 
#'
#' @examples
#' library(dplyr)
#' 
#' dmcognigen_cov %>% 
#'   mutate(RFCAT = calculate_rfcat(EGFR))
#'
#' # Below will also work if the dataset contains expected variables
#' dmcognigen_cov %>% 
#'   mutate(RFCAT = calculate_rfcat())
calculate_rfcat <- function(egfr){
  
  # get all arguments
  all_args <- as.list(environment())
  
  # check if any argument is missing
  args_list <- check_args_missing(all_args)
  missing_args <- names(args_list)[is.na(args_list)]
  
  # detect missing arguments
  detected_vars <- get_variable_from_env(missing_args, parent.frame()$.top_env)
  
  # combine provided and detected arguments
  args_list[is.na(args_list)] <- detected_vars
  
  list2env(args_list, envir = environment()) 
  admiraldev::assert_numeric_vector(egfr)
  
  evaluate_calculation(
    "rfcat",
    envir_list = args_list
  )
}

#' Calculate Total Bilirubin Category
#'
#' @md 
#' @description `r get_from_calculations_table("tbilcat", "purpose")`
#' @details 
#' ```{r, echo=FALSE, comment=NA}
#' report_calculation("tbilcat")
#' ```
#' @references `r get_from_calculations_table("tbilcat", "reference")`
#' @param tbil `r describe_param("tbil")`
#' @param tbiluln `r describe_param("uln")`
#'
#' @return `r describe_param("return_num_vect")`
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' dmcognigen_cov %>% 
#'   mutate(TBILCAT = calculate_tbilcat(tbil = TBIL, tbiluln = TBILULN))
#'
#' # Below will also work if the dataset contains expected variables
#' dmcognigen_cov %>% 
#'   mutate(TBILCAT = calculate_tbilcat())
calculate_tbilcat <- function(tbil, tbiluln){
  
  # get all arguments
  all_args <- as.list(environment())
  
  # check if any argument is missing
  args_list <- check_args_missing(all_args)
  missing_args <- names(args_list)[is.na(args_list)]
  
  # detect missing arguments
  detected_vars <- get_variable_from_env(missing_args, parent.frame()$.top_env)
  
  # combine provided and detected arguments
  args_list[is.na(args_list)] <- detected_vars
  
  # make sure all arguments are numeric
  list2env(args_list, envir = environment()) 
  admiraldev::assert_numeric_vector(tbil)
  admiraldev::assert_numeric_vector(tbiluln)
  
  evaluate_calculation(
    "tbilcat",
    envir_list = args_list
  )
}

#' Calculate Aspartate Aminotransferase Category
#'
#' @md 
#' @description `r get_from_calculations_table("astcat", "purpose")`
#' @details 
#' ```{r, echo=FALSE, comment=NA}
#' report_calculation("astcat")
#' ```
#' @references `r get_from_calculations_table("astcat", "reference")`
#' @param ast `r describe_param("ast")`
#' @param astuln `r describe_param("uln")`
#'
#' @return `r describe_param("return_num_vect")`
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' dmcognigen_cov %>%
#'   mutate(ASTCAT = calculate_astcat(ast = AST, astuln = ASTULN))
#'
#' # Below will also work if the dataset contains expected variables
#' dmcognigen_cov %>% 
#'   mutate(ASTCAT = calculate_astcat())
calculate_astcat <- function(ast, astuln){
  
  # get all arguments
  all_args <- as.list(environment())
  
  # check if any argument is missing
  args_list <- check_args_missing(all_args)
  missing_args <- names(args_list)[is.na(args_list)]
  
  # detect missing arguments
  detected_vars <- get_variable_from_env(missing_args, parent.frame()$.top_env)
  
  # combine provided and detected arguments
  args_list[is.na(args_list)] <- detected_vars
  
  # make sure all arguments are numeric
  list2env(args_list, envir = environment()) 
  admiraldev::assert_numeric_vector(ast)
  admiraldev::assert_numeric_vector(astuln)
  
  evaluate_calculation(
    "astcat",
    envir_list = args_list
  )
}

#' Calculate NCI Liver Function Group
#'
#' @md 
#' @description `r get_from_calculations_table("nciliv", "purpose")`
#' @details 
#' ```{r, echo=FALSE, comment=NA}
#' report_calculation("nciliv")
#' ```
#' @references `r get_from_calculations_table("nciliv", "reference")`
#' @param tbilcat `r describe_param("tbilcat")`
#' @param astcat `r describe_param("astcat")`
#'
#' @return `r describe_param("return_num_vect")`
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' dmcognigen_cov %>% 
#'   mutate(NCILIV = calculate_nciliv(tbilcat = TBILCAT, astcat = ASTCAT))
#'
#' # Below will also work if the dataset contains expected variables
#' dmcognigen_cov %>% 
#'   mutate(NCILIV = calculate_nciliv())
calculate_nciliv <- function(tbilcat, astcat){
  
  # get all arguments
  all_args <- as.list(environment())
  
  # check if any argument is missing
  args_list <- check_args_missing(all_args)
  missing_args <- names(args_list)[is.na(args_list)]
  
  # detect missing arguments
  detected_vars <- get_variable_from_env(missing_args, parent.frame()$.top_env)
  
  # combine provided and detected arguments
  args_list[is.na(args_list)] <- detected_vars
  
  # make sure all arguments are numeric
  list2env(args_list, envir = environment()) 
  admiraldev::assert_numeric_vector(tbilcat)
  admiraldev::assert_numeric_vector(astcat)
  
  evaluate_calculation(
    "nciliv",
    envir_list = args_list
  )
}
