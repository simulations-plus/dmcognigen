## code to prepare `dmcognigen_cov` dataset goes here

library(dplyr)
library(tidyr)

new_variable_labels <- c(
  
  # derived from dm
  SEXF = "Sex",
  SEXFC = "Sex",
  RACEN = "Race",
  
  # derived from vs
  HTCM = "Height (cm)",
  WTKG = "Weight (kg)",
  
  # derived from lb
  AST = "Baseline AST (U/L)",
  ASTULN = "AST Upper Limit of Normal (U/L)",
  SCR = "Baseline SCR (mg/dL)",
  SCRULN = "SCR Upper Limit of Normal (mg/dL)",
  TBIL = "Baseline TBIL (mg/dL)",
  TBILULN = "TBIL Upper Limit of Normal (mg/dL)",
  
  # derived from calculations
  ASTCAT = "Baseline AST Category",
  BMI = "Baseline BMI (kg/m^2)",
  BSA = "Baseline BSA (m^2)",
  IBW = "Baseline IBW (kg)",
  CRCL = "Baseline CrCL (mL/min)",
  CRCLP = "Baseline CrCL (mL/min)",
  EGFR = "Baseline eGFR (mL/min/1.73m^2)",
  EGFRSCHW = "Baseline eGFR (mL/min/1.73m^2)",
  IBWCHILD = "Baseline IBW (kg)",
  LBM = "Baseline LBM (kg)",
  TBILCAT = "Baseline TBIL Category",
  RFCAT = "Baseline Renal Fx Category",
  NCILIV = "Baseline NCI Liver Fx Group"
  
)


# from dm dataset ---------------------------------------------------------

pharmaversesdtm::dm %>% 
  cnt(SEX)

pharmaversesdtm::dm%>% 
  cnt(RACE, ETHNIC)

dm <- pharmaversesdtm::dm %>% 
  select(STUDYID, USUBJID, SUBJID, AGE, SEX, RACE, ETHNIC, ARMCD, ARM, ACTARMCD, ACTARM, COUNTRY) %>% 
  mutate(
    SEXF = case_when(
      SEX == "F" ~ 1,
      SEX == "M" ~ 0
    ),
    SEXFC = case_when(
      SEXF == 1 ~ "Female",
      SEXF == 0 ~ "Male"
    ),
    RACEN = case_when(
      RACE == "WHITE" ~ 1,
      RACE == "BLACK OR AFRICAN AMERICAN" ~ 2,
      RACE == "ASIAN" ~ 3,
      RACE == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 4
    )
  ) %>% 
  arrange(STUDYID, USUBJID) %>% 
  relocate(SEXF, SEXFC, .after = SEX) %>% 
  relocate(RACEN, .after = RACE)

dm %>% 
  cnt(SEXF, SEXFC, SEX)

dm %>% 
  cnt(RACEN, RACE)

glimpse(dm)


# from vs dataset ---------------------------------------------------------

pharmaversesdtm::vs %>% 
  cnt(VSTESTCD, VSTEST, VSPOS, VSLOC)

pharmaversesdtm::vs %>% 
  filter(VSTESTCD %in% c("HEIGHT", "WEIGHT")) %>% 
  cnt(VSTESTCD, VSTEST, VSBLFL, VSORRESU, VSSTRESU, is.na(VSSTRESN))

pharmaversesdtm::vs %>% 
  cnt(VISITNUM, VISIT)

vs_bl_wide <- pharmaversesdtm::vs %>% 
  filter(VSTESTCD %in% c("HEIGHT", "WEIGHT")) %>% 
  group_by(STUDYID, USUBJID, VSTESTCD) %>% 
  # for each subject/testcd combination, keep the record flagged as baseline
  # if available. otherwise, use the screening visit.
  filter(
    case_when(
      any(VSBLFL == "Y") ~ VSBLFL == "Y",
      any(VISIT == "BASELINE") ~ VISIT == "BASELINE",
      any(VISIT == "SCREENING 2") ~ VISIT == "SCREENING 2",
      any(VISIT == "SCREENING 1") ~ VISIT == "SCREENING 1"
    )
  ) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = c(STUDYID, USUBJID),
    names_from = VSTESTCD,
    values_from = VSSTRESN
  ) %>% 
  rename(
    HTCM = HEIGHT,
    WTKG = WEIGHT
  )

glimpse(vs_bl_wide)


# from lb dataset ---------------------------------------------------------

pharmaversesdtm::lb %>% 
  cnt(LBTESTCD, LBTEST, LBORRESU, LBSTRESU) %>% 
  print(n = Inf)

# will be interested in: CREAT (SCR), BILI (TBIL), AST.
# for all, we want their original units.
# AST (U/L)
# SCR (mg/dL)
# TBIL (mg/dL)
pharmaversesdtm::lb %>% 
  filter(LBTESTCD %in% c("AST", "BILI", "CREAT")) %>% 
  cnt(LBTESTCD, LBTEST, LBORRESU, LBSTRESU)

# lower/upper limits
pharmaversesdtm::lb %>% 
  filter(LBTESTCD %in% c("AST", "BILI", "CREAT")) %>% 
  cnt(LBTESTCD, LBTEST, LBORRESU, LBORNRLO, LBORNRHI)

# baseline flags
pharmaversesdtm::lb %>% 
  filter(LBTESTCD %in% c("AST", "BILI", "CREAT")) %>% 
  cnt(LBTESTCD, LBTEST, LBBLFL)

pharmaversesdtm::lb %>% 
  filter(
    LBTESTCD %in% c("AST", "BILI", "CREAT"),
    LBBLFL == "Y"
  ) %>% 
  cnt(VISITNUM, VISIT, LBBLFL)

pharmaversesdtm::lb %>% 
  filter(LBTESTCD %in% c("AST", "BILI", "CREAT")) %>% 
  cnt(VISITNUM, VISIT) %>% 
  print(n = Inf)

lb_bl_wide <- pharmaversesdtm::lb %>% 
  filter(LBTESTCD %in% c("AST", "BILI", "CREAT")) %>% 
  group_by(STUDYID, USUBJID, LBTESTCD) %>% 
  filter(
    case_when(
      any(LBBLFL == "Y") ~ LBBLFL == "Y",
      any(LBDY < 0) ~ LBDY == min(LBDY),
    )
  ) %>% 
  ungroup() %>% 
  mutate(
    LBORRESN = as.numeric(LBORRES),
    ULN = as.numeric(LBORNRHI)
  ) %>% 
  pivot_wider(
    id_cols = c(STUDYID, USUBJID),
    names_from = LBTESTCD,
    values_from = c(LBORRESN, ULN)
  ) %>%
  rename(
    AST = LBORRESN_AST,
    ASTULN = ULN_AST,
    
    SCR = LBORRESN_CREAT,
    SCRULN = ULN_CREAT,
    
    TBIL = LBORRESN_BILI,
    TBILULN = ULN_BILI
  ) %>%
  relocate(STUDYID, USUBJID, AST, ASTULN, SCR, SCRULN, TBIL, TBILULN)

glimpse(lb_bl_wide)


# combine -----------------------------------------------------------------

dmcognigen_cov <- dm %>% 
  left_join(
    vs_bl_wide,
    by = c("STUDYID", "USUBJID")
  ) %>% 
  left_join(
    lb_bl_wide,
    by = c("STUDYID", "USUBJID")
  ) %>% 
  # add variables we have functions for
  mutate(
    ASTCAT = calculate_astcat(),
    TBILCAT = calculate_tbilcat(),
    BMI = calculate_bmi(),
    BSA = calculate_bsa(),
    IBW = calculate_ibw(),
    CRCL = calculate_crcl(),
    CRCLP = calculate_crcl_peck(),
    EGFR = calculate_egfr(),
    EGFRSCHW = calculate_egfr_child(),
    IBWCHILD = calculate_ibw_child(),
    LBM = calculate_lbm(),
    RFCAT = calculate_rfcat(),
    NCILIV = calculate_nciliv()
  )

dmcognigen_cov <- set_labels(dmcognigen_cov, new_variable_labels)

glimpse(dmcognigen_cov)

lapply(
  purrr::set_names(names(dmcognigen_cov)), 
  function(x) attr(dmcognigen_cov[[x]], "label")
)


# drop screen failure subjects --------------------------------------------

dmcognigen_cov %>% 
  cnt(ACTARMCD, ACTARM, is.na(HTCM), is.na(WTKG), is.na(SCR))

dmcognigen_cov %>% 
  cnt(STUDYID, n_distinct_vars = c(USUBJID, SUBJID))

dmcognigen_cov %>% 
  cnt(STUDYID, ACTARMCD, ACTARM, n_distinct_vars = c(USUBJID, SUBJID))

dmcognigen_cov <- dmcognigen_cov %>% 
  filter(
    ACTARMCD != "Scrnfail"
  )

dmcognigen_cov %>% 
  cnt(STUDYID, n_distinct_vars = c(USUBJID, SUBJID))

dmcognigen_cov %>% 
  cnt(STUDYID, ACTARMCD, ACTARM, n_distinct_vars = c(USUBJID, SUBJID))


# write -------------------------------------------------------------------

usethis::use_data(dmcognigen_cov, overwrite = TRUE)
