## code to prepare `dmcognigen_dose` dataset goes here

library(dplyr)
library(tidyr)
library(lubridate)

new_variable_labels <- c(
  DTTM = "Date/Time",
  DVID = "Observation Type",
  DVIDC = "Observation Type",
  EVID = "Event ID",
  MDV = "Missing Dependent Variable",
  DOSE = "Dose (mg)",
  TRT = "Treatment",
  ROUTE = "Route of Administration",
  AMT = "Amount (mg)",
  FDDTTM = "First Dose Date/Time",
  EDDTTM = "Last Dose Date/Time",
  DAY = "Day"
)


# from ex -----------------------------------------------------------------

pharmaversesdtm::ex %>% 
  cnt(EXTRT, EXDOSE, EXROUTE, EXDOSFRQ, n_distinct_vars = USUBJID)

pharmaversesdtm::ex %>% 
  cnt(VISIT, n_distinct_vars = USUBJID)

# Missing end times
pharmaversesdtm::ex %>% 
  group_by(USUBJID) %>%
  filter(any(is.na(EXENDTC))) %>%
  select(USUBJID, VISIT, VISITNUM, EXSTDTC, EXENDTC, EXDOSE) %>%
  split(~ USUBJID)

# Missing end dates imputed to start dates 

dose <- pharmaversesdtm::ex %>%
  mutate(
    start_date= lubridate::date(EXSTDTC),
    end_date = if_else(is.na(EXENDTC), start_date, lubridate::date(EXENDTC)),
    DOSE = EXDOSE,
    AMT = EXDOSE,
    ROUTE = EXROUTE,
    TRT = EXTRT,
    EVID = 1,
    DVID = 0,
    DVIDC = "Dose",
    MDV = 1,
    DAY = EXSTDY
  ) %>%
  arrange(STUDYID, USUBJID, start_date)


dose %>% 
  group_by(USUBJID) %>%
  filter(any(is.na(EXENDTC))) %>%
  select(USUBJID, VISIT, VISITNUM, start_date, end_date) %>%
  split(~ USUBJID)

dose %>% 
  cnt(DOSE, n_distinct_vars = USUBJID)

dose %>% 
  group_by(USUBJID) %>%
  filter(n_distinct(DOSE) > 1) %>%
  cnt(n_distinct_vars = USUBJID)


# Missing times set to midnight (based on structure of concentration data)
dose <- dose %>%
  mutate(
    DTTM = ymd_hms(start_date, truncated = 4)
  ) %>%
  group_by(USUBJID) %>%
  mutate(
    FDDTTM = min(DTTM),
    EDDTTM = max(DTTM)
  ) %>%
  ungroup() %>%
  select(-c(start_date, end_date))


dose %>%
  select(where(~ any(is.na(.x)))) %>%
  names()

dose %>%
  cnt(EDDTTM == FDDTTM, VISIT, DOSE, n_distinct_vars = USUBJID)


# labels and variable order -----------------------------------------------

dmcognigen_dose <- dose %>% 
  relocate(any_of(names(new_variable_labels)), .after = USUBJID) %>% 
  set_labels(new_variable_labels)

# dataset label
attr(dmcognigen_dose, "label") <- "CDISCPILOT01 Doses"

glimpse(dmcognigen_dose)

lapply(
  purrr::set_names(names(dmcognigen_dose)), 
  function(x) attr(dmcognigen_dose[[x]], "label")
)


# write -------------------------------------------------------------------

usethis::use_data(dmcognigen_dose, overwrite = TRUE)
