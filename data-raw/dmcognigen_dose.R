## code to prepare `dmcognigen_dose` dataset goes here

library(dplyr)
library(tidyr)
library(lubridate)


# requirements ------------------------------------------------------------

data("dmcognigen_pk_requirements")
requirements <- dmcognigen_pk_requirements

# for reference
requirements_decode_tbls <- as_decode_tbls(requirements)

requirements_decode_tbls[c("EVID", "DVID", "MDV")]


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


# join decodes ------------------------------------------------------------

dose <- dose %>%
  join_decode_labels(
    decode_tbls = requirements,
    lvl_to_lbl = "{var}C"
  )


# labels and variable order -----------------------------------------------

dmcognigen_dose <- dose %>% 
  relocate(any_of(requirements$variable_name), .after = USUBJID) %>% 
  set_labels(requirements)

# don't include variables without a label
missing_labels <- purrr::map(dmcognigen_dose, attr, which = "label") %>% 
  purrr::keep(is.null) %>% 
  names()

dmcognigen_dose <- dmcognigen_dose %>% 
  select(-any_of(missing_labels))

# dataset label
attr(dmcognigen_dose, "label") <- "CDISCPILOT01 Doses"

glimpse(dmcognigen_dose)

lapply(
  purrr::set_names(names(dmcognigen_dose)), 
  function(x) attr(dmcognigen_dose[[x]], "label")
)


# write -------------------------------------------------------------------

usethis::use_data(dmcognigen_dose, overwrite = TRUE)
