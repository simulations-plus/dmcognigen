## code to prepare `dmcognigen_dose` dataset goes here

library(dplyr)
library(tidyr)
library(lubridate)

new_variable_labels <- c(
  DUR = "Duration (d)",
  FDDTTM =	"First Dose Date/Time",
  EDDTTM = "Last Dose Date/Time",
  DTTM = "Date/Time",
  ENDTTM="End Date/Time of Treatment",
  DOSENUM="Dose Number",
  DREG="Dosing Regimen",
  DREGC="Dosing Regimen",
  ROUTE="Route of Administration",
  TRT01A="Treatment",
  MDV="Missing Dependent Variable",
  DVID="Observation Type",
  DVIDC="Observation Type",
  DOSE="Dose (mg)",
  EVID="Event ID"
  )


pharmaversesdtm::ex %>% 
  cnt(VISIT,EXTRT,EXDOSE,EXROUTE,EXDOSFRQ)

# Missing end times
pharmaversesdtm::ex %>% 
  group_by(USUBJID) %>%
  filter(any(is.na(EXENDTC))) %>%
  select(USUBJID,VISIT,VISITNUM,EXSTDTC,EXENDTC,EXDOSE) %>%
  split(~USUBJID)

# Missing end dates imputed to start dates 

dose <- pharmaversesdtm::ex %>%
  mutate(
    start_date= lubridate::date(EXSTDTC),
    end_date = if_else(is.na(EXENDTC),start_date,lubridate::date(EXENDTC)),
    TRT01A = EXTRT,
    DOSE = EXDOSE,
    ROUTE = EXROUTE,
    DREG = c(QD = 1)[EXDOSFRQ],
    DREGC = EXDOSFRQ,
    DOSENUM = 1:n(),
    EVID=1,
    DVID=1,
    DVIDC="Dose",
    MDV=1
  ) %>%
  ungroup() %>%
  arrange(STUDYID, USUBJID, start_date)


dose %>% 
  group_by(USUBJID) %>%
  filter(any(is.na(EXENDTC))) %>%
  select(USUBJID,VISIT,VISITNUM,start_date,end_date) %>%
  split(~USUBJID)

dose %>% 
  cnt(STUDYID,DREGC)

dose %>% 
  cnt(DOSE,n_distinct_vars = USUBJID)

dose %>% 
  group_by(USUBJID) %>%
  filter(n_distinct(DOSE)>1) %>%
  cnt(n_distinct_vars = USUBJID)

# FDDTTM always BASELINE
dose %>%
  filter(start_date==end_date) %>%
  cnt(VISIT,DOSENUM)

# expand to daily records -------------------------------------------------

# Missing times set to midnight
dose_expanded <- dose %>%
  rowwise() %>%
  mutate(seq_of_dates = list(seq.Date(start_date, end_date, by = "day"))) %>%
  unnest(cols = c(seq_of_dates)) %>%
  mutate(DTTM = lubridate::ymd_hms(seq_of_dates, truncated = 4)) %>%
  group_by(USUBJID) %>%
  mutate(FDDTTM = min(DTTM),
         EDDTTM = max(DTTM)) %>%
  ungroup() %>%
  select(-c(seq_of_dates,start_date,end_date))


dose_expanded %>%
  select(where(~any(is.na(.)))) %>%
  names()

dose_expanded %>%
  cnt(EDDTTM==FDDTTM,VISIT,DOSENUM,
      DOSE,n_distinct_vars = USUBJID)

dose_expanded %>% 
  filter(USUBJID=="01-701-1148") %>%
  select(USUBJID,VISIT,VISITNUM,DTTM,EXSTDTC,EXENDTC,DOSE)%>%
  print(n=500)


dmcognigen_dose <- dmcognigen::set_labels(dose_expanded, new_variable_labels)

glimpse(dmcognigen_dose)




lapply(
  purrr::set_names(names(dmcognigen_dose)), 
  function(x) attr(dmcognigen_dose[[x]], "label")
)



# write -------------------------------------------------------------------

usethis::use_data(dmcognigen_dose, overwrite = TRUE)
