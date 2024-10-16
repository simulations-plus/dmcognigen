## code to prepare `dmcognigen_pk` dataset goes here

library(dplyr)
library(tidyr)

new_variable_labels <- c(NTSFD = "Nominal Time Since First Dose (h)",
                         TSFD = "Time Since First Dose (h)",
                         TSPD = "Nominal Time Since Previous Dose (h)",
                         ONUM = "Overall Sequence Number",
                         NUM = "Sequence Number")

# load datasets -----------------------------------------------------------
cov <- dmcognigen_cov
conc <- dmcognigen_conc
dose <- dmcognigen_dose

existing_labels <- c(cov,dose,conc) %>%
  purrr::map(~attr(.x, "label")) 

# one per subj
cov %>%
cnt(n_distinct_vars = USUBJID)


intersect(names(cov),names(conc))

setdiff(unique(cov$USUBJID),unique(conc$USUBJID))
setdiff(y=unique(cov$USUBJID),unique(conc$USUBJID))

set.seed(45)
example_subj <- cov %>%
  group_by(ACTARM) %>%
  slice_sample(n=2) %>%
  pull(USUBJID)


cov %>%
filter(USUBJID %in% example_subj) %>%
cnt(USUBJID,ACTARM)

# DVID,DVIDC
# 1=Dose
# 2= XANOMELINE Plasma (ug/mL)
  
conc_cov <- conc %>%
  left_join(cov,by=c("STUDYID","USUBJID")) %>%
  mutate(NTSFD=PCTPTNUM) %>%
  arrange(USUBJID,DTTM,EVID,DVID)


conc_cov %>%
  filter(USUBJID %in% example_subj) %>%
  select(USUBJID, DTTM, VISIT, PCTPT, NTSFD, EVID, DVIDC, DV) %>%
  split( ~ USUBJID)



# dose --------------------------------------------------------------------

# looking at profiles- dose time should probably be imputed to midnight 
dose <- dose %>%
  mutate(
    DAY = case_when(VISIT == "BASELINE" ~ 1,
                    VISIT == "WEEK 2" ~ 14,
                    VISIT == "WEEK 24" ~ 168),
    NTSFD = case_when(
      VISIT == "BASELINE" ~ 0,
      VISIT == "WEEK 2" ~ 2 * 7 * 24,
      VISIT == "WEEK 24" ~ 24 * 7 * 24
    )
  )


pk <- conc_cov %>%
  bind_rows(dose) %>%
  arrange(USUBJID,DTTM,EVID,DVID)


pk %>%
  filter(USUBJID %in% example_subj) %>%
  select(USUBJID,DTTM,VISIT,PCTPT,NTSFD,EVID,DVIDC,DVID,DV) %>%
  split(~USUBJID)



pk <- pk %>%
  mutate(dose_times = if_else(EVID == 1, DTTM, NA_POSIXct_)) %>%
  group_by(USUBJID) %>%
  fill(FDDTTM, EDDTTM, !!!names(cov), ROUTE, DREG, DREGC, .direction = "downup") %>%
  fill(dose_times, DOSENUM,DOSE,.direction = "down") %>%
  mutate(
    TSFD = as.numeric(difftime(DTTM, FDDTTM), units = "hours"),
    TSPD = as.numeric(difftime(DTTM, dose_times), units = "hours"),
    trailing_dose = if_else(EVID == 0, 1, NA_real_)
  ) %>%
  fill(trailing_dose, .direction = "up") %>%
  mutate(trailing_dose = ifelse(is.na(trailing_dose) &
                                  EVID == 1, 1, 0)) %>%
  ungroup() %>%
  arrange(USUBJID, DTTM, EVID, DVID)


pk %>%
  cnt(trailing_dose,DVID,DOSE,n_distinct_vars = USUBJID)

# Remove trailing doses
# all of the 81 mg doses are trailing 

pk <- pk %>%
  filter(trailing_dose==0) 

pk %>%
  cnt(trailing_dose,DVID,DOSE,n_distinct_vars = USUBJID)


# add NUM and ONUM 
pk <- pk %>%
  group_by(USUBJID) %>%
  mutate(NUM=1:n())%>%
  ungroup()%>%
  mutate(ONUM=row_number(),
         MDV=EVID) %>%
  select(ONUM,NUM,any_of(names(cov)),TRT01A,
         contains("TTM"),DAY,VISIT,PCTPT,NTSFD,TSFD,TSPD,DVID,DV,DOSE,everything(),
         -c(trailing_dose,dose_times))


pk %>%
  filter(USUBJID %in% example_subj) %>%
  select(ONUM,NUM,USUBJID,ACTARM,DTTM,DAY,VISIT,PCTPT,NTSFD,TSFD,TSPD,DVID,DV,DOSE) %>%
  split(~USUBJID)


# combine -----------------------------------------------------------------

dmcognigen_pk <- pk %>%
  set_labels(., existing_labels) %>%
  set_labels(., new_variable_labels)

glimpse(dmcognigen_pk)


lapply(
  purrr::set_names(names(dmcognigen_pk)), 
  function(x) attr(dmcognigen_pk[[x]], "label")
)


# write -------------------------------------------------------------------

usethis::use_data(dmcognigen_pk, overwrite = TRUE)
