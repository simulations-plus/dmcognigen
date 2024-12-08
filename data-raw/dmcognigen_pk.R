## code to prepare `dmcognigen_pk` dataset goes here

library(dplyr)
library(tidyr)


# requirements ------------------------------------------------------------

data("dmcognigen_pk_requirements")
requirements <- dmcognigen_pk_requirements

# for reference
requirements_decode_tbls <- as_decode_tbls(requirements)

requirements_decode_tbls


# data --------------------------------------------------------------------

data("dmcognigen_cov")
cov <- dmcognigen_cov
cov_labels <- purrr::map_chr(cov, attr, which = "label")

data("dmcognigen_conc")
conc <- dmcognigen_conc
conc_labels <- purrr::map_chr(conc, attr, which = "label")

data("dmcognigen_dose")
dose <- dmcognigen_dose
dose_labels <- purrr::map_chr(dose, attr, which = "label")


# doses -------------------------------------------------------------------

dose %>% 
  cnt(TRT, ROUTE, n_distinct_vars = USUBJID)

dose %>% 
  cnt(DVID, EVID, MDV, n_distinct_vars = USUBJID)

dose %>% 
  cnt(DOSE, n_distinct_vars = USUBJID)


# concentrations ----------------------------------------------------------

conc %>% 
  cnt(DVID, PCTESTCD, PCTEST, n_distinct_vars = USUBJID)


# combine pk concentrations and doses -------------------------------------

pk <- bind_rows(
  conc,
  dose
) %>% 
  # in the case of samples and doses at the same time, will sort sample first.
  arrange(USUBJID, DTTM, EVID) %>% 
  # fill dose variables
  group_by(USUBJID) %>% 
  fill(
    DOSE, TRT, ROUTE, FDDTTM, EDDTTM, 
    .direction = "downup"
  ) %>% 
  ungroup() %>% 
  mutate(
    # dose-normalized concentration
    DNCP = case_when(
      DOSE == 0 ~ NA_real_,
      TRUE ~ DV / DOSE
    )
  ) %>% 
  arrange(USUBJID, DTTM, EVID)

pk %>% 
  cnt(STUDYID, n_distinct_vars = c(USUBJID, ID))

pk %>% 
  cnt(DVID, DVIDC, EVID, MDV, n_distinct_vars = USUBJID)

pk %>% 
  cnt(VISITNUM, VISIT, EVID)

pk %>% 
  cnt(DOSE, EVID, n_distinct_vars = USUBJID)


# merge stationary covariates ---------------------------------------------

# one per subject
cov %>%
  cnt(n_distinct_vars = USUBJID)

intersect(names(cov), names(pk))

setdiff(unique(cov$USUBJID), unique(pk$USUBJID))
setdiff(y = unique(cov$USUBJID), unique(pk$USUBJID))

pk <- pk %>% 
  left_join(
    cov %>% 
      select(-c(DOMAIN)),
    by = c("STUDYID", "USUBJID")
  )

pk %>% 
  cnt(ACTARMCD, ACTARM, EVID, n_distinct_vars = USUBJID)

pk %>% 
  cnt(ACTARMCD, ACTARM, DOSE, n_distinct_vars = USUBJID)


# time variables ----------------------------------------------------------


## actual time ------------------------------------------------------------

pk %>% 
  filter(EVID == 0) %>% 
  cnt(VISITNUM, VISIT, n_distinct_vars = PCTPT)

pk %>% 
  filter(EVID == 0) %>% 
  cnt(PCTPTNUM, PCTPT, n_distinct_vars = VISIT)

pk <- pk %>% 
  arrange(USUBJID, DTTM, EVID) %>% 
  group_by(USUBJID) %>% 
  mutate(
    # previous dose
    PRVDDTTM = case_when(
      EVID == 1 ~ DTTM,
      TRUE ~ lubridate::NA_POSIXct_
    ),
    # next dose
    NXTDDTTM = case_when(
      EVID == 1 ~ DTTM,
      TRUE ~ lubridate::NA_POSIXct_
    )
  ) %>% 
  fill(PRVDDTTM, .direction = "downup") %>% 
  fill(NXTDDTTM, .direction = "updown") %>% 
  mutate(
    # time from first dose
    TSFD = as.numeric(difftime(DTTM, FDDTTM, units = "hours")),
    # time since previous dose
    TSPD = case_when(
      EVID == 1 ~ 0,
      TRUE ~ as.numeric(difftime(DTTM, PRVDDTTM, units = "hours"))
    )
  ) %>% 
  ungroup()

pk %>% 
  select(TSFD, TSPD) %>% 
  summary()


## nominal times ----------------------------------------------------------

pk %>% 
  cnt(EVID, PCTPTNUM, PCTPT, n_distinct_vars = VISIT)

pk %>% 
  cnt(EVID, VISITNUM, VISIT, n_distinct_vars = PCTPT)

pk %>% 
  select(PCDY, EXSTDY) %>% 
  summary()

pk <- pk %>% 
  mutate(
    # nominal time since previous dose in hours
    NTSPD = case_when(
      # PCTPTNUM is already nominal time
      EVID == 0 ~ PCTPTNUM,
      EVID == 1 ~ 0
    ),
    
    # nominal time since first dose in hours
    NTSFD = case_when(
      # PCTPTNUM is already nominal time (working with post-first-dose, only)
      EVID == 0 ~ PCTPTNUM,
      EVID == 1 & VISIT == "BASELINE" ~ 0,
      # these dose timepoints won't be included in the final dataset
      EVID == 1 ~ 24 * 7 * as.numeric(stringr::str_replace(VISIT, "WEEK (\\d+)", "\\1"))
    )
  )

pk %>% 
  filter(is.na(NTSPD)) %>% 
  cnt(PCTPTNUM, PCTPT)

pk %>% 
  filter(is.na(NTSFD)) %>% 
  cnt(VISITNUM, VISIT)

pk %>% 
  select(NTSFD, NTSPD) %>% 
  summary()

pk %>% 
  cnt(NTSFD, NTSPD, VISITNUM, VISIT, EVID, PCTPTNUM, PCTPT)


# deletions ---------------------------------------------------------------

# normally, deletions are performed in a specific order with careful review.

pk %>% 
  cnt(DVID, DVIDC, EVID, MDV, n_distinct_vars = USUBJID)

pk <- pk %>% 
  filter(
    # remove pre-first-dose PK concentrations
    TSFD >= 0
  )

pk %>% 
  cnt(DVID, DVIDC, EVID, MDV, n_distinct_vars = USUBJID)

# remove trailing doses
pk <- pk %>% 
  group_by(USUBJID) %>% 
  filter(
    EVID == 0 | TSFD <= max(TSFD[EVID == 0])
  ) %>% 
  ungroup()

pk %>% 
  cnt(DVID, DVIDC, EVID, MDV, n_distinct_vars = USUBJID)


# ONUM, NUM ---------------------------------------------------------------

# add NUM and ONUM 
pk <- pk %>%
  group_by(USUBJID) %>%
  mutate(
    NUM = 1:n()
  ) %>%
  ungroup() %>%
  mutate(
    ONUM = 1:n()
  )


# join decodes ------------------------------------------------------------

# not expecting many new variables because they were merged in interim data sets  

pk <- pk %>%
  join_decode_labels(
    decode_tbls = requirements,
    lvl_to_lbl = list(
      "{var}C",
      RACEN = "RACEC"
    )
  )


# labels and variable order -----------------------------------------------

dmcognigen_pk <- pk %>%
  relocate(
    any_of(requirements$variable_name),
    .before = 1
  ) %>% 
  set_labels(cov_labels) %>%
  set_labels(conc_labels) %>%
  set_labels(dose_labels) %>%
  set_labels(requirements)

# don't include variables without a label
missing_labels <- purrr::map(dmcognigen_pk, attr, which = "label") %>% 
  purrr::keep(is.null) %>% 
  names()

dmcognigen_pk <- dmcognigen_pk %>% 
  select(-any_of(missing_labels))

# dataset label
attr(dmcognigen_pk, "label") <- "CDISCPILOT01 PK"

glimpse(dmcognigen_pk)


lapply(
  purrr::set_names(names(dmcognigen_pk)), 
  function(x) attr(dmcognigen_pk[[x]], "label")
)


# write -------------------------------------------------------------------

usethis::use_data(dmcognigen_pk, overwrite = TRUE)
