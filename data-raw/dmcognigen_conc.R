## code to prepare `dmcognigen_conc` dataset goes here

library(dplyr)


new_variable_labels <- c(
  # derived from pc
  DTTM = "Date/Time",
  DVID = "Observation Type",
  DVIDC = "Observation Type",
  EVID = "Event ID",
  MDV = "Missing Dependent Variable",
  DV = "Xanomeline Concentration (ug/mL)",
  LOGDV = "Ln Transformed Xanomeline Conc (ug/mL)",
  BLQFN = "BLQ Flag",
  LLOQ = "Lower Limit of Quantitation",
  DAY = "Day"
)


# from pc dataset ---------------------------------------------------------

pharmaversesdtm::pc %>% 
  cnt(PCTESTCD, PCTEST, PCLLOQ, PCORRESU, PCSTRESU)

pharmaversesdtm::pc %>% 
  cnt(VISITNUM, VISIT, PCTPTNUM, PCTPT)

# non-numeric original results
pharmaversesdtm::pc %>% 
  filter(suppressWarnings(is.na(as.numeric(PCORRES)))) %>% 
  cnt(PCORRES)

dmcognigen_conc <- pharmaversesdtm::pc %>% 
  mutate(
    LLOQ = PCLLOQ,
    DTTM = lubridate::ymd_hms(PCDTC),
    BLQFN = as.numeric(stringr::str_detect(PCORRES, "<")),
    DV = case_when(
      BLQFN == 1 ~ LLOQ / 2,
      TRUE ~ PCSTRESN
    ),
    LOGDV = log(DV),
    EVID = 0,
    DVID = 1,
    DVIDC = "Xanomeline Concentration (ug/mL)",
    MDV = 0,
    DAY = PCDY
  ) %>% 
  relocate(DV, LOGDV, BLQFN, LLOQ, .after = USUBJID)

dmcognigen_conc %>%
  cnt(is.na(DTTM))


# combine -----------------------------------------------------------------

dmcognigen_conc <- dmcognigen_conc %>% 
  relocate(any_of(names(new_variable_labels)), .after = USUBJID) %>% 
  set_labels(new_variable_labels)

# dataset label
attr(dmcognigen_conc, "label") <- "CDISCPILOT01 PK Concentrations"

glimpse(dmcognigen_conc)

lapply(
  purrr::set_names(names(dmcognigen_conc)), 
  function(x) attr(dmcognigen_conc[[x]], "label")
)


# write -------------------------------------------------------------------

usethis::use_data(dmcognigen_conc, overwrite = TRUE)
