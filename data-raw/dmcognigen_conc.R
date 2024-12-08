## code to prepare `dmcognigen_conc` dataset goes here

library(dplyr)


# requirements ------------------------------------------------------------

data("dmcognigen_pk_requirements")
requirements <- dmcognigen_pk_requirements

# for reference
requirements_decode_tbls <- as_decode_tbls(requirements)

requirements_decode_tbls[c("EVID", "DVID", "MDV", "BLQFN")]


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
    MDV = 0,
    DAY = PCDY
  ) %>% 
  relocate(DV, LOGDV, BLQFN, LLOQ, .after = USUBJID)

dmcognigen_conc %>%
  cnt(is.na(DTTM))


# join decodes ------------------------------------------------------------

dmcognigen_conc <- dmcognigen_conc %>%
  join_decode_labels(
    decode_tbls = requirements,
    lvl_to_lbl = "{var}C"
  )


# labels and variable order -----------------------------------------------

dmcognigen_conc <- dmcognigen_conc %>% 
  relocate(any_of(requirements$variable_name), .after = USUBJID) %>% 
  set_labels(requirements)

# don't include variables without a label
missing_labels <- purrr::map(dmcognigen_conc, attr, which = "label") %>% 
  purrr::keep(is.null) %>% 
  names()

dmcognigen_conc <- dmcognigen_conc %>% 
  select(-any_of(missing_labels))


# dataset label
attr(dmcognigen_conc, "label") <- "CDISCPILOT01 PK Concentrations"

glimpse(dmcognigen_conc)

lapply(
  purrr::set_names(names(dmcognigen_conc)), 
  function(x) attr(dmcognigen_conc[[x]], "label")
)


# write -------------------------------------------------------------------

usethis::use_data(dmcognigen_conc, overwrite = TRUE)
