## code to prepare `dmcognigen_conc` dataset goes here

library(dplyr)


new_variable_labels <- c(
  
  # derived from pc
  DV = "Xanomeline Concentration (ug/mL)",
  LOGDV = "Ln Transformed Xanomeline Conc (ug/mL)",
  BLQFN = "BLQ Flag",
  LLOQ = "Lower Limit of Quantitation"
)


# from pc dataset ---------------------------------------------------------

admiral.test::admiral_pc %>% 
  cnt(PCTESTCD, PCTEST, PCLLOQ, PCORRESU, PCSTRESU)

admiral.test::admiral_pc %>% 
  cnt(VISITNUM, VISIT, PCTPTNUM, PCTPT)

# non-numeric original results
admiral.test::admiral_pc %>% 
  filter(suppressWarnings(is.na(as.numeric(PCORRES)))) %>% 
  cnt(PCORRES)


dmcognigen_conc <- admiral.test::admiral_pc %>% 
  mutate(
    LLOQ = PCLLOQ,
    BLQFN = as.numeric(stringr::str_detect(PCORRES, "<")),
    DV = case_when(
      BLQFN == 1 ~ LLOQ / 2,
      TRUE ~ PCSTRESN
    ),
    LOGDV = log(DV)
  ) %>% 
  relocate(DV, LOGDV, BLQFN, LLOQ, .after = USUBJID)



# combine -----------------------------------------------------------------

dmcognigen_conc <- set_labels(dmcognigen_conc, new_variable_labels)

glimpse(dmcognigen_conc)

lapply(
  purrr::set_names(names(dmcognigen_conc)), 
  function(x) attr(dmcognigen_conc[[x]], "label")
)


# write -------------------------------------------------------------------

usethis::use_data(dmcognigen_conc, overwrite = TRUE)
