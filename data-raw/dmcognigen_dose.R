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
  TRT01A="Treatment"
  )


dose <- admiral.test::admiral_ex %>%
  transmute(STUDYID,
            USUBJID,
            DTTM=EXSTDTC,
            ENDTTM=EXENDTC,
            TRT01A = EXTRT,
            DOSE = EXDOSE,
            ROUTE = EXROUTE,
            DREG=1,
            DREGC=EXDOSFRQ,
            VISIT
            ) %>%
  group_by(USUBJID) %>%
  mutate(DOSENUM=1:n(),
         FDDTTM=min(DTTM),
         EDDTTM=max(DTTM)
         )%>% 
  arrange(STUDYID,USUBJID,DTTM)



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
  filter(FDDTTM==DTTM) %>%
  cnt(VISIT,DOSENUM)

# One dose
# EDDTTM sometimes BASELINE
dose %>%
  filter(EDDTTM==DTTM) %>%
  cnt(VISIT,DOSENUM)
   

dose %>%
  cnt(EDDTTM==FDDTTM,VISIT,DOSENUM,
      DOSE,n_distinct_vars = USUBJID)


dmcognigen_dose <- dmcognigen::set_labels(dose, new_variable_labels)

glimpse(dmcognigen_dose)

lapply(
  purrr::set_names(names(dmcognigen_dose)), 
  function(x) attr(dmcognigen_dose[[x]], "label")
)


dmcognigen_dose %>%
  purrr::map(~Hmisc::label(.x))

# write -------------------------------------------------------------------

usethis::use_data(dmcognigen_dose, overwrite = TRUE)
