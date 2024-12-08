## code to prepare `dmcognigen_pk_requirements` dataset goes here

# in practice, this content is expected to be imported.

reqs <- tibble::tribble(
  ~variable_name, ~variable_label,                           ~pk_ard, ~pk_mif,
  "ONUM",         "Overall Sequence Number",                 "x",     "x",
  "NUM",          "Sequence Number",                         "x",     "x",
  "STUDYID",      "Study Identifier",                        "x",     "",
  "USUBJID",      "Unique Subject Identifier",               "x",     "",
  "ID",           "Subject ID",                              "x",     "x",
  
  "TSFD",         "Time Since First Dose (h)",               "x",     "x",
  "TSPD",         "Time Since Previous Dose (h)",            "x",     "x",
  "NTSFD",        "Nominal Time Since First Dose (h)",       "x",     "",
  "NTSPD",        "Nominal Time Since Previous Dose (h)",    "x",     "",
  
  "DVID",         "Observation Type",                        "x",     "x",
  "DVIDC",        "Observation Type",                        "x",     "", 
  "EVID",         "Event ID",                                "x",     "x",
  "MDV",          "Missing Dependent Variable",              "x",     "x",
  "DV",           "Xanomeline Concentration (ug/mL)",        "x",     "x",
  "LOGDV",        "Ln Transformed Xanomeline Conc (ug/mL)",  "x",     "x",
  "BLQFN",        "BLQ Flag",                                "x",     "", 
  "LLOQ",         "Lower Limit of Quantitation",             "x",     "", 
  
  "FED",          "Fed",                                     "",      "", 
  "DOSE",         "Dose (mg)",                               "x",     "x",
  "TRT",          "Treatment",                               "x",     "", 
  "ROUTE",        "Route of Administration",                 "x",     "", 
  "AMT",          "Amount (mg)",                             "x",     "x",
  "FDDTTM",       "First Dose Date/Time",                    "x",     "", 
  "EDDTTM",       "Last Dose Date/Time",                     "x",     "", 
  
  "DNCP",         "Dose-Norm Xanomeline Conc (ug/mL/mg)",    "x",     "x",
  
  "DTTM",         "Date/Time",                               "x",     "", 
  "DAY",          "Day",                                     "x",     "", 
  
  "RACEN",        "Race",                                    "x",     "x",
  "RACEC",        "Race",                                    "x",     "",
  "SEXF",         "Sex",                                     "x",     "x",
  "SEXFC",        "Sex",                                     "x",     "", 
  "HTCM",         "Height (cm)",                             "x",     "", 
  "WTKG",         "Weight (kg)",                             "x",     "x",
  "AST",          "Baseline AST (U/L)",                      "x",     "", 
  "ASTULN",       "AST Upper Limit of Normal (U/L)",         "x",     "", 
  "SCR",          "Baseline SCR (mg/dL)",                    "x",     "", 
  "SCRULN",       "SCR Upper Limit of Normal (mg/dL)",       "x",     "", 
  "TBIL",         "Baseline TBIL (mg/dL)",                   "x",     "", 
  "TBILULN",      "TBIL Upper Limit of Normal (mg/dL)",      "x",     "", 
  "ASTCAT",       "Baseline AST Category",                   "x",     "", 
  "BMI",          "Baseline BMI (kg/m^2)",                   "x",     "", 
  "BSA",          "Baseline BSA (m^2)",                      "x",     "", 
  "IBW",          "Baseline IBW (kg)",                       "x",     "", 
  "CRCL",         "Baseline CrCL (mL/min)",                  "x",     "", 
  "CRCLP",        "Baseline CrCL (mL/min)",                  "x",     "", 
  "EGFR",         "Baseline eGFR (mL/min/1.73m^2)",          "x",     "", 
  "EGFRSCHW",     "Baseline eGFR (mL/min/1.73m^2)",          "x",     "", 
  "IBWCHILD",     "Baseline IBW (kg)",                       "x",     "", 
  "LBM",          "Baseline LBM (kg)",                       "x",     "", 
  "TBILCAT",      "Baseline TBIL Category",                  "x",     "", 
  "RFCAT",        "Baseline Renal Fx Category",              "x",     "", 
  "RFCATC",       "Baseline Renal Fx Category",              "x",     "", 
  "NCILIV",       "Baseline NCI Liver Fx Group",             "x",     "",
  "NCILIVC",      "Baseline NCI Liver Fx Group",             "x",     ""
  
)

# some decodes
reqs$format_decode <- ""

reqs$format_decode[reqs$variable_name == "SEXF"] <- 
  "0=Male
1=Female"

reqs$format_decode[reqs$variable_name == "RFCAT"] <- 
  "1=Normal Function (>=90 mL/min)
2=Mild Impairment (60-89 mL/min)
3=Moderate Impairment (30-59 mL/min)
4=Severe Impairment (15-29 mL/min)
5=End Stage Disease (<15 mL/min or Dialysis)"

reqs$format_decode[reqs$variable_name == "NCILIV"] <- 
  "0=Normal Group A
1=Mild Group B1
2=Mild Group B2
3=Moderate Group C
4=Severe Group D"

reqs$format_decode[reqs$variable_name == "DVID"] <-
  "0=Dose
1=Xanomeline Concentration (ug/mL)"

reqs$format_decode[reqs$variable_name == "EVID"] <- 
  "0=PK or PD measure
1=Dose
2=Other"

reqs$format_decode[reqs$variable_name == "MDV"] <- 
  "0=PK or PD measure
1=Dose or Other"

reqs$format_decode[reqs$variable_name == "FED"] <- 
  "0=Fasted
1=Fed"

reqs$format_decode[reqs$variable_name == "RACEN"] <-
  "1=White/Caucasian
2=Black/African American
3=Asian
4=American Indian or Alaska Native"

reqs$format_decode[reqs$variable_name == "BLQFN"] <-
  "0=No
1=Yes"


# apply requirements attributes -------------------------------------------

dmcognigen_pk_requirements <- as_requirements(reqs)


# write -------------------------------------------------------------------

usethis::use_data(dmcognigen_pk_requirements, overwrite = TRUE)
