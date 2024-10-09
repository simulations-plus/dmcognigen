mk_dummy_reqs_table <- function() {
  
  structure(list(path = c("../requirements_test/100103-d1pk-req-2023-01-03.xlsm", 
                          "../requirements_test/100103-d1pk-req-2023-01-18.xlsm", "../requirements_test/205024-lx9211-d1pk-req-2022-10-26.xlsm"
  ), path_date = structure(c(19360, 19375, 19291), class = "Date"), 
  created = structure(c(1674771963.33623, 1674771968.58438, 
                        1669744343.90151), tzone = "", class = c("POSIXct", "POSIXt"
                        )), modified = structure(c(1672848697.10329, 1674680451.13788, 
                                                   1666811328.72608), tzone = "", class = c("POSIXct", "POSIXt"
                                                   )), date = structure(c(19360, 19375, 19291), class = "Date"), 
  sheets = list(c("GENERAL", "INITIAL PK MODEL", "DOSING", 
                  "PK CONC", "PD", "specs", "Study Protocols", "ADDITIONAL DOCUMENTATION", 
                  "REFERENCE", "Defined Named", "Template Considerations"), 
                c("GENERAL", "INITIAL PK MODEL", "DOSING", "PK CONC", 
                  "PD", "specs", "Study Protocols", "ADDITIONAL DOCUMENTATION", 
                  "REFERENCE", "Defined Named", "Template Considerations"
                ), c("GENERAL", "INITIAL PK MODEL", "DOSING", "PK CONC", 
                     "PD", "specs", "ADDITIONAL DOCUMENTATION", "REFERENCE", 
                     "Defined Named", "Template Considerations")), is_qc = c(FALSE, 
                                                                             FALSE, FALSE)), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, 
                                                                                                                                                     -3L))
  
}


dummy_reqs_table <- mk_dummy_reqs_table()


make_temp_dir <- function() {
 
  dummy_dir <- tempdir()
  
  if(dir.exists(dummy_dir)) {
    unlink(dummy_dir,recursive = TRUE)
  }
  
  dir.create(dummy_dir)
  dummy_dir
}

dummy_dir <- make_temp_dir()

dummy_names <- basename(dummy_reqs_table$path)

dummy_times <- dummy_reqs_table$modified

dummy_paths <- file.path(dummy_dir, dummy_names)

purrr::walk2(dummy_names, dummy_times, ~ {
  
  openxlsx::write.xlsx(
  list(specs = example_requirements, unit_conversions = data.frame(), discussions = data.frame()),
  file = file.path(dummy_dir, .x)
  )
  
  Sys.setFileTime(file.path(dummy_dir, .x), .y)
  
  }

)
