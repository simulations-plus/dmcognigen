% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/requirements.R
\name{requirements}
\alias{requirements}
\alias{read_requirements}
\alias{as_requirements}
\alias{available_requirements_table}
\title{Get available and latest data requirements files}
\usage{
read_requirements(
  path = ".",
  pattern = "req",
  sheet = "specs",
  docx_header_pattern = NULL,
  date_format = c("ymd", "mdy", "dmy"),
  subset = NULL,
  variable_name_col = "variable_name",
  variable_label_col = "variable_label",
  decode_col = "format_decode",
  make_clean_names_fn = janitor::make_clean_names,
  ...
)

as_requirements(
  .data,
  variable_name_col = "variable_name",
  variable_label_col = "variable_label",
  decode_col = "format_decode"
)

available_requirements_table(
  path = ".",
  pattern = "req",
  sheet = "specs",
  date_format = c("ymd", "mdy", "dmy"),
  drop_qc = TRUE
)
}
\arguments{
\item{path}{a single directory path or the path to a data requirements file.
For \code{read_requirements}, providing a directory path will result in the
latest matching data requirements file being selected, while providing a
file path will result in that file being selected. Defaults to the working
directory.}

\item{pattern}{\code{character} string containing a regular expression. Only
file names which match the regular expression will be returned. Defaults to
\code{"req"}.}

\item{sheet}{either a \code{character} vector of required Excel sheet
name(s), the \code{numeric} index of the sheet position, or \code{NULL}
(for no required sheet names, and to include docx files). Only one sheet
name or index should be provided to \code{read_requirements}. Defaults to
\code{"specs"}, so `sheet = NULL` must be used to match the latest docx
file.}

\item{docx_header_pattern}{one or more patterns of required table header
names. Can be \code{character} or a list containing any combination of
\code{character}, \code{\link[stringr]{regex}}, and
\code{\link[stringr]{fixed}} patterns. For case-insensitive, use regex.}

\item{date_format}{\code{character} indicating the format of the date.
Defaults to the year-month-day format \code{"ymd"}.}

\item{subset}{an expression that returns a logical value and is defined in
the terms of the imported requirements table (like
\code{\link[dplyr]{filter}}). If the expression results in an error, that
error is reported as a warning and the subset is not applied. An example is
\code{subset = pk_ard == "x"}, which indicates to subset to variables
marked for inclusion in the PK Analysis Ready Dataset.}

\item{variable_name_col, variable_label_col, decode_col}{\code{character}
column names in the data requirements that describe the variable names,
their labels, and their decodes. These should match results after
transformations performed by \code{make_clean_names_fn}.}

\item{make_clean_names_fn}{a function/formula that cleans/transforms the
original variable names. Defaults to
\code{\link[janitor]{make_clean_names}}.}

\item{...}{optional arguments passed to either
\code{\link[openxlsx]{read.xlsx}} or \code{\link[docxtractr]{read_docx}}.}

\item{.data}{data frame to apply requirements attributes to.}

\item{drop_qc}{\code{logical} indicating whether to remove versions of the
data requirements that are used for QC. These are identified by patterns of
"qc" or "marked" in the filename.}
}
\description{
This family of functions allows the user to explore, import, and
  leverage the contents of data requirements files. Supported files types are
  Excel files (xlsx, xls, xlsm) and Word files (docx only). Note that \code{sheets
  = NULL} must be used to include docx files in searches.

\describe{
  \item{\code{read_requirements}}{Read the latest data requirements file.}
  \item{\code{as_requirements}}{Apply requirements attributes to a data frame.}
  \item{\code{available_requirements_table}}{Get available data requirements
  files. Returns a \code{tibble} including available data requirements paths
  and other information.}
}
}
\examples{
\dontrun{
# read the latest requirements file in the working directory, based on CPP defaults
reqs <- read_requirements()

# specify a particular file and sheet
reqs <- read_requirements(path = "requirements.xlsx", sheet = 1)

# read the latest docx requirements file
reqs <- read_requirements(
  pattern = "req.*docx",
  sheet = NULL, 
  docx_header_pattern = stringr::regex("variable", ignore_case = TRUE)
)

# apply attributes to an existing data frame
reqs <- as_requirements(reqs_df)

# get all available requirements files
available_requirements_table()

# include docx files in search
available_requirements_table(sheet = NULL)

# only include requirements with a specs sheet
available_requirements_table(sheet = "specs")

# include QC versions
available_requirements_table(drop_qc = FALSE)
}

}
