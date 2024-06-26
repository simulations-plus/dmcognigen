% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculations.R
\name{calculate_crcl}
\alias{calculate_crcl}
\title{Calculate Creatinine Clearance}
\usage{
calculate_crcl(age, scr, sexf, wtkg)
}
\arguments{
\item{age}{Age (y). Numeric vector.}

\item{scr}{Serum Creatinine (mg/dL). Numeric vector.}

\item{sexf}{Sex. Numeric vector including values of 0 and/or 1. 0=Male; 1=Female.}

\item{wtkg}{Weight (kg). Numeric vector.}
}
\value{
This function returns a numeric vector the same length as its inputs
}
\description{
Calculate Creatinine Clearance (CrCl), a measure of renal function based on age, weight, gender, and serum creatinine. This is the Cockcroft and Gault Formula.
}
\details{
\if{html}{\out{<div class="sourceCode">}}\preformatted{Formula to calculate CRCL: 
  Males: CRCL [mL/min] = ((140 - AGE [y]) × WTKG [kg]) ÷ (72 × SCR [mg/dL])
  Females: CRCL [mL/min] = (((140 - AGE [y]) × WTKG [kg]) ÷ (72 × SCR [mg/dL])) × 0.85
}\if{html}{\out{</div>}}
}
\examples{
library(dplyr)

dmcognigen_cov \%>\% 
  mutate(CRCL = calculate_crcl(
    age = AGE, 
    scr = SCR, 
    sexf = SEXF,
    wtkg = WTKG 
  ))

# Below will also work if the dataset contains expected variables
dmcognigen_cov \%>\% 
  mutate(CRCL = calculate_crcl())

# Set a cap at some value, like 160
dmcognigen_cov \%>\% 
  mutate(CRCL = pmin(calculate_crcl(), 160))
}
\references{
Cockcroft DW, Gault MH. Prediction of creatinine clearance from serum creatinine. Nephron. 1976;16:31-41.
}
\seealso{
\code{\link{calculate_crcl_peck}} for Peck formula
}
