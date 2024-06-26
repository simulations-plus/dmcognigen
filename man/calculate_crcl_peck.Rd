% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculations.R
\name{calculate_crcl_peck}
\alias{calculate_crcl_peck}
\title{Calculate Creatinine Clearance using the Peck Formula}
\usage{
calculate_crcl_peck(age, crcl, ibw, scr, sexf, wtkg)
}
\arguments{
\item{age}{Age (y). Numeric vector.}

\item{crcl}{Creatinine Clearance (mL/min)}

\item{ibw}{Ideal Body Weight (kg). Numeric vector.}

\item{scr}{Serum Creatinine (mg/dL). Numeric vector.}

\item{sexf}{Sex. Numeric vector including values of 0 and/or 1. 0=Male; 1=Female.}

\item{wtkg}{Weight (kg). Numeric vector.}
}
\value{
This function returns a numeric vector the same length as its inputs
}
\description{
Calculate Creatinine Clearance (CrCl) using the Peck Formula, a measure of renal function based on age, ideal weight, gender, and serum creatinine
}
\details{
\if{html}{\out{<div class="sourceCode">}}\preformatted{Formula to calculate CRCL_PECK: 
  If WTKG [kg] < 120\% of IBW [kg], CRCL [mL/min] = CRCL [mL/min] (Cockroft & Gault)
  If WTKG [kg] >= 120\% of IBW [kg]:
    Males: CRCL [mL/min] = ((140 - AGE [y]) × IBW [kg]) ÷ (72 × SCR [mg/dL])
    Females: CRCL [mL/min] = (((140 - AGE [y]) × IBW [kg]) ÷ (72 × SCR [mg/dL])) × 0.85
}\if{html}{\out{</div>}}
}
\examples{
library(dplyr)

dmcognigen_cov \%>\% 
  mutate(CRCLP = calculate_crcl_peck(
    age = AGE, 
    crcl = CRCL, 
    ibw = IBW, 
    scr = SCR, 
    sexf = SEXF,
    wtkg = WTKG
  ))

# Below will also work if the dataset contains expected variables
dmcognigen_cov \%>\% 
  mutate(CRCLP = calculate_crcl_peck())
}
\references{
Peck CC, Conner DP, Murphy MG. Simple Techniques for Individualizing Drug Therapy. Vancouver, WA. Applied Therapeutics, Inc. 1989.
}
\seealso{
\code{\link{calculate_crcl}} for Cockcroft and Gault formula
}
