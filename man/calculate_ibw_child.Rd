% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculations.R
\name{calculate_ibw_child}
\alias{calculate_ibw_child}
\title{Calculate IBW child (Ideal Body Weight for children)}
\usage{
calculate_ibw_child(htcm)
}
\arguments{
\item{htcm}{Height (cm). Numeric vector.}
}
\value{
This function returns a numeric vector the same length as its inputs
}
\description{
Calculate Ideal Body Weight (for children), a measure of potential body fat based on height
}
\details{
\if{html}{\out{<div class="sourceCode">}}\preformatted{Formula to calculate IBW_CHILD: 
  IBW = [(HTCM) ^ 2 × 1.65] ÷ 1000
}\if{html}{\out{</div>}}
}
\examples{
library(dplyr)

dmcognigen_cov \%>\% 
  mutate(IBWCHILD = calculate_ibw_child(HTCM))

# Below will also work if the dataset contains expected variables
dmcognigen_cov \%>\% 
  mutate(IBWCHILD = calculate_ibw_child())
}
\references{
Traub SL and Johnson CE. Am J Hosp Pharm. 1980;37:195-201
}
\seealso{
\code{\link{calculate_ibw}}
}