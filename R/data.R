#' Example PK Data Requirements
#'
#' @description
#' Data Requirements designed based on \code{pharmaversesdtm} example datasets,
#' which are updated SDTM datasets that use data from the CDISC pilot project.
#' 
#' This has the same structure and attributes as results from
#' \code{\link{read_requirements}} and \code{\link{as_requirements}}.
#' 
#' @examples
#' head(dmcognigen_pk_requirements)
#' 
#' as_decode_tbls(dmcognigen_pk_requirements)
#' 
"dmcognigen_pk_requirements"


#' Covariate Dataset
#'
#' A dataset generated from \code{pharmaversesdtm} example datasets, which are
#' updated SDTM datasets that use data from the CDISC pilot project.
#' 
#' @seealso 
#' \code{\link[pharmaversesdtm]{dm}}, 
#' \code{\link[pharmaversesdtm]{lb}}, 
#' \code{\link[pharmaversesdtm]{vs}}
"dmcognigen_cov"


#' PK Concentrations Dataset
#'
#' A dataset generated from \code{pharmaversesdtm} example datasets, which are
#' updated SDTM datasets that use data from the CDISC pilot project.
#' 
#' @seealso 
#' \code{\link[pharmaversesdtm]{pc}}
"dmcognigen_conc"


#' Dose Dataset
#'
#' A dataset generated from \code{pharmaversesdtm} example datasets, which are
#' updated SDTM datasets that use data from the CDISC pilot project.
#' 
#' @seealso 
#' \code{\link[pharmaversesdtm]{ex}}
"dmcognigen_dose"


#' PK Dataset
#'
#' A dataset generated from \code{pharmaversesdtm} example datasets, which are
#' updated SDTM datasets that use data from the CDISC pilot project.
#' 
#' @seealso 
#' \code{\link[pharmaversesdtm]{dm}}, 
#' \code{\link[pharmaversesdtm]{ex}},
#' \code{\link[pharmaversesdtm]{lb}}, 
#' \code{\link[pharmaversesdtm]{pc}},
#' \code{\link[pharmaversesdtm]{vs}}
"dmcognigen_pk"
