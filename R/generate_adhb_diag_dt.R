#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param adhb_sepsis_cohort_dt
#' @param audit_diags_lookup_dt
#' @return
#' @author Matthew Moore
#' @export
generate_adhb_diag_dt <- function(adhb_sepsis_cohort_dt, audit_diags_lookup_dt) {

  
  adhb_diag_dt = unique(adhb_sepsis_cohort_dt[, .(PMS_UNIQUE_IDENTIFIER = Event.ID, NHI, Diag.Sequence.No, Diag.Code, Diag.Desc, ICD.Version.No)])

  adhb_diag_dt[, diag_cat := stringr::str_extract( Diag.Code, "^[A-Z0-9]{1,3}")]
  # adhb_diag_dt[, diag_cat := stringr::str_extract(diagnosis, "^[A-Z0-9]{1,3}")]
  
  # adhb_diag_dt = merge(adhb_diag_dt,
  #                      audit_diags_lookup_dt[, .(diag_cat = infection_code)],
  #                      all.x = TRUE)
  
  adhb_diag_dt[, infection_code := diag_cat %in% audit_diags_lookup_dt$infection_code]
  
  return(adhb_diag_dt)
}
