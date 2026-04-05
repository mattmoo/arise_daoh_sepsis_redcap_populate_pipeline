#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ed_event_dt
#' @return
#' @author Matthew Moore
#' @export
generate_redcap_export_dt <- function(ed_event_dt, arise_eligibility_dt) {

  redcap_export_dt = ed_event_dt[pms_unique_identifier %in% arise_eligibility_dt[eligible == TRUE, pms_unique_identifier]]
  redcap_export_dt[, ADM_SRC := NULL]
  redcap_export_dt[, nmds_facility := NULL]
  
  redcap_export_dt[, infection_code := NULL]
  redcap_export_dt[, infection_code_first_two := NULL]
  
  redcap_export_dt[, record_id := .I]
  
  # redcap_export_dt[, nhi := 'ABC1234']
  
  return(redcap_export_dt)

}
