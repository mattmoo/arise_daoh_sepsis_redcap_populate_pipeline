#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param adhb_event_dt
#' @param moh_cohort_dt
#' @param moh_nmds_event_dt
#' @param moh_nnpac_event_dt
#' @param moh_nmds_event_dt
#' @param adhb_diag_dt
#' @param first_lactate_dt
#' @param max_lactate_dt
#' @return
#' @author Matthew Moore
#' @export
generate_ed_event_dt <- function(adhb_event_dt,
                                 moh_cohort_dt,
                                 moh_nmds_event_dt,
                                 moh_nnpac_event_dt,
                                 adhb_infection_dt,
                                 first_lactate_dt,
                                 max_lactate_dt,
                                 recap_datetime_fmt = "%Y-%m-%d %H:%M") {
  
  ed_event_dt = merge(adhb_event_dt[, .(nhi = NHI, 
                                        AGENCY,
                                        PMS_UNIQUE_IDENTIFIER, 
                                        ed_presentation_datetime = Admit.Date.Time,
                                        ed_triage_datetime = Triage.Date.Time)],
                      moh_cohort_dt[, .(
                        nhi = supplied_nhi,
                        date_of_birth = bthdate,
                        date_of_death = dthdate,
                        gender = GEND,
                        ethnicity_priority = ethnicgp
                      )], all.x = TRUE)
  
  # TODO: Tidy this up to prioritise infection type.
  ed_event_dt = merge(ed_event_dt,
                      adhb_infection_dt,
                      by = 'PMS_UNIQUE_IDENTIFIER', all.x = TRUE)
  
  
  
  ed_event_dt = merge(
    ed_event_dt,
    moh_nmds_event_dt[, .(
      PMS_UNIQUE_IDENTIFIER,
      AGENCY,
      nmds_event_end_type = END_TYPE,
      ADM_SRC,
      nmds_facility = FACILITY,
      nmds_event_id = EVENT_ID
    )],
    by = c('PMS_UNIQUE_IDENTIFIER', 'AGENCY'),
    all.x = TRUE
  )
  
  ed_event_dt = merge(
    ed_event_dt,
    moh_nnpac_event_dt[, .(
      PMS_UNIQUE_IDENTIFIER,
      nnpac_event_end_type = END_TYPE,
      nnpac_event_id = EVENT_ID
    )],
    by = c('PMS_UNIQUE_IDENTIFIER'),
    all.x = TRUE
  )
  
  ed_event_dt = merge(
    ed_event_dt, 
    first_lactate_dt[, .(PMS_UNIQUE_IDENTIFIER, first_lactate = Result)], by = 'PMS_UNIQUE_IDENTIFIER', all.x = TRUE)
  
  ed_event_dt = merge(
    ed_event_dt, max_lactate_dt[, .(
      PMS_UNIQUE_IDENTIFIER,
      highest_lactate_6h_reading = Result,
      highest_lactate_6h_time = Sample_time
    )], by = 'PMS_UNIQUE_IDENTIFIER', all.x = TRUE)
  
  
  setnames(ed_event_dt,
           'PMS_UNIQUE_IDENTIFIER',
           'pms_unique_identifier')
  ed_event_dt[, AGENCY := NULL]
  
  assertthat::are_equal(ed_event_dt[, .N], adhb_event_dt[, .N])
  
  return(ed_event_dt)
  
}
