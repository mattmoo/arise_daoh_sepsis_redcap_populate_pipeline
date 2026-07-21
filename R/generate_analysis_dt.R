#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param index_event_dt
#' @param eligibility_dt
#' @param mortality_dt
#' @param daoh_dt
#' @return
#' @author Matthew Moore
#' @export
generate_analysis_dt <- function(eligible_event_dt,
                                 index_event_dt, 
                                 ed_event_dt,
                                 moh_cohort_dt,
                                 mortality_dt,
                                 daoh_dt,
                                 priority_ethnicity_lookup_dt,
                                 audit_diags_lookup_dt) {

  
  analysis_dt = merge(
    eligible_event_dt,
    index_event_dt,
    all.x = TRUE
  )
  
  analysis_dt = merge(
    analysis_dt,
    ed_event_dt[, .(
      pms_unique_identifier,
      ed_presentation_datetime,
      ed_triage_datetime,
      icd_code,
      nmds_event_id,
      nmds_event_end_type,
      nnpac_event_id,
      nnpac_event_end_type,
      first_lactate,
      highest_lactate_6h_reading
      )],
    all.x = TRUE
  )
  
  analysis_dt = merge(
    analysis_dt,
    daoh_dt,
    by = 'index_event_id',
    all.x = TRUE
  )
  
  analysis_dt = merge(
    analysis_dt,
    mortality_dt,
    by = 'index_event_id',
    all.x = TRUE
  )
  
  
  analysis_dt = merge(
    analysis_dt,
    priority_ethnicity_lookup_dt,
    by.x = 'ethnicity_priority',
    by.y = 'priority.ethnicity.code.L2',
    all.x = TRUE
  )
  
  analysis_dt = merge(
    analysis_dt,
    audit_diags_lookup_dt,
    by.x = 'icd_code',
    by.y = 'infection_code',
    all.x = TRUE
  )
  
  analysis_dt = merge(
    analysis_dt,
    moh_cohort_dt[, .(
      nhi = supplied_nhi,
      date_of_birth = bthdate,
      ethnicity_priority = ethnicgp,
      gender = GEND
      )],
    all.x = TRUE
  )
  
  analysis_dt[, age_years := as.numeric(difftime(ed_presentation_datetime, date_of_birth, unit = 'days'))/365.25]
  
  analysis_dt[, gender := factor(gender, levels = c('F', 'M', 'U'), labels = c('Female', 'Male', 'Unknown'))]
  
  
  return(analysis_dt)
  

}
