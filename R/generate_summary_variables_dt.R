#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param eligible_event_dt
#' @param priority_ethnicity_lookup_dt
#' @param audit_diags_lookup_dt
generate_summary_variables_dt <- function(eligible_event_dt,
                                          priority_ethnicity_lookup_dt,
                                          audit_diags_lookup_dt) {

  summary_variables_dt = merge(
    eligible_event_dt,
    priority_ethnicity_lookup_dt,
    by.x = 'ethnicity_priority',
    by.y = 'priority.ethnicity.code.L2',
    all.x = TRUE
  )
  
  summary_variables_dt = merge(
    summary_variables_dt,
    audit_diags_lookup_dt,
    by.x = 'icd_code',
    by.y = 'infection_code',
    all.x = TRUE
  )
  
  summary_variables_dt[, days_to_death := difftime(date_of_death, ed_presentation_datetime, unit = 'days')]
  summary_variables_dt[, mort30 := !is.na(days_to_death) & days_to_death <= 30]
  summary_variables_dt[, mort90 := !is.na(days_to_death) & days_to_death <= 90]
  
  
  summary_variables_dt[, age_years := as.numeric(difftime(ed_presentation_datetime, date_of_birth, unit = 'days'))/365.25]
  
  summary_variables_dt[, gender := factor(gender, levels = c('F', 'M', 'U'), labels = c('Female', 'Male', 'Unknown'))]
  
  return(summary_variables_dt)

}
