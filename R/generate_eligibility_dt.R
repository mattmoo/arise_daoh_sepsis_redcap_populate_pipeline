#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param index_event_dt
#' @param redcap_data_dt
#' @return
#' @author Matthew Moore
#' @export
generate_eligibility_dt <- function(
  index_event_dt,
  arise_eligibility_dt,
  moh_cohort_dt,
  redcap_data_dt,
  first_lactate_dt,
  max_lactate_dt
) {
  eligibility_dt <- merge(
    arise_eligibility_dt[, -"eligible"],
    index_event_dt,
    all.x = TRUE
  )

  eligibility_dt <- merge(
    eligibility_dt,
    redcap_data_dt[, .(
      pms_unique_identifier,
      bp90_fluid_6hr,
      infection,
      arise_eligible,
      arise_exclusion
    )],
    by = 'pms_unique_identifier',
    all.x = TRUE
  )

  eligibility_dt <- merge(
    eligibility_dt,
    moh_cohort_dt[, .(PRIM_HCU, bthdate)],
    by = 'PRIM_HCU',
    all.x = TRUE
  )

  eligibility_dt <- merge(
    eligibility_dt,
    first_lactate_dt[, .(
      pms_unique_identifier = PMS_UNIQUE_IDENTIFIER,
      first_lactate_sample_time = Sample_time,
      first_lactate_result = Result
    )],
    by = 'pms_unique_identifier',
    all.x = TRUE
  )
  eligibility_dt <- merge(
    eligibility_dt,
    max_lactate_dt[, .(
      pms_unique_identifier = PMS_UNIQUE_IDENTIFIER,
      max6hr_lactate_sample_time = Sample_time,
      max6hr_lactate_result = Result
    )],
    by = 'pms_unique_identifier',
    all.x = TRUE
  )

  return(eligibility_dt)
}
