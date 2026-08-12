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
                                 arise_eligible_event_dt,
                                 index_event_dt, 
                                 ed_event_dt,
                                 moh_cohort_dt,
                                 moh_nmds_event_dt,
                                 mortality_dt,
                                 daoh_dt,
                                 priority_ethnicity_lookup_dt,
                                 audit_diags_lookup_dt,
                                 redcap_data_dt,
                                 comorbidity_score_dt) {
  # 
  # eligible_event_dt = as.data.table(cohort(initial_cohortflow_obj))[,.(pms_unique_identifier)]
  # arise_eligible_event_dt = as.data.table(cohort(cohortflow_obj))[,.(pms_unique_identifier)]
  # 
  
  analysis_dt = merge(
    eligible_event_dt,
    index_event_dt,
    all.x = TRUE
  )
  
  analysis_dt[, arise_eligible := factor(fifelse(pms_unique_identifier %in% arise_eligible_event_dt$pms_unique_identifier, "Yes", "No"),
                                        levels = c("No", "Yes"))]
  
  analysis_dt = merge(
    analysis_dt,
    ed_event_dt[, .(
      pms_unique_identifier,
      # ed_presentation_datetime,
      ed_triage_datetime,
      # ed_discharge_datetime,
      ethnicity_priority,
      icd_code,
      # nmds_event_id,
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
      PRIM_HCU = supplied_nhi,
      date_of_birth = bthdate,
      # ethnicity_priority = ethnicgp,
      DOM,
      gender = GEND,
      nzdep2023 = Dom_average_NZDep2023
    )],
    by = 'PRIM_HCU',
    all.x = TRUE
  )
  
  analysis_dt = merge(
    analysis_dt,
    moh_nmds_event_dt[, .(
      pms_unique_identifier = PMS_UNIQUE_IDENTIFIER,
      admit_datetime = EVENT_START_DATETIME,
      discharge_datetime = EVENT_END_DATETIME
    )],
    all.x = TRUE,
    by = 'pms_unique_identifier'
  )
  
  analysis_dt = merge(
    analysis_dt,
    comorbidity_score_dt[, .(EVENT_ID, m3_score = score)],
    by.x = 'nmds_event_id',
    by.y = 'EVENT_ID',
    all.x = TRUE
  )
  
  analysis_dt = merge(
    analysis_dt,
    redcap_data_dt[, .(
      pms_unique_identifier,
      news,
      snomed_cpc,
      triage_category,
      first_sbp,
      first_dbp,
      lowest_sbp,
      first_spo2,
      first_spo2_on_oxygen,
      highest_ews,
      time_highest_ews,
      first_temperature,
      first_heart_rate,
      first_resp_rate,
      first_avpu,
      first_gcs,
      primary_infection_site,
      primary_growth_species,
      primary_growth_spec_other,
      iv_fluids_vol_before,
      iv_fluids_vol_ed,
      first_antibiotic_datetime,
      first_antibiotic,
      first_antibiotic_roa,
      first_antibiotics_appropriate,
      first_appropriate_abx_datetime,
      # time_to_first_abx,
      # time_to_first_approp_abx,
      primary_vasopressor_bolus,
      primary_vasopressor_infusion,
      primary_vasopressor_roa,
      ed_disposition
    )],
    all.x = TRUE,
    by = 'pms_unique_identifier'
  )
  
  analysis_dt[, time_to_first_abx := difftime(first_antibiotic_datetime, ed_presentation_datetime, units = 'mins')]
  analysis_dt[, time_to_first_approp_abx := difftime(first_appropriate_abx_datetime, ed_presentation_datetime, units = 'mins')]
  
  analysis_dt[, first_antibiotic_grp := forcats::fct_lump_min(
    first_antibiotic, min = 8, other_level = "Other Antibiotic - See Comments"
  )]
  
  analysis_dt[, mort_in_hospital := nmds_event_end_type == "DD"]
  
  analysis_dt[, age_years := round(as.numeric(difftime(ed_presentation_datetime, date_of_birth, unit = 'days'))/365.25)]
  analysis_dt[, date_of_birth := NULL]
  analysis_dt[, nzdep2023_int := as.integer(nzdep2023)]
  analysis_dt[, nzdep2023_quintile_int := as.integer(ceiling(nzdep2023/2))]
  analysis_dt[, nzdep2023 := factor(nzdep2023_int, levels = 1:10)]
  analysis_dt[, nzdep2023_quintile := factor(nzdep2023_quintile_int, levels = 1:5)]

  
  analysis_dt[, gender := factor(gender, levels = c('F', 'M', 'U'), labels = c('Female', 'Male', 'Unknown'))]
  
  col_groups <- list(
    ids = c("pms_unique_identifier", "PRIM_HCU", "index_event_id", "record_id",
            "nmds_event_id", "nnpac_event_id"),
    
    event = c('admit_datetime', 'discharge_datetime'),
    
    cohort = c("icd_code", "arise_eligible", "DOM"),
    
    # Table 1
    demographics = c("age_years", "gender",
                     "ethnicity_priority", "priority.ethnicity.code.L1",
                     "priority.ethnicity.desc.L1", "priority.ethnicity.desc.L2", 'nzdep2023',
                     'nzdep2023_quintile', 'nzdep2023_int',
                     'nzdep2023_quintile_int'),
    
    ed_timing = c(
      "ed_presentation_datetime",
      "ed_presentation_date",
      "ed_triage_datetime"
      # "ed_discharge_datetime"
    ), 
    
    severity = c("m3_score", "snomed_cpc", "triage_category", "news", "highest_ews",
                 "time_highest_ews", "first_sbp", "first_dbp", "lowest_sbp",
                 "first_heart_rate", "first_resp_rate", "first_temperature",
                 "first_spo2", "first_spo2_on_oxygen", "first_avpu", "first_gcs",
                 "first_lactate", "highest_lactate_6h_reading"),
    
    infection = c("infection_desc", "primary_infection_site",
                  "primary_growth_species", "primary_growth_spec_other"),
    
    # Table 2
    fluids = c("iv_fluids_vol_before", "iv_fluids_vol_ed"),
    
    antibiotics = c("first_antibiotic_datetime", "first_antibiotic_grp", "first_antibiotic",
                    "first_antibiotic_roa", "first_antibiotics_appropriate",
                    "first_appropriate_abx_datetime",
                    "time_to_first_abx", "time_to_first_approp_abx"),
    
    vasopressors = c("primary_vasopressor_bolus", "primary_vasopressor_infusion",
                     "primary_vasopressor_roa"),
    
    # Table 3
    outcomes = c("ed_disposition", "nmds_event_end_type", "nnpac_event_end_type", "DOD",
                 "mort_in_hospital",
                 "mort.30.day", "mort.90.day",
                 "daoh_period_start", "daoh_period_end",
                 "dih", "dd", "daoh", "daoh_jittered")
  )
  
  new_order <- unlist(col_groups, use.names = FALSE)
  
  print(setdiff(names(analysis_dt), new_order))
  print(setdiff(new_order, names(analysis_dt)))
  stopifnot(
    length(setdiff(names(analysis_dt), new_order)) == 0,
    length(setdiff(new_order, names(analysis_dt))) == 0
  )
  
  data.table::setcolorder(analysis_dt, new_order, skip_absent = TRUE)
  
  return(analysis_dt)
  

}
