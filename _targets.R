# Needed for Java to read input XLSX
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx16192m"))

library(targets)
library(tarchetypes)
library(conflicted)
library(dotenv)
library(crew)
# R.utils::sourceDirectory('R')


lapply(
  FUN = source,
  X = list.files(
    path = 'R',
    pattern = '*\\.R$',
    full.names = TRUE,
    recursive = TRUE
  )
)
options(tidyverse.quiet = TRUE)

package_list = c(
  "data.table",
  # "icd10amachi",
  "REDCapR",
  "lubridate",
  "ggplot2",
  "gtsummary",
  "fst",
  # "readxl",
  "xlsx",
  "openxlsx",
  "assertthat",
  "stringr",
  "httr2",
  "keyring",
  "flextable",
  "writexl",
  "cohortflow",
  "healthcodingnz",
  "daohtools",
  "forcats"
  # "xlsx"
)
new.packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

tar_option_set(
  # use_crew = TRUE,
  # controller = crew_controller_local(workers = 6, seconds_idle = 30),
  # storage = "worker",
  # retrieval = "worker",
  # memory = "transient",
  # garbage_collection = TRUE,
  packages = package_list
)

data.table::setDTthreads(threads = 7)

conflicts_prefer(lubridate::year)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  
  tar_target(
    redcap_uri,
    'https://redcap.fmhs.auckland.ac.nz/api/'
  ),
  tar_target(
    daoh_limits,
    c(0,89)
  ),
  tar_target(
    global_initialisation_vector_raw,
    charToRaw('ngatirangiwewehi')
  ),
  
  tar_target(
    nhi_encryption_fn, 
    function(x, key = keyring::key_get("GLOBAL_NHI_ENCRYPT_KEY", keyring = 'arise'))
      aes_encrypt_vector(x, global_initialisation_vector_raw, key = key)), 
  
  tar_target(
    nhi_decryption_fn, 
    function(x, key = keyring::key_get("GLOBAL_NHI_ENCRYPT_KEY", keyring = 'arise'))
      aes_decrypt_vector(x, global_initialisation_vector_raw, key = key)), 
  
  tar_target(
    label_list,
    list(
      # identifiers
      pms_unique_identifier          = "PMS unique identifier",
      PRIM_HCU                       = "Encrypted NHI",
      index_event_id                 = "Index event ID",
      record_id                      = "Record ID",
      nmds_event_id                  = "NMDS event ID",
      nnpac_event_id                 = "NNPAC event ID",
      
      # cohort
      icd_code                       = "ICD code",
      arise_eligible                 = "ARISE eligible",
      
      # demographics
      age_years                      = "Age (years)",
      gender                         = "Gender",
      ethnicity_priority             = "Ethnicity (priority)",
      priority.ethnicity.code.L1     = "Ethnicity (L1, priority) code",
      priority.ethnicity.desc.L1     = "Ethnicity (L1, priority)",
      priority.ethnicity.desc.L2     = "Ethnicity (L2, priority)",
      DOM                            = "Domicile code",
      nzdep_decile                   = "NZDep2023 decile",
      nzdep2023_decile               = "NZDep2023 decile",
      nzdep2023                      = "NZDep2023 decile",
      nzdep2023_quintile             = "NZDep2023 quintile",
      
      # hospital event
      admit_datetime                 = "Hospital admission date/time",
      discharge_datetime             = "Hospital discharge date/time",
      ed_discharge_datetime          = "Hospital discharge date/time (ED extract)",
      
      # ED presentation
      ed_presentation_datetime       = "ED arrival date/time",
      ed_presentation_date           = "ED arrival date",
      ed_triage_datetime             = "ED triage date/time",
      snomed_cpc                     = "SNOMED chief presenting complaint",
      triage_category                = "Triage category",
      
      # severity
      m3_score = "M3",
      news                           = "National Early Warning Score (NEWS)",
      highest_ews                    = "Highest EWS in ED",
      time_highest_ews               = "Time of highest EWS",
      first_sbp                      = "First systolic blood pressure",
      first_dbp                      = "First diastolic blood pressure",
      lowest_sbp                     = "Lowest systolic blood pressure",
      first_heart_rate               = "First heart rate in ED",
      first_resp_rate                = "First respiratory rate in ED",
      first_temperature              = "First temperature in ED",
      first_spo2                     = "First SpO2 in ED",
      first_spo2_on_oxygen           = "First SpO2 recorded on oxygen",
      first_avpu                     = "First AVPU in ED",
      first_gcs                      = "First Glasgow Coma Scale in ED",
      first_lactate                  = "First lactate in ED",
      highest_lactate_6h_reading     = "Highest lactate within 6 hours",
      
      # infection
      infection_desc                 = "Infection description",
      primary_infection_site         = "Primary site of infection",
      primary_growth_species         = "Primary growth cultured",
      primary_growth_spec_other      = "Primary growth cultured (specify)",
      
      # treatments
      iv_fluids_vol_before           = "IV fluids before arrival (ml)",
      iv_fluids_vol_ed               = "IV fluids in ED (ml)",
      first_antibiotic_datetime      = "First antibiotic administration date/time",
      first_antibiotic               = "First antibiotic administered",
      first_antibiotic_grp           = "First antibiotic administered",
      first_antibiotic_roa           = "First antibiotic route of administration",
      first_antibiotics_appropriate  = "First antibiotics were appropriate",
      first_appropriate_abx_datetime = "First appropriate antibiotic date/time",
      time_to_first_abx              = "Minutes to first antibiotics",
      time_to_first_approp_abx       = "Minutes to first appropriate antibiotics",
      primary_vasopressor_bolus      = "Vasopressor IV bolus product",
      primary_vasopressor_infusion   = "Vasopressor IV infusion product",
      primary_vasopressor_roa        = "Vasopressor route of administration",
      
      # outcomes
      ed_disposition                 = "ED disposition",
      nmds_event_end_type            = "NMDS event end type",
      nnpac_event_end_type           = "NNPAC event end type",
      DOD                            = "Date of death",
      mort_in_hospital               = "Mortality (In-hospital)",
      mort.30.day                    = "Mortality (30-day)",
      mort.90.day                    = "Mortality (90-day)",
      daoh_period_start              = "DAOH period start",
      daoh_period_end                = "DAOH period end",
      dih                            = "Days in hospital",
      dd                             = "Days dead",
      daoh                           = "Days alive and out of hospital (90)",
      daoh_jittered                  = "DAOH90 (jittered)"
    )
  ),
  

  tar_target(
    input_data_directory_path,
    '//files.auckland.ac.nz/research/resmed202400055-daoh-sepsis-data/data/raw'
    # 'data/raw'
  ),
  tar_target(
    lookup_directory_path,
    '//files.auckland.ac.nz/research/resmed202400055-daoh-sepsis-data/data/lookup'
    # 'data/lookup'
  ),
  tar_target(
    output_directory_path,
    '//files.auckland.ac.nz/research/resmed202400055-daoh-sepsis-data/output'
    # 'data/lookup'
  ),
  tar_target(
    moh_input_data_directory_path,
    file.path(input_data_directory_path, 'moh')
  ),
  tar_target(
    moh_cohort_file_path,
    file.path(moh_input_data_directory_path, 'MOH-DataServices_mis5359.txt'),
    format = 'file'
  ),
  tar_target(
    moh_nmds_events_txt_file_path,
    file.path(moh_input_data_directory_path, 'MOH-DataServices_pus11613_events.txt'),
    format = 'file'
  ),
  tar_target(
    moh_nmds_diags_txt_file_path,
    file.path(moh_input_data_directory_path, 'MOH-DataServices_pus11613_diags.txt'),
    format = 'file'
  ),
  tar_target(
    moh_nnpac_events_txt_file_path,
    file.path(moh_input_data_directory_path, 'MOH-DataServices_prs0682_events.txt'),
    format = 'file'
  ),
  tar_target(
    moh_nnpac_diags_txt_file_path,
    file.path(moh_input_data_directory_path, 'MOH-DataServices_prs0682_diags.txt'),
    format = 'file'
  ),
  
  # There are two ADHB files to load, one of them has lactate results in a
  # separate worksheet, but the other has the sepsis events with triage and
  # discharge times.
  tar_target(
    adhb_input_data_directory_path,
    file.path(input_data_directory_path, 'adhb')
  ),
  tar_target(
    adhb_sepsis_event_xlsx_file_path,
    file.path(adhb_input_data_directory_path, 'Sepsis_Audit V2 extract TTT inpatient data warehouse 18 Jan 2023_datetime presentation sepsis code.xlsx'),
    format = 'file'
  ),
  tar_target(
    adhb_sepsis_lactate_xlsx_file_path,
    file.path(adhb_input_data_directory_path, 'Sepsis_Audit V2 extract TTT inpatient data warehouse 18 Jan 2023_NHIs with Lactate results 23 Jan.xlsx'),
    format = 'file'
  ),
  
  # Codes from the audit spreadsheet
  tar_target(
    audit_diags_lookup_path,
    file.path(lookup_directory_path, "audit_diags.csv")
  ),
  tar_target(
    audit_diags_lookup_dt,
    data.table::fread(audit_diags_lookup_path)[, infection_desc := factor(infection_desc)]
  ),
  
  # Ethnicity lookup tables
  tar_target(
    ethnicity_lookup_data_path,
    file.path(lookup_directory_path, "ethnicityMergeDT1-20190731.csv"),
    format = "file"
  ),
  # Lookup for facilities
  tar_target(
    facilities_lookup_data_path,
    file.path(lookup_directory_path, "facilities20180501.csv"),
    format = "file"
  ),
  tar_target(
    event_end_type_lookup_xls_path,
    file.path(lookup_directory_path, "copy_of_event-end-type_01.xls"),
  ),
  tar_target(
    admission_type_lookup_xls_path,
    file.path(lookup_directory_path, "admission-type.xls"),
  ),
  tar_target(
    admission_source_lookup_xls_path,
    file.path(lookup_directory_path, "admission-source.xls"),
  ),
  tar_target(
    hlthspec_lookup_xls_path,
    file.path(lookup_directory_path, "health_specialty_code_table_july_2020.xls"),
  ),
  tar_target(
    nzdep2023_dom_lookup_txt_path,
    file.path(lookup_directory_path, "NZDep2023_WgtAvDom-text.txt"),
  ),
  tar_target(
    priority_ethnicity_lookup_dt,
    generate_priority_ethnicity_lookup_dt(
      ethnicity_lookup_data_path
    )
  ),
  tar_target(
    event_end_type_lookup_dt, 
    as.data.table(readxl::read_xls(event_end_type_lookup_xls_path))[, .(
      END_TYPE = factor(`Event End Type code`),
      END_TYPE_DESC = factor(Description)
    )]
  ),
  tar_target(
    admission_type_lookup_dt,
    as.data.table(readxl::read_xls(admission_type_lookup_xls_path))[, .(
      ADM_TYPE = factor(`Admission Type code`),
      ADM_TYPE_DESC = factor(Description)
    )],
  ),
  tar_target(
    admission_source_lookup_dt,
    as.data.table(readxl::read_xls(admission_source_lookup_xls_path))[, .(
      ADM_SRC = factor(`Admission Source Code`),
      ADM_SRC_DESC = factor(Description)
    )],
  ),
  tar_target(
    hlthspec_lookup_dt,
    as.data.table(readxl::read_xls(hlthspec_lookup_xls_path))[, .(
      HLTHSPEC = factor(`Health Specialty Code`),
      HLTHSPEC_DESC = factor(Description)
    )],
  ),
  tar_target(
    nzdep2023_dom_lookup_dt,
    data.table::fread(nzdep2023_dom_lookup_txt_path)
  ),
  
  tar_target(
    adhb_sepsis_cohort1_raw_dt,
    load_adhb_sepsis_cohort_raw_dt(
      adhb_sepsis_lactate_xlsx_file_path,
      xlsx_pwd = keyring::key_get("ADHB_XLSX_LACTATE", keyring = 'arise'),
      nhi_encryption_fn,
      colIndex = 1:7
      # colClasses = c(
      #   Admit.Date.Time = 'POSIXct',
      #   Event.ID = 'character'
      # )
    ),
    format = 'fst_dt'
  ),
  tar_target(
    adhb_sepsis_cohort2_raw_dt,
    load_adhb_sepsis_cohort_raw_dt(
      adhb_sepsis_event_xlsx_file_path,
      xlsx_pwd = keyring::key_get("ADHB_XLSX_LACTATE", keyring = 'arise'),
      nhi_encryption_fn
      # colClasses = c(
      #   Admit.Date.Time = 'POSIXct',
      #   Triage.Date.Time = 'POSIXct',
      #   Discharge.Date.Time = 'POSIXct'
      # )
    ),
    format = 'fst_dt'
  ),
  tar_target(
    adhb_sepsis_lactate_results_raw_dt,
    load_adhb_sepsis_lactate_results_raw_dt(
      adhb_sepsis_lactate_xlsx_file_path,
      xlsx_pwd = keyring::key_get("ADHB_XLSX_LACTATE", keyring = 'arise'),
      nhi_encryption_fn
      # colClasses = c(
      #   REQUESTEDDATE = 'POSIXct',
      #   SPECIMENCOLLECTEDDATE = 'POSIXct',
      #   RECEIVEDDATE = 'POSIXct'
      # )
      # colClasses=NA
    ),
    format = 'fst_dt'
  ),
  
  tar_target(
    adhb_sepsis_cohort_dt,
    generate_adhb_sepsis_cohort_dt(
      adhb_sepsis_cohort1_raw_dt,
      adhb_sepsis_cohort2_raw_dt
    ),
    format = 'fst_dt'
  ),
  tar_target(
    adhb_sepsis_lactate_results_dt,
    clean_adhb_sepsis_lactate_results_dt(
      adhb_sepsis_lactate_results_raw_dt
    ),
    format = 'fst_dt'
  ),
  
  tar_target(
    adhb_patient_dt,
    unique(adhb_sepsis_cohort_dt[, .(NHI)])
  ),
  tar_target(
    adhb_event_dt,
    unique(adhb_sepsis_cohort_dt[, .(PMS_UNIQUE_IDENTIFIER = Event.ID, NHI, Admit.Date.Time, Triage.Date.Time, Discharge.Date.Time, FACILITY = 3260, AGENCY = 1022)])
  ),
  tar_target(
    adhb_diag_dt,
    generate_adhb_diag_dt(
      adhb_sepsis_cohort_dt,
      audit_diags_lookup_dt
    )
  ),
  
  # MOH data
  tar_target(
    moh_cohort_raw_dt,
    data.table::fread(
      input = moh_cohort_file_path
    )[, supplied_nhi := nhi_encryption_fn(supplied_nhi)][, PRIM_HCU := nhi_encryption_fn(PRIM_HCU)],
  ),
  tar_target(
    moh_nmds_diag_raw_dt,
    data.table::fread(
      input = moh_nmds_diags_txt_file_path
    ),
    format = 'fst_dt'
  ),
  tar_target(
    moh_nmds_event_raw_dt,
    data.table::fread(
      input = moh_nmds_events_txt_file_path
    )[, supplied_nhi := nhi_encryption_fn(supplied_nhi)][, PRIM_HCU := nhi_encryption_fn(PRIM_HCU)],
    format = 'fst_dt'
  ),
  tar_target(
    moh_nnpac_diag_raw_dt,
    data.table::fread(
      input = moh_nnpac_diags_txt_file_path
    ),
    format = 'fst_dt'
  ),
  tar_target(
    moh_nnpac_event_raw_dt,
    data.table::fread(
      input = moh_nnpac_events_txt_file_path
    )[, supplied_nhi := nhi_encryption_fn(supplied_nhi)][, PRIM_HCU := nhi_encryption_fn(PRIM_HCU)],
    format = 'fst_dt'
  ),
  
  tar_target(
    moh_cohort_dt,
    clean_moh_cohort_dt(
      moh_cohort_raw_dt,
      priority_ethnicity_lookup_dt,
      nzdep2023_dom_lookup_dt
    ),
    format = 'fst_dt'
  ),
  tar_target(
    moh_nnpac_event_dt,
    clean_moh_nnpac_event_dt(
      moh_nnpac_event_raw_dt,
      event_end_type_lookup_dt
    ),
    format = 'fst_dt'
  ),
  tar_target(
    moh_nmds_event_dt,
    clean_moh_nmds_event_dt(
      moh_nmds_event_raw_dt,
      event_end_type_lookup_dt,
      admission_type_lookup_dt,
      admission_source_lookup_dt,
      hlthspec_lookup_dt
    ), format = 'fst_dt'
  ),
  
  tar_target(
    moh_nmds_diag_dt,
    copy(moh_nmds_diag_raw_dt)
  ),
  
  # Get the lactate results within six hours of each admission per patient.
  tar_target(
    adhb_event_lactate_results_dt,
    generate_adhb_event_lactate_results_dt(
      adhb_event_dt,
      adhb_sepsis_lactate_results_dt,
      time_window = hours(6)
    )
  ),
  tar_target(
    first_lactate_dt,
    adhb_event_lactate_results_dt[, .SD[Sample_time == min(Sample_time)], by = PMS_UNIQUE_IDENTIFIER]
  ),
  tar_target(
    max_lactate_dt,
    adhb_event_lactate_results_dt[, .SD[Result == max(Result)][1], by = PMS_UNIQUE_IDENTIFIER]
  ),
  tar_target(
    adhb_infection_dt,
    generate_adhb_infection_dt(
      adhb_diag_dt
    )
  ),
  
  # Export table for REDCap
  tar_target(
    ed_event_dt,
    generate_ed_event_dt(
      adhb_event_dt,
      moh_cohort_dt,
      moh_nmds_event_dt,
      moh_nnpac_event_dt,
      adhb_infection_dt,
      first_lactate_dt,
      max_lactate_dt,
      recap_datetime_fmt = "%Y-%m-%d %H:%M"
    )
  ),
  
  tar_target(
    arise_eligibility_dt,
    generate_arise_eligibility_dt(
      ed_event_dt
    )
  ),
  
  tar_target(
    arise_eligibility_eulerr_fit,
    generate_arise_eligibility_dt(
      ed_event_dt
    )
  ),
  
  tar_target(
    redcap_export_dt,
    generate_redcap_export_dt(
      ed_event_dt,
      arise_eligibility_dt
    )
  ),
  
  tar_target(
    summary_variables_dt,
    generate_summary_variables_dt(
      eligible_event_dt = ed_event_dt[pms_unique_identifier %in% arise_eligibility_dt[eligible == TRUE, pms_unique_identifier]],
      priority_ethnicity_lookup_dt,
      audit_diags_lookup_dt
    )
  ),
  
  tar_target(
    comorbidity_score_dt,
    generate_comorbidity_score_dt(
      diag.dt = moh_nmds_diag_dt
    )
  ),

  # Don't uncomment this!
  # Was used to export to REDCap
  # tar_target(
  #   export_to_redcap_result_list,
  #   export_to_redcap(
  #     redcap_export_dt,
  #     nhi_decryption_fn
  #   )
  # )
  
  # Pull redcap metadata.
  tar_target(
    redcap_metadata_dt,
    pull_redcap_metadata(
      redcap_uri = redcap_uri,
      token = keyring::key_get("REDCAP_API", keyring = 'arise')
    )
  ),
  
  # Pull data and apply metadata.
  tar_target(
    redcap_data_dt,
    pull_redcap_data(
      redcap_uri = redcap_uri,
      token = keyring::key_get("REDCAP_API", keyring = 'arise'),
      redcap_metadata_dt
    )
  ),
  
  # Get index events for analysis
  tar_target(
    index_event_dt,
    redcap_export_dt[, .(
      index_event_id = .I,
      record_id,
      PRIM_HCU = nhi,
      nmds_event_id,
      pms_unique_identifier,
      ed_presentation_datetime,
      ed_presentation_date = as.IDate(ed_presentation_datetime)
    )]
  ),
  
  # Variables for assessing eligibility.
  tar_target(
    eligibility_dt,
    generate_eligibility_dt(
      arise_eligibility_dt,
      redcap_data_dt,
      first_lactate_dt
    )
  ),
  
  # Cohortflow
  # Criteria to be assessed for ARISE.
  tar_target(
    initial_criteria_obj,
    cf_criteria() |>
      exclude(~ not_transfer == FALSE, label = "Transfer",  category = "Pre-screen") |>
      exclude(~ has_lactate == FALSE, label = "No lactate recorded",  category = "Pre-screen") |>
      exclude(~ high_lactate == FALSE, label = "All lactate within 6 hours <2mmol/L",  category = "Pre-screen") |>
      exclude(~ infection_code == FALSE, label = "Admission has no infection diagnostic code",  category = "Pre-screen") |>
      exclude(~ infection_code_first_two == FALSE, label = "Infection code is not primary or secondary",  category = "Pre-screen") |>
      
      exclude(~ infection %ilike% 'Transfer', label = "Hospital transfer", category = "Records screen") |>
      exclude(~ infection == 'Not ED Visit', label = "Not ED visit", category = "Records screen") |>
      exclude(~ is.na(infection) | infection == 'Not Available' | infection == 'Not recorded', label = "Suspected infection unknown", category = "Records screen") |>
      exclude(~ infection == 'Not Infection', label = "No suspected infection", category = "Records screen") |>
      exclude(~ bp90_fluid_6hr == 'No', label = "BP>=90mmHg after 1L fluid", category = "Records screen") |>
      exclude(~ first_lactate_result <= 2, label = "First Lactate <= 2mmol/L", category = "Records screen")
  ),
  # Additional criteria for ARISE.
  tar_target(
    arise_criteria_obj,
    initial_criteria_obj |>
      
      # exclude(~ is.na(arise_exclusion), label = "REDCap exclusion missing", category = "ARISE criteria") |>
      exclude(~ arise_exclusion == 'Hypotension not due to sepsis', label = "Hypotension not due to sepsis", category = "ARISE criteria") |>
      exclude(~ arise_exclusion == 'Requirement for immediate surgery', label = "Requirement for immediate surgery", category = "ARISE criteria") |>
      exclude(~ arise_exclusion == 'Severe CHF/ESRF/other comorbidity precluding fluids or vasopressor', label = "Comorbidity precluding fluids or vasopressor", category = "ARISE criteria") |>
      exclude(~ arise_exclusion == 'DKA/severe gastro needing high fluid volumes', label = "DKA/severe gastro needing high fluid volumes", category = "ARISE criteria") |>
      exclude(~ arise_exclusion == 'Ceiling of care not for ICU', label = "Ceiling of care not for ICU", category = "ARISE criteria") |>
      exclude(~ arise_exclusion == 'Death imminent/inevitable', label = "Death imminent/inevitable", category = "ARISE criteria") |> 
      exclude(~ arise_exclusion == 'Underlying disease with death likely in < 90 days', label = "Underlying disease with death likely in < 90 days", category = "ARISE criteria")
  ),
  
  tar_target(
    initial_cohortflow_obj,
    apply_criteria(
      eligibility_dt, 
      initial_criteria_obj)
  ),
  
  tar_target(
    cohortflow_obj,
    apply_criteria(
      eligibility_dt, 
      arise_criteria_obj)
  ),
  
  tar_target(
    attr_tbl,
    as_attrition_tibble(cohortflow_obj)
  ),
  tar_target(
    attrition_ft,
    as_attrition_table(cohortflow_obj, backend = "flextable")
  ),
  
  # Calculate DAOH
  tar_target(
    hospitalisation_dt,
    generate_hospitalisation_dt(
      moh_nmds_event_dt,
      moh_cohort_dt)
  ),
  tar_target(
    daoh_dt,
    generate_daoh_dt(
      index_event_dt = index_event_dt,
      moh_patient_dt = moh_cohort_dt,
      hospitalisation_dt,
      daoh_limits
    )
  ), 
  # Calculate mortality.
  tar_target(
    mortality_dt,
    daoh_dt[,.(
      index_event_id,
      mort.30.day = !is.na(DOD) & (as.numeric(interval(daoh_period_start, DOD)/days(1)) <= 30),
      mort.90.day = !is.na(DOD) & (as.numeric(interval(daoh_period_start, DOD)/days(1)) <= 90)
    )]),
  
  tar_target(
    analysis_dt,
    generate_analysis_dt(
      eligible_event_dt = as.data.table(cohort(initial_cohortflow_obj))[,.(pms_unique_identifier)],
      arise_eligible_event_dt = as.data.table(cohort(cohortflow_obj))[,.(pms_unique_identifier)],
      index_event_dt, 
      ed_event_dt,
      moh_cohort_dt,
      moh_nmds_event_dt,
      mortality_dt,
      daoh_dt,
      priority_ethnicity_lookup_dt,
      audit_diags_lookup_dt,
      redcap_data_dt,
      comorbidity_score_dt[map == 'm3', .(EVENT_ID, m3_score = score)]
    )
  ),
  
  
  tar_target(
    overall_summary_gt,
    tbl_summary(
      data = summary_variables_dt,
      label = label_list,
      include = c(
        'age_years',
        'gender',
        'mort30',
        'mort90',
        'priority.ethnicity.desc.L1',
        'priority.ethnicity.desc.L2'
      )
    )
  ),  
  tar_target(
    ethnicity_summary_gt,
    tbl_summary(
      data = summary_variables_dt,
      by = 'priority.ethnicity.desc.L1',
      label = label_list,
      include = c(
        'age_years',
        'gender',
        'mort30',
        'mort90'
      )
    ) %>% add_overall()
  ),
  
  tar_target(
    table1_gt,
    
    analysis_dt[, .(
      age_years,
      gender,
      priority.ethnicity.desc.L1,
      nzdep_decile = as.numeric(as.character(nzdep2023)),
      m3_score,
      first_lactate,
      first_sbp,
      news,
      primary_infection_site,
      primary_growth_species,
      arise_eligible
    )] |> 
      tbl_summary(
        by = arise_eligible,
        label = label_list,
        type = list(nzdep_decile ~ "continuous"),
        statistic = list(
          all_continuous()  ~ "{mean} ({sd})",
          news      ~ "{median} [{p25}, {p75}]",
          nzdep_decile      ~ "{median} [{p25}, {p75}]",
          first_lactate     ~ "{median} [{p25}, {p75}]",
          m3_score     ~ "{median} [{p25}, {p75}]",
          all_categorical() ~ "{n} ({p}%)"
        ),
        digits = list(
          age_years     ~ 1,
          nzdep_decile  ~ 0,
          first_lactate ~ 1,
          first_sbp     ~ 0
        ),
        missing = "ifany",
        missing_text = "Missing"
      ) |>
      add_n() |>
      add_p(
        test = list(
          all_continuous()  ~ "wilcox.test",
          all_categorical() ~ "fisher.test"
        ),
        pvalue_fun = \(x) style_pvalue(x, digits = 3)
      ) |>
      add_overall(last = TRUE, col_label = "**Total**  \nN = {N}") |>
      modify_header(all_stat_cols(FALSE) ~ "**{level}**  \nN = {n}") |>
      modify_spanning_header(all_stat_cols(FALSE) ~ paste0("**", label_list$arise_eligible, "**")) |>
      bold_labels()
  ),
  
  tar_target(
    table1_ethnicity_gt,
    
    analysis_dt[, .(
      age_years,
      gender,
      nzdep_decile = as.numeric(as.character(nzdep2023)),
      first_lactate,
      first_sbp,
      m3_score,
      news,
      primary_infection_site,
      primary_growth_species,
      arise_eligible,
      priority.ethnicity.desc.L1
    )] |>
      tbl_summary(
        by = priority.ethnicity.desc.L1,
        label = label_list,
        type = list(nzdep_decile ~ "continuous"),
        statistic = list(
          all_continuous()  ~ "{mean} ({sd})",
          news      ~ "{median} [{p25}, {p75}]",
          nzdep_decile      ~ "{median} [{p25}, {p75}]",
          first_lactate     ~ "{median} [{p25}, {p75}]",
          m3_score     ~ "{median} [{p25}, {p75}]",
          all_categorical() ~ "{n} ({p}%)"
        ),
        digits = list(
          age_years     ~ 1,
          nzdep_decile  ~ 0,
          first_lactate ~ 1,
          first_sbp     ~ 0
        ),
        missing = "ifany",
        missing_text = "Missing"
      ) |>
      add_n() |>
      add_p(
        test = list(
          all_continuous()  ~ "kruskal.test",
          all_categorical() ~ "fisher.test"
        ),
        pvalue_fun = \(x) style_pvalue(x, digits = 3)
      ) |>
      add_overall(last = TRUE, col_label = "**Total**  \nN = {N}") |>
      modify_header(all_stat_cols(FALSE) ~ "**{level}**  \nN = {n}") |>
      modify_spanning_header(all_stat_cols(FALSE) ~ paste0("**", label_list$arise_eligible, "**")) |>
      bold_labels()
  ),
  
  
  # DAOH bootstrapping
  tar_target(boot_R,    10000L),
  tar_target(boot_conf, 0.95),
  tar_target(daoh_boot_strata_vec, c("arise_eligible", "priority.ethnicity.desc.L1")),
  
  tar_target(
    daoh_boot_dt,
    groupingsets(
      analysis_dt[!is.na(daoh)],
      j = boot_daoh(daoh, R = boot_R, conf = boot_conf),
      by = daoh_boot_strata_vec,
      sets = c(list(character(0)), as.list(daoh_boot_strata_vec))
    )
  ),
  
  # Draw nice tables of bootstrapped stats
  tar_target(daoh_boot_strata_all, daoh_boot_strata_vec),
  tar_target(
    daoh_boot_ft_list,
    list(
      daoh_boot_stratum = daoh_boot_strata_vec,
      daoh_boot_table = draw_daoh_boot_table(
        boot_dt     = daoh_boot_dt,
        stratum_var = daoh_boot_strata_vec,
        by_vars     = daoh_boot_strata_all,
        labels      = label_list
      )
    ),
    pattern = map(daoh_boot_strata_vec),
    iteration = "list"
  ),
  
  tar_target(
    table2_gt,
    analysis_dt[, c(  "time_to_first_abx",
                                "first_antibiotic_grp",
                                "first_antibiotics_appropriate",
                                "iv_fluids_vol_before",
                                "iv_fluids_vol_ed",
                                "primary_vasopressor_bolus",
                                "primary_vasopressor_infusion", 
                                "arise_eligible"), with = FALSE] |>
      tbl_summary(
        by = arise_eligible,
        label = label_list,
        statistic = list(
          all_continuous()  ~ "{mean} ({sd})",
          all_categorical() ~ "{n} ({p}%)"
        ),
        digits = all_continuous() ~ 0,
        missing = "ifany",
        missing_text = "Missing"
      ) |>
      add_n() |>
      add_p(
        test = list(
          all_continuous()  ~ "wilcox.test",
          all_categorical() ~ "fisher.test"
        ),
        test.args = first_antibiotic_grp ~ list(simulate.p.value = TRUE, B = 1e5),
        pvalue_fun = \(x) style_pvalue(x, digits = 3)
      ) |>
      add_overall(last = TRUE, col_label = "**Total**  \nN = {N}") |>
      modify_header(all_stat_cols(FALSE) ~ "**{level}**  \nN = {n}") |>
      modify_spanning_header(all_stat_cols(FALSE) ~ paste0("**", label_list$arise_eligible, "**")) |>
      bold_labels()
  ),
  
  tar_target(
    table3_gt,
    analysis_dt[, .(
      # nmds_event_end_type,
      ed_disposition,
      mort_in_hospital,
      mort.30.day,
      mort.90.day,
      daoh,
      arise_eligible
    )] |>
      tbl_summary(
        by = arise_eligible,
        label = c(label_list,
                  list(died_in_hospital = "Died in hospital (index event)")),
        statistic = list(
          all_continuous()  ~ "{median} [{p25}, {p75}]",
          all_categorical() ~ "{n} ({p}%)"
        ),
        digits = daoh ~ 1,
        missing = "ifany",
        missing_text = "Missing"
      ) |>
      add_n() |>
      add_p(
        test = list(
          all_continuous()  ~ "wilcox.test",
          all_categorical() ~ "fisher.test"
        ),
        pvalue_fun = \(x) style_pvalue(x, digits = 3)
      ) |>
      add_overall(last = TRUE, col_label = "**Total**  \nN = {N}") |>
      modify_header(all_stat_cols(FALSE) ~ "**{level}**  \nN = {n}") |>
      modify_spanning_header(all_stat_cols(FALSE) ~ paste0("**", label_list$arise_eligible, "**")) |>
      bold_labels()
  ),
  
  tar_target(
    table3_ethnicity_gt,
    analysis_dt[, .(
      mort_in_hospital,
      mort.30.day,
      mort.90.day,
      daoh,
      priority.ethnicity.desc.L1
    )] |>
      tbl_summary(
        by = priority.ethnicity.desc.L1,
        label = c(label_list,
                  list(mort_in_hospital = "Died in hospital (index event)")),
        statistic = list(
          all_continuous()  ~ "{median} [{p25}, {p75}]",
          all_categorical() ~ "{n} ({p}%)"
        ),
        digits = daoh ~ 1,
        missing = "ifany",
        missing_text = "Missing"
      ) |>
      add_n() |>
      add_p(
        test = list(
          all_continuous()  ~ "kruskal.test",
          all_categorical() ~ "fisher.test"
        ),
        test.args = all_categorical() ~ list(simulate.p.value = TRUE, B = 1e5),
        pvalue_fun = \(x) style_pvalue(x, digits = 3)
      ) |>
      add_overall(last = TRUE, col_label = "**Total**  \nN = {N}") |>
      modify_header(all_stat_cols(FALSE) ~ "**{level}**  \nN = {n}") |>
      modify_spanning_header(all_stat_cols(FALSE) ~
                               paste0("**", label_list$priority.ethnicity.desc.L1, "**")) |>
      bold_labels()
  ),
  
  tar_target(
    table3_arise_ethnicity_gt,
    analysis_dt[arise_eligible == 'Yes', .(
      mort_in_hospital,
      mort.30.day,
      mort.90.day,
      daoh,
      priority.ethnicity.desc.L1
    )] |>
      tbl_summary(
        by = priority.ethnicity.desc.L1,
        label = c(label_list,
                  list(mort_in_hospital = "Died in hospital (index event)")),
        statistic = list(
          all_continuous()  ~ "{median} [{p25}, {p75}]",
          all_categorical() ~ "{n} ({p}%)"
        ),
        digits = daoh ~ 1,
        missing = "ifany",
        missing_text = "Missing"
      ) |>
      add_n() |>
      add_p(
        test = list(
          all_continuous()  ~ "kruskal.test",
          all_categorical() ~ "fisher.test"
        ),
        test.args = all_categorical() ~ list(simulate.p.value = TRUE, B = 1e5),
        pvalue_fun = \(x) style_pvalue(x, digits = 3)
      ) |>
      add_overall(last = TRUE, col_label = "**Total**  \nN = {N}") |>
      modify_header(all_stat_cols(FALSE) ~ "**{level}**  \nN = {n}") |>
      modify_spanning_header(all_stat_cols(FALSE) ~
                               paste0("**", label_list$priority.ethnicity.desc.L1, "**")) |>
      bold_labels()
  ),
  
  # Output files
  tar_target(
    table_output_directory_path,
    file.path(output_directory_path, 'tables')
  ),
  
  tar_target(
    tbl1_table_docx_file,
    write_table(
      table    = table1_gt,
      filename = "table1_demographics.docx",
      path     = table_output_directory_path,
      caption  = "Table 1. Baseline demographics"
    ),
    format = "file"
  ),
  
  tar_target(
    tbl1_ethnicity_table_docx_file,
    write_table(
      table    = table1_ethnicity_gt,
      filename = "table1_demographics_by_ethnicity.docx",
      path     = table_output_directory_path,
      caption  = "Table 1. Baseline demographics by Ethnicity"
    ),
    format = "file"
  ),
  
  tar_target(
    tbl2_table_docx_file,
    write_table(
      table    = table2_gt,
      filename = "table2_treatments.docx",
      path     = table_output_directory_path,
      caption  = "Table 2. Treatments"
    ),
    format = "file"
  ),
  
  tar_target(
    tbl3_table_docx_file,
    write_table(
      table    = table3_gt,
      filename = "table3_outcomes.docx",
      path     = table_output_directory_path,
      caption  = "Table 3. Outcomes by ARISE eligibility"
    ),
    format = "file"
  ),
  
  tar_target(
    tbl3_ethnicity_table_docx_file,
    write_table(
      table    = table3_ethnicity_gt,
      filename = "table3_outcomes_ethnicity.docx",
      path     = table_output_directory_path,
      caption  = "Table 3b. Outcomes by prioritised ethnicity"
    ),
    format = "file"
  ),
  
  tar_target(
    tbl3_arise_ethnicity_table_docx_file,
    write_table(
      table    = table3_arise_ethnicity_gt,
      filename = "table3_outcomes_arise_ethnicity.docx",
      path     = table_output_directory_path,
      caption  = "Table 3b. Outcomes for ARISE eligible by prioritised ethnicity"
    ),
    format = "file"
  ),
  
  
  tar_target(
    daoh_boot_table_docx_file,
    write_table(
      table    = daoh_boot_ft_list$daoh_boot_table$flextable,
      filename = paste0("daoh_boot_", daoh_boot_ft_list$daoh_boot_stratum, ".docx"),
      path     = table_output_directory_path,
      caption  = paste0("Days alive and out of hospital to 90 days, by ",
                        tolower(daoh_boot_ft_list$daoh_boot_table$label))
    ),
    pattern = map(daoh_boot_ft_list),
    format = "file"
  ),
  
  tar_target(
    attrition_table_docx_file,
    write_table(
      table    = attrition_ft,
      filename = "eligibility_criteria.docx",
      path     = table_output_directory_path
    ),
    format = "file"
  ),
  
  tar_target(
    arise_sepsis_xlsx_file,
    {
      stamp <- format(Sys.time(), "%Y%m%d-%H%M")
      hash  <- substr(digest::digest(analysis_dt), 1, 8)
      path  <- file.path(output_directory_path,
                         sprintf("arise_sepsis_%s_%s.xlsx", stamp, hash))
      writexl::write_xlsx(analysis_dt, path = path)
      path
    }
  ),
  
  tar_target(
    arise_sepsis_eligibility_xlsx_file,
    {
      stamp <- format(Sys.time(), "%Y%m%d-%H%M")
      hash  <- substr(digest::digest(analysis_dt), 1, 8)
      path  <- file.path(output_directory_path,
                         sprintf("arise_sepsis_eligibility_%s_%s.xlsx", stamp, hash))
      writexl::write_xlsx(eligibility_dt, path = path)
      path
    }
  )
  
  
)