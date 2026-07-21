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
  
  "cohortflow",
  "healthcodingnz",
  "daohtools"
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
    c(0,90)
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
    c(
      list(
        mort30 = 'Mortality (30-day)',
        mort90 = 'Mortality (90-day)',
        age_years = 'Age (years)',
        gender = 'Gender',
        priority.ethnicity.desc.L1 = 'Ethnicity (L1, Priority)',
        priority.ethnicity.desc.L2 = 'Ethnicity (L2, Priority)'
      ),
      setNames(as.list(redcap_metadata_dt$field_name), redcap_metadata_dt$field_label)
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
      priority_ethnicity_lookup_dt
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
  tar_target(
    criteria_obj,
    cf_criteria() |>
      exclude(~ not_transfer == FALSE, label = "Transfer",  category = "Pre-screen") |>
      exclude(~ has_lactate == FALSE, label = "No lactate recorded",  category = "Pre-screen") |>
      exclude(~ high_lactate == FALSE, label = "All lactate within 6 hours <2mmol/L",  category = "Pre-screen") |>
      exclude(~ infection_code == FALSE, label = "Admission has no infection diagnostic code",  category = "Pre-screen") |>
      exclude(~ infection_code_first_two == FALSE, label = "Infection code is not primary or secondary",  category = "Pre-screen") |>
      
      exclude(~ infection == 'Transfer', label = "Hospital transfer", category = "Records screen") |>
      exclude(~ infection == 'Not ED Visit', label = "Not ED visit", category = "Records screen") |>
      exclude(~ is.na(infection) | infection == 'Not Available' | infection == 'Not recorded', label = "Suspected infection unknown", category = "Records screen") |>
      exclude(~ infection == 'Not Infection', label = "No suspected infection", category = "Records screen") |>
      exclude(~ bp90_fluid_6hr == 'No', label = "BP>=90mmHg after 1L fluid", category = "Records screen") |>
      exclude(~ first_lactate_result <= 2, label = "First Lactate <= 2mmol/L", category = "Records screen") |>
      
      exclude(~ is.na(arise_exclusion), label = "REDCap missing", category = "ARISE criteria") |>
      exclude(~ arise_exclusion == 'Hypotension not due to sepsis', label = "Hypotension not due to sepsis", category = "ARISE criteria") |>
      exclude(~ arise_exclusion == 'Severe CHF/ESRF/other comorbidity precluding fluids or vasopressor', label = "Comorbidity precluding fluids or vasopressor", category = "ARISE criteria") |>
      exclude(~ arise_exclusion == 'DKA/severe gastro needing high fluid volumes', label = "DKA/severe gastro needing high fluid volumes", category = "ARISE criteria") |>
      exclude(~ arise_exclusion == 'Ceiling of care not for ICU', label = "Ceiling of care not for ICU", category = "ARISE criteria") |>
      exclude(~ arise_exclusion == 'Death imminent/inevitable', label = "Death imminent/inevitable", category = "ARISE criteria") |> 
      exclude(~ arise_exclusion == 'Underlying disease with death likely in < 90 days', label = "Underlying disease with death likely in < 90 days", category = "ARISE criteria")
  ),
  
  tar_target(
    cohortflow_obj,
    apply_criteria(
      eligibility_dt, 
      criteria_obj)
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
      eligible_event_dt = as.data.table(cohort(cohortflow_obj))[,.(pms_unique_identifier)],
      index_event_dt, 
      ed_event_dt,
      moh_cohort_dt,
      mortality_dt,
      daoh_dt,
      priority_ethnicity_lookup_dt,
      audit_diags_lookup_dt
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
  
  
)