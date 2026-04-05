#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param adhb_sepsis_cohort1_raw_dt
#' @param adhb_sepsis_cohort2_raw_dt
#' @return
#' @author Matthew Moore
#' @export
generate_adhb_sepsis_cohort_dt <- function(adhb_sepsis_cohort1_raw_dt,
                                           adhb_sepsis_cohort2_raw_dt) {

  adhb_sepsis_cohort_dt = merge(adhb_sepsis_cohort1_raw_dt[, -c("Admit.Date.Time")],
                                adhb_sepsis_cohort2_raw_dt,
                                all.x = TRUE)[NHI != '']
  
  diag_levels_dt = adhb_sepsis_cohort_dt[, .N, by = .(Diag.Code, Diag.Desc)][order(Diag.Code)]
  
  adhb_sepsis_cohort_dt[, Diag.Desc := factor(Diag.Desc, levels = diag_levels_dt$Diag.Desc)]
  adhb_sepsis_cohort_dt[, ICD.Version.No := as.numeric(ICD.Version.No)]
  adhb_sepsis_cohort_dt[, Diag.Sequence.No := as.numeric(Diag.Sequence.No)]
  
  adhb_sepsis_cohort_dt[, Admit.Date.Time := openxlsx::convertToDateTime(Admit.Date.Time)]
  adhb_sepsis_cohort_dt[, Triage.Date.Time := openxlsx::convertToDateTime(Triage.Date.Time)]
  adhb_sepsis_cohort_dt[, Discharge.Date.Time := openxlsx::convertToDateTime(Discharge.Date.Time)]
  
  setcolorder(
    adhb_sepsis_cohort_dt,
    c(
      "NHI",
      "Event.ID",
      "Admit.Date.Time",
      "Triage.Date.Time",
      "Discharge.Date.Time",
      "Diag.Code",
      "Diag.Desc",
      "ICD.Version.No",
      "Diag.Sequence.No"
    )
  )
  
  return(adhb_sepsis_cohort_dt)

}
