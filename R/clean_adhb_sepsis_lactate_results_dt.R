#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param adhb_sepsis_lactate_results_dt
#' @return
#' @author Matthew Moore
#' @export
clean_adhb_sepsis_lactate_results_dt <- function(adhb_sepsis_lactate_results_raw_dt) {

  adhb_sepsis_lactate_results_dt = copy(adhb_sepsis_lactate_results_raw_dt)
  
  adhb_sepsis_lactate_results_dt[, REQUESTTESTDESC := factor(REQUESTTESTDESC)]
  adhb_sepsis_lactate_results_dt[, SUBTESTDESC := factor(SUBTESTDESC)]
  adhb_sepsis_lactate_results_dt[, REQUESTTESTDESC := factor(REQUESTTESTDESC)]
  adhb_sepsis_lactate_results_dt[, OBSR_ABNORMAL := factor(OBSR_ABNORMAL, levels = c('N', 'L', 'H', 'HH'))]
  adhb_sepsis_lactate_results_dt[, OBSR_RES_TYPE := factor(OBSR_RES_TYPE)]
  adhb_sepsis_lactate_results_dt[, FACILITY := factor(FACILITY)]
  adhb_sepsis_lactate_results_dt[, OBSR_STATUS := factor(OBSR_STATUS)]
  
  adhb_sepsis_lactate_results_dt[, REQUESTEDDATE := openxlsx::convertToDateTime(REQUESTEDDATE)]
  adhb_sepsis_lactate_results_dt[, SPECIMENCOLLECTEDDATE := openxlsx::convertToDateTime(SPECIMENCOLLECTEDDATE)]
  adhb_sepsis_lactate_results_dt[, RECEIVEDDATE := openxlsx::convertToDateTime(RECEIVEDDATE)]
  adhb_sepsis_lactate_results_dt[, REPORTEDDATE := openxlsx::convertToDateTime(REPORTEDDATE)]
  
  adhb_sepsis_lactate_results_dt[, Result_raw := Result]
  adhb_sepsis_lactate_results_dt[, Result_numeric_string := stringr::str_remove(Result, '-( )*mmol/(l|L)?')]
  adhb_sepsis_lactate_results_dt[Result_numeric_string == '<1.9', Result_numeric := 1]
  adhb_sepsis_lactate_results_dt[Result_numeric_string == '<4.0', Result_numeric := 3]
  adhb_sepsis_lactate_results_dt[Result_numeric_string == '>4.0', Result_numeric := 5]
  adhb_sepsis_lactate_results_dt[Result_numeric_string == '>30.0', Result_numeric := 30]
  adhb_sepsis_lactate_results_dt[is.na(Result_numeric) & !(Result_raw == '' | Result_raw %ilike% 'comment|unavailable'), Result_numeric := as.numeric(Result_numeric_string)]
  
  adhb_sepsis_lactate_results_dt[Result_raw == '' | Result_raw %ilike% 'comment|unavailable', Result_numeric_string := NA_real_]
  
  adhb_sepsis_lactate_results_dt[, Result := Result_numeric]
  
  adhb_sepsis_lactate_results_dt[, Result_numeric := NULL]
  adhb_sepsis_lactate_results_dt[, Result_numeric_string := NULL]
  
  # Exclude some artefactual results.
  adhb_sepsis_lactate_results_dt = adhb_sepsis_lactate_results_dt[!is.na(Result)]
  adhb_sepsis_lactate_results_dt = adhb_sepsis_lactate_results_dt[, .SD[REPORTEDDATE == min(REPORTEDDATE)], by = .(OBRQ_FILLER_ORD)]
  
  setnames(adhb_sepsis_lactate_results_dt, 'EventID', 'PMS_UNIQUE_IDENTIFIER')
  setnames(adhb_sepsis_lactate_results_dt, 'PAT_ALIAS_ID', 'NHI')
  setcolorder(adhb_sepsis_lactate_results_dt, c('OBRQ_FILLER_ORD'))
  
  return(adhb_sepsis_lactate_results_dt)

}
