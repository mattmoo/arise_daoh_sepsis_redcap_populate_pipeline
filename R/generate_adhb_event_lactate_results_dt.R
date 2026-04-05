#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param adhb_event_dt
#' @param adhb_sepsis_lactate_results_dt
#' @param time_window
#' @return
#' @author Matthew Moore
#' @export
generate_adhb_event_lactate_results_dt <- function(adhb_event_dt,
                                                   adhb_sepsis_lactate_results_dt,
                                                   time_window = hours(6)) {

  
  adhb_event_match_dt = copy(adhb_event_dt)
  
  adhb_event_match_dt[, Admit.Date.Time2 := Admit.Date.Time + time_window]
  
  
  adhb_event_lactate_results_dt = adhb_sepsis_lactate_results_dt[adhb_event_match_dt, .(
    NHI,
    PMS_UNIQUE_IDENTIFIER = i.PMS_UNIQUE_IDENTIFIER,
    Result = x.Result,
    Sample_time = x.SPECIMENCOLLECTEDDATE
  ), # Non equi join:
  on = .(
    NHI,
    SPECIMENCOLLECTEDDATE >= Admit.Date.Time,
    SPECIMENCOLLECTEDDATE <= Admit.Date.Time2
  )][!is.na(Result)]
  
  # Samples from exactly the same time get the max result.
  adhb_event_lactate_results_dt = adhb_event_lactate_results_dt[, .SD[.N == 1 | Result == max(Result)], 
                                                                by = .(PMS_UNIQUE_IDENTIFIER, Sample_time)]
  
  return(adhb_event_lactate_results_dt)
}
