#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ed_event_dt
#' @return
#' @author Matthew Moore
#' @export
generate_arise_eligibility_dt <- function(ed_event_dt) {

  arise_eligibility_dt = ed_event_dt[, .(pms_unique_identifier, not_transfer = !is.na(ADM_SRC) & ADM_SRC != 'T', 
                                         has_lactate = !is.na(first_lactate),
                                         high_lactate = !is.na(highest_lactate_6h_reading) & highest_lactate_6h_reading >= 2, 
                                         infection_code,
                                         infection_code_first_two)]
  
  arise_eligibility_dt[, eligible := not_transfer & has_lactate & infection_code_first_two & high_lactate]
  # arise_eligibility_dt[, eligible := not_transfer & infection_code]
  
  return(arise_eligibility_dt)

}
