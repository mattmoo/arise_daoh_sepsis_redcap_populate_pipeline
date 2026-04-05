#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param adhb_diag_dt
#' @return
#' @author Matthew Moore
#' @export
generate_adhb_infection_dt <- function(adhb_diag_dt) {

  adhb_infection_dt = adhb_diag_dt[, .(infection_code = any(infection_code)), by = PMS_UNIQUE_IDENTIFIER]
  
  adhb_infection_dt[, infection_code_first_two := PMS_UNIQUE_IDENTIFIER %in% adhb_diag_dt[Diag.Sequence.No <= 2, .(inf = any(infection_code)), by = PMS_UNIQUE_IDENTIFIER][inf == TRUE, PMS_UNIQUE_IDENTIFIER]]
  
  adhb_infection_dt = merge(adhb_infection_dt,
                            adhb_diag_dt[infection_code == TRUE, .SD[Diag.Sequence.No == min(Diag.Sequence.No), .(icd_code = diag_cat)], by = PMS_UNIQUE_IDENTIFIER],
                            by = "PMS_UNIQUE_IDENTIFIER",
                            all.x = TRUE)
  
  return(adhb_infection_dt)

}
