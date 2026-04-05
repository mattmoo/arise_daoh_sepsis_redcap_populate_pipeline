#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param adhb_sepsis_lactate_xlsx_file_path
#' @param xlsx_pwd
#' @param nhi_encryption_fn
#' @return
#' @author Matthew Moore
#' @export
load_adhb_sepsis_lactate_results_raw_dt <- function(adhb_sepsis_lactate_xlsx_file_path,
                                                    xlsx_pwd,
                                                    nhi_encryption_fn,
                                                    ...) {
  

  adhb_sepsis_cohort_raw_dt = as.data.table(xlsx::read.xlsx2(
    file = adhb_sepsis_lactate_xlsx_file_path,
    sheetIndex = 4,
    password = xlsx_pwd,
    ...
  ))[PAT_ALIAS_ID != '', PAT_ALIAS_ID := nhi_encryption_fn(PAT_ALIAS_ID)]
  adhb_sepsis_cohort_raw_dt[, PAT_ID := NULL]
  rJava::.jgc()
  
  return(adhb_sepsis_cohort_raw_dt)

}
