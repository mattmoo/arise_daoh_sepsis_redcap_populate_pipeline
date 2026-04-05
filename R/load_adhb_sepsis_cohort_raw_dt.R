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
load_adhb_sepsis_cohort_raw_dt <- function(adhb_sepsis_lactate_xlsx_file_path,
                                           xlsx_pwd,
                                           nhi_encryption_fn,
                                           ...) {
  # options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx16192m"))
  # gc()
  # print(1)
  # a = xlsx::read.xlsx2(
  #   file = adhb_sepsis_lactate_xlsx_file_path,
  #   sheetIndex = 1,
  #   password = xlsx_pwd,
  #   ...
  # )
  # print(head(a))
  adhb_sepsis_cohort_raw_dt = as.data.table(xlsx::read.xlsx2(
    file = adhb_sepsis_lactate_xlsx_file_path,
    sheetIndex = 1,
    password = xlsx_pwd,
    ...
  ))[NHI != '', NHI := nhi_encryption_fn(NHI)]
  # print(2)
  # adhb_sepsis_cohort_raw_dt[, NHI := nhi_encryption_fn(NHI)]
  # print(3)
  rJava::.jgc()
  
  return(adhb_sepsis_cohort_raw_dt)

}
