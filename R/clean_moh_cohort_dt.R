#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param moh_cohort_raw_dt
#' @return
#' @author Matthew Moore
#' @export
clean_moh_cohort_dt <- function(moh_cohort_raw_dt, priority_ethnicity_lookup_dt) {

  moh_cohort_dt = unique(moh_cohort_raw_dt)
  
  moh_cohort_dt[, bthdate := as.Date(bthdate, format = "%d/%m/%Y")]
  moh_cohort_dt[, dthdate := as.Date(dthdate, format = "%d/%m/%Y")]
  moh_cohort_dt[, DOM := factor(DOM)]
  moh_cohort_dt[, GEND_DESC := factor(
    GEND,
    levels = c("M", "F", "O", 'U'),
    labels = c("Male", "Female", "Other", "Unknown")
  )]
  
  moh_cohort_dt[, ethnicgp := factor(ethnicgp)]
  
  moh_cohort_dt = merge(
    moh_cohort_dt,
    priority_ethnicity_lookup_dt,
    by.x = "ethnicgp",
    by.y = "priority.ethnicity.code.L2",
    all.x = TRUE
  )
  
  setcolorder(moh_cohort_dt,
              c(
                'supplied_nhi',
                'PRIM_HCU',
                'bthdate',
                'dthdate'
              ))
  
  assertthat::assert_that(moh_cohort_dt[, .N, by = supplied_nhi][N>1, .N] == 0)
  
  return(moh_cohort_dt)

}
