#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param diag.dt
generate_comorbidity_score_dt <- function(diag.dt) {

  
  comorbidity_obj = comorbidity::comorbidity(x = diag.dt,
                                             id = 'EVENT_ID',
                                             code = 'CLIN_CD',
                                             map = 'm3_icd10_am',
                                             assign0 = F)
  
  score_dt = data.table(
    EVENT_ID = comorbidity_obj$EVENT_ID,
    map = 'm3_icd10_am',
    weights = 'm3',
    assign0 = F,
    score = comorbidity::score(comorbidity_obj, weights = 'm3', assign0 = F)
  )
  
  return(score_dt)

}
