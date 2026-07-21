##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param index_event_dt
##' @param moh_patient_dt
##' @param hospitalisation_dt
##'@param daoh_limits
generate_daoh_dt <- function(index_event_dt,
                             moh_patient_dt,
                             hospitalisation_dt,
                             daoh_limits) {
  
  index_event_dt = as.data.table(index_event_dt)
  
  consolidated_hospitalisation_dt = daohtools::consolidate.events(
    index.op.dt = index_event_dt,
    event.dt = hospitalisation_dt,
    patient.dt = moh_patient_dt,
    daoh.limits = daoh_limits,
    index.event.id.col.name = 'index_event_id',
    index.event.date.col.name = "ed_presentation_date",
    patient.id.col.name = 'PRIM_HCU',
    dod.col.name = 'dthdate'
  )
  
  daoh_dt = daohtools::calculate.daoh(
    index.op.dt = index_event_dt,
    patient.dt = moh_patient_dt,
    daoh.event.dt = consolidated_hospitalisation_dt,
    index.event.id.col.name = 'index_event_id',
    patient.id.col.name = 'PRIM_HCU',
    dod.col.name = 'dthdate'
  )
  
  # print(daoh_dt)
  
  daoh_dt = daoh_dt[, .(index_event_id,
                        daoh_period_start = daoh.period.start,
                        daoh_period_end = daoh.period.end,
                        DOD = dthdate,
                        dih = as.numeric(dih),
                        dd = as.numeric(dd),
                        daoh = as.numeric(daoh),
                        daoh_jittered = as.numeric(daoh) + rnorm(.N, mean = 0, sd = 0.05)
  )]
  
  return(daoh_dt)
  
}