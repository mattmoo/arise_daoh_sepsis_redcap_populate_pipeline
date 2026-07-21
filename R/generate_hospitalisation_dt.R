##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param moh_event_dt
##' @param moh_patient_dt
generate_hospitalisation_dt <- function(moh_event_dt, 
                                        moh_patient_dt, 
                                        include_hospital_only = FALSE) {
  
  hospitalisation_dt <- rbindlist(list(# moh_nnpac_dt[,.(PRIM_HCU,
    #                 EVSTDATE = date_of_service,
    #                 EVENDATE = date_of_service)],
    
    moh_event_dt[!include_hospital_only | FACILITY_TYPE == 1
                 , .(PRIM_HCU,
                     EVENT_ID,
                     PMS_UNIQUE_IDENTIFIER,
                     EVSTDATE = as.IDate(EVENT_START_DATETIME),
                     EVENDATE= as.IDate(EVENT_END_DATETIME))])
  )
  
  # There is a peculiarity where some hospital admissions are after death
  # (particularly from NNPAC) eliminate these.
  hospitalisation_dt <- merge(x = hospitalisation_dt,
                              y = moh_patient_dt,
                              by = 'PRIM_HCU',
                              all.x = T)[
                                is.na(dthdate) | EVSTDATE <= dthdate][
                                  ,.(PRIM_HCU, DOD = dthdate, EVSTDATE, EVENDATE)
                                ]
  
  return(hospitalisation_dt)
  
}