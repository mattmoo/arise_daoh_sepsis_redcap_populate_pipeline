#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param moh_nmds_event_raw_dt
#' @param event_end_type_lookup_dt
#' @return
#' @author Matthew Moore
#' @export
clean_moh_nmds_event_dt <- function(moh_nmds_event_raw_dt,
                                    event_end_type_lookup_dt,
                                    admission_type_lookup_dt,
                                    admission_source_lookup_dt,
                                    hlthspec_lookup_dt) {

  name_vector = names(moh_nmds_event_raw_dt)
  
  moh_nmds_event_dt = merge(moh_nmds_event_raw_dt,
                            event_end_type_lookup_dt,
                            by = 'END_TYPE',
                            all.x = TRUE)
  moh_nmds_event_dt = merge(moh_nmds_event_dt,
                            admission_type_lookup_dt,
                            by = 'ADM_TYPE',
                            all.x = TRUE)
  moh_nmds_event_dt = merge(moh_nmds_event_dt,
                            admission_source_lookup_dt,
                            by = 'ADM_SRC',
                            all.x = TRUE)
  moh_nmds_event_dt = merge(moh_nmds_event_dt,
                            hlthspec_lookup_dt,
                            by = 'HLTHSPEC',
                            all.x = TRUE)
  
  moh_nmds_event_dt[, `:=`(
    # event_date = as.Date(event_date, format = "%Y-%m-%d"),
    # event_time = as.POSIXct(event_time, format = "%H:%M:%S"),
    EVENT_START_DATETIME  = as.POSIXct(EVENT_START_DATETIME, format = "%d%b%Y:%H:%M:%S"),
    EVENT_END_DATETIME  = as.POSIXct(EVENT_END_DATETIME, format = "%d%b%Y:%H:%M:%S"),
    LAST_UPDATED_DATE = lubridate::dmy(LAST_UPDATED_DATE)
  )]
  
  
  setcolorder(moh_nmds_event_dt, name_vector)
  
  moh_nmds_event_dt = unique(moh_nmds_event_dt)
  
  return(moh_nmds_event_dt)

}
