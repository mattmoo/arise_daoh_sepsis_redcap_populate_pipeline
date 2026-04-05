#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param redcap_export_dt
#' @return
#' @author Matthew Moore
#' @export
export_to_redcap <- function(redcap_export_dt, nhi_decryption_fn = NULL) {
  
  if (!is.null(nhi_decryption_fn)) {
    redcap_export_dt[, encrypted_nhi := nhi]
    redcap_export_dt[, nhi := nhi_decryption_fn(nhi)]
  }

  result_list = REDCapR::redcap_write_oneshot(
    ds = as.data.frame(redcap_export_dt),
    redcap_uri = 'https://redcap.fmhs.auckland.ac.nz/api/',
    token = keyring::key_get("REDCAP_API", keyring = 'arise')
  )
  
  return(result_list)

}
