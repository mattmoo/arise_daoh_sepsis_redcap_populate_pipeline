#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param redcap_uri
#' @param token
#' @return
#' @author Matthew Moore
#' @export
pull_redcap_metadata <- function(redcap_uri, token) {

  as.data.table(REDCapR::redcap_metadata_read(
    redcap_uri = redcap_uri,
    token = token
  )$data)

}
