#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param redcap_uri
#' @param token
#' @param redcap_metadata_dt
#' @return
#' @author Matthew Moore
#' @export
pull_redcap_data <- function(redcap_uri, token,
                             redcap_metadata_dt) {

  
  dt = as.data.table(REDCapR::redcap_read_oneshot(
    redcap_uri = redcap_uri,
    token = token,
    forms = c('visit_or_admission', 'arise_criteria'),
    locale = readr::locale(tz = "Pacific/Auckland")
  )$data)
  
  # Loop through every column in the raw data
  for (col in names(dt)) {
    
    # Checkboxes are exported as "fieldname___1", so we extract the base name
    base_name <- sub("___.*", "", col) 
    
    # Find the corresponding row in the metadata
    meta_row <- redcap_metadata_dt[field_name == base_name]
    
    if (nrow(meta_row) > 0) {
      
      # 1. APPLY VARIABLE LABEL (for gtsummary)
      attr(dt[[col]], "label") <- meta_row$field_label[1]
      
      # 2. APPLY FACTORS (for radio buttons and dropdowns)
      # We only factor the base columns, skipping raw checkboxes for now
      if (meta_row$field_type[1] %in% c("radio", "dropdown") && col == base_name) {
        
        choice_string <- meta_row$select_choices_or_calculations[1]
        
        if (!is.na(choice_string) && choice_string != "") {
          # Parse REDCap's "1, Option A | 2, Option B" format
          options_list <- trimws(unlist(strsplit(choice_string, "\\|")))
          
          # Extract the numeric values (before the comma)
          val_levels <- trimws(sub(",.*", "", options_list))
          # Extract the text labels (after the comma)
          val_labels <- trimws(sub("^[^,]+,", "", options_list))
          
          # Convert the column to a factor
          dt[[col]] <- factor(dt[[col]], levels = val_levels, labels = val_labels)
        }
      }
    }
  }
  
  dt[, pms_unique_identifier := as.character(pms_unique_identifier)]
  
  return(dt)

}
