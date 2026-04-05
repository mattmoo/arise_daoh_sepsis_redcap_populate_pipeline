#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ethnicity_lookup_data_path
generate_priority_ethnicity_lookup_dt <- function(ethnicity_lookup_data_path) {

  priority_ethnicity_lookup_dt = unique(fread(ethnicity_lookup_data_path)[, .(
    priority.ethnicity.code.L1 = factor(code.L1),
    priority.ethnicity.code.L2 = factor(code.L2),
    priority.ethnicity.desc.L1 = factor(
      desc.L1,
      levels = c(
        "Maori",
        "Pacific Peoples",
        "Asian",
        "European",
        "Middle Eastern/Latin American/African (MELAA)",
        "Other ethnicity",
        "Residual categories"
      ),
      labels = c(
        "Māori",
        "Pacific Peoples",
        "Asian",
        "European",
        "Other ethnicity",
        "Other ethnicity",
        "Other ethnicity"
      )
    ),
    priority.ethnicity.desc.L2 = factor(desc.L2)
  )])
  
  return(priority_ethnicity_lookup_dt)

}
