#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param pkg_vector
#' @param path
#' @return
#' @author Matthew Moore
#' @export
write_bibtex <- function(pkg_vector, 
                         filename = 'analysis_citations.bib',
                         path,
                         add_base = TRUE,
                         add_rstudio = TRUE) {
  
  fn = file.path(path, filename)
  file.create(fn)
  
  # writeLines(fn, 
  #            text = toBibtex(citation_bibentry))
  if (add_base == TRUE) {
    pkg_vector = c("base", pkg_vector)
  }
  
  knitr::write_bib(pkg_vector, file = fn)
  
  if (add_rstudio) {
    # Convert the bibentry object to a character vector
    rstudio_bib_string = toBibtex(rstudioapi::versionInfo()$citation)
    
    # Use base R's write() function to append the text
    # We add a newline for good spacing
    write(c("\n", rstudio_bib_string), file = fn, append = TRUE)
  }
  
  return(fn)
  
}
