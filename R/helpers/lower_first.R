#' Lower-case the first letter of a label for use mid-sentence, leaving
#' acronyms ("ARISE eligible") and later proper nouns ("Māori") alone.
lower_first <- function(x) {
  ifelse(grepl("^[A-Z][a-z]", x),
         paste0(tolower(substr(x, 1, 1)), substring(x, 2)), x)
}