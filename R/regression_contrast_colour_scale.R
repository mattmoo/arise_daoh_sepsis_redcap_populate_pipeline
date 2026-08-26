#' Colour and fill scales for regression contrasts
#'
#' Colour and fill are returned together as a list because every contrast layer
#' uses both (line and ribbon), and setting one without the other produces a
#' legend split across two keys.
#'
#' The legend title is taken from the exposure's entry in the variable label
#' list, so it reads "Ethnicity (L1, priority)" rather than the column name.
#'
#' @param exposure_var exposure variable name
#' @param labels named list of variable labels
#' @param palette RColorBrewer qualitative palette; Dark2 is used rather than
#'   Set2 to distinguish regression figures from the descriptive ones
#' @param drop retain unused factor levels, so a group that could not be
#'   estimated in one panel keeps its colour in the others
regression_contrast_colour_scale <- function(exposure_var,
                                             labels = NULL,
                                             palette = "Dark2",
                                             drop = FALSE,
                                             ...) {

  nm <- if (!is.null(labels[[exposure_var]])) labels[[exposure_var]]
        else exposure_var

  list(
    ggplot2::scale_colour_brewer(palette = palette, name = nm, drop = drop, ...),
    ggplot2::scale_fill_brewer(palette = palette, name = nm, drop = drop, ...)
  )
}
