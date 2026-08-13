#' Scales for the summary plots
#'
#' Kept together because they are a single set applied as one unit at output,
#' and because the fill scale's legend title is the only thing that varies
#' between branches.

#' Fill scale for the stratification variable
#'
#' Legend title is taken from the label list, so it reads "ARISE eligible" or
#' "Ethnicity (L1, priority)" rather than the bare column name.
#'
#' @param by_var stratification variable name
#' @param labels named list of variable labels
#' @param palette RColorBrewer qualitative palette
summary_plot_fill_scale <- function(by_var, labels = NULL, palette = "Set2") {
  ggplot2::scale_fill_brewer(
    palette = palette,
    name = if (!is.null(labels[[by_var]])) labels[[by_var]] else by_var
  )
}


#' Percentage x axis for the categorical plots
#'
#' Left expansion is zeroed so bars start at the axis; right expansion leaves
#' room for the Wilson interval whiskers.
#'
#' @param accuracy rounding for the percentage labels
summary_plot_percent_scale <- function(accuracy = 1) {
  ggplot2::scale_x_continuous(
    labels = scales::percent_format(accuracy = accuracy),
    expand = ggplot2::expansion(c(0, 0.05))
  )
}

#' Fill scale for DAOH histograms stacked by vital status
#'
#' Deaths in black is semantic rather than decorative, so the values are fixed
#' rather than palette-driven.
summary_plot_mortality_fill_scale <- function() {
  ggplot2::scale_fill_manual(
    values = c("Alive at 90 days" = "grey75",
               "Died within 90 days" = "black"),
    name = NULL
  )
}


#' Compressed count axis for the DAOH histogram
#'
#' Both sqrt and pseudo-log admit zero, unlike log. sqrt is the milder of the
#' two and is usually enough to lift the tail against the day-0 spike.
#'
#' @param trans "sqrt" or "pseudo_log"
summary_plot_count_scale <- function(trans = c("sqrt", "pseudo_log", "none"),
                                     expand = ggplot2::expansion(c(0, 0.05)),
                                     ...) {
  trans <- match.arg(trans)
  if (trans == "sqrt")
    ggplot2::scale_y_sqrt(expand = expand, ...)
  else if (trans == "pseudo_log") {
    ggplot2::scale_y_continuous(trans = scales::pseudo_log_trans(base = 10),
                                expand = expand,
                                ...)
  } else {
    ggplot2::scale_y_continuous(expand = expand, ...)
  }
}


#' Day axis for DAOH plots
#'
#' Limits are padded half a bin either side so the day-0 and day-max bars are
#' drawn whole rather than clipped at the panel edge, which matters here
#' because both endpoints carry mass.
#'
#' @param daoh_max upper bound of the DAOH window
#' @param by break interval in days
#' @param expand axis expansion; zero by default since the limits already
#'   carry the padding
summary_plot_daoh_x_scale <- function(breaks = seq(0, 90, by = 10),
                                      limits = c(-1, 90 + 1),
                                      expand = ggplot2::expansion(0, 0),
                                      ...) {
  ggplot2::scale_x_continuous(
    breaks = breaks,
    limits = limits,
    expand = expand,
    ...
  )
}