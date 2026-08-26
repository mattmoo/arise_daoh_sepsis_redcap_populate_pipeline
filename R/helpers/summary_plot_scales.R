#' Scales for the summary plots
#'
#' Kept together because they are a single set applied as one unit at output.
#'
#' Every scale carries its own `name`. Axis and legend titles are labels, and
#' labels are applied at the output stage, so the plotting functions set none
#' and the titles have to live here. A scale supplied without a name will fall
#' back to the mapped variable's expression, which for these plots is an
#' internal name such as `.value` or `prop`.


#' Fill scale for the stratification variable
#'
#' Legend title is taken from the label list, so it reads "ARISE eligible" or
#' "Ethnicity (L1, priority)" rather than the bare column name.
#'
#' @param by_var stratification variable name
#' @param labels named list of variable labels
#' @param palette RColorBrewer qualitative palette
summary_plot_fill_scale <- function(by_var, labels = NULL, palette = "Set2",
                                    ...) {
  ggplot2::scale_fill_brewer(
    palette = palette,
    name = if (!is.null(labels[[by_var]])) labels[[by_var]] else by_var,
    ...
  )
}


#' Grouped discrete x axis for the continuous panels
#'
#' @param by_var stratification variable name
#' @param labels named list of variable labels
summary_plot_group_x_scale <- function(by_var, labels = NULL, ...) {
  ggplot2::scale_x_discrete(
    name = if (!is.null(labels[[by_var]])) labels[[by_var]] else by_var,
    ...
  )
}


#' Percentage x axis for the categorical plots
#'
#' Left expansion is zeroed so bars start at the axis; right expansion leaves
#' room for the Wilson interval whiskers.
#'
#' @param name axis title
#' @param accuracy rounding for the percentage labels
#' @param expand axis expansion
summary_plot_percent_scale <- function(name = "Percentage within group",
                                       accuracy = 1,
                                       expand = ggplot2::expansion(c(0, 0.05)),
                                       ...) {
  ggplot2::scale_x_continuous(
    name   = name,
    labels = scales::percent_format(accuracy = accuracy),
    expand = expand,
    ...
  )
}


#' Fill scale for DAOH histograms stacked by vital status
#'
#' Deaths in black is semantic rather than decorative, so the values are fixed
#' rather than palette-driven. The legend needs no title: the two level names
#' say what they are.
summary_plot_mortality_fill_scale <- function(...) {
  ggplot2::scale_fill_manual(
    values = c("Alive at 90 days" = "grey75",
               "Died within 90 days" = "black"),
    name = NULL,
    ...
  )
}


#' Compressed count axis for the DAOH histogram
#'
#' Both sqrt and pseudo-log admit zero, unlike log. sqrt is the milder of the
#' two and is usually enough to lift the tail against the day-0 spike.
#'
#' Also serves the discrete distribution panels, where `trans = "none"` and the
#' name is set to a percentage instead.
#'
#' @param trans "sqrt", "pseudo_log" or "none"
#' @param name axis title
#' @param expand axis expansion
summary_plot_count_scale <- function(trans = c("sqrt", "pseudo_log", "none"),
                                     name = "Patients",
                                     expand = ggplot2::expansion(c(0, 0.05)),
                                     ...) {
  trans <- match.arg(trans)
  if (trans == "sqrt")
    ggplot2::scale_y_sqrt(name = name, expand = expand, ...)
  else if (trans == "pseudo_log") {
    ggplot2::scale_y_continuous(name = name,
                                trans = scales::pseudo_log_trans(base = 10),
                                expand = expand,
                                ...)
  } else {
    ggplot2::scale_y_continuous(name = name, expand = expand, ...)
  }
}


#' Day axis for DAOH plots
#'
#' Limits are padded a bin either side so the day-0 and day-max bars are drawn
#' whole rather than clipped at the panel edge, which matters here because both
#' endpoints carry mass.
#'
#' Note that `limits` on a continuous scale drops observations outside the range
#' rather than zooming, so the padding must cover the full observed range of
#' DAOH. Widen it rather than narrowing if in doubt.
#'
#' @param name axis title
#' @param breaks break positions in days
#' @param limits axis range, padded beyond the data
#' @param expand axis expansion; zero by default since the limits already carry
#'   the padding
summary_plot_daoh_x_scale <- function(
    name = "Days alive and out of hospital to 90 days",
    breaks = seq(0, 90, by = 10),
    limits = c(-1, 91),
    expand = ggplot2::expansion(0, 0),
    ...) {
  ggplot2::scale_x_continuous(
    name   = name,
    breaks = breaks,
    limits = limits,
    expand = expand,
    ...
  )
}

