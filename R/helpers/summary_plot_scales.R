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

#' Scales and caption for the power figure
#'
#' Kept together because they are a single set applied as one unit at output,
#' matching the pattern used for the summary and regression scales. Each carries
#' its own `name`, since the plotting function sets no labels.


#' Sample size axis
#'
#' Breaks are derived from the plotted range rather than fixed, so widening the
#' simulated range does not leave the axis labelled to the old maximum.
#'
#' @param name axis title
#' @param n approximate number of breaks
#' @param expand axis expansion
power_plot_n_scale <- function(name = "Total participants",
                               n = 8,
                               expand = ggplot2::expansion(c(0.02, 0.02)),
                               ...) {
  ggplot2::scale_x_continuous(
    name   = name,
    breaks = scales::breaks_pretty(n = n),
    expand = expand,
    ...
  )
}


#' Power axis
#'
#' Fixed at 0 to 1. Power is a proportion with meaningful endpoints, and a free
#' axis would let a curve spanning 0.1 to 0.4 fill the panel and read as though
#' it covered the useful range.
#'
#' @param name axis title
#' @param by break interval
#' @param accuracy rounding for the percentage labels
power_plot_power_scale <- function(name = "Power",
                                   by = 0.2,
                                   accuracy = 1,
                                   expand = ggplot2::expansion(c(0.01, 0.01)),
                                   ...) {
  ggplot2::scale_y_continuous(
    name   = name,
    labels = scales::percent_format(accuracy = accuracy),
    limits = c(0, 1),
    breaks = seq(0, 1, by = by),
    expand = expand,
    ...
  )
}


#' Colour and fill scales for the effect sizes shown
#'
#' Returned together, and sharing a `name`, because the curve uses colour and
#' the uncertainty envelope uses fill. ggplot merges the two into one legend
#' only when the titles and key values match exactly; supply one without the
#' other, or with a different title, and the figure gets two legends for the
#' same variable.
#'
#' @param name legend title, applied to both scales
#' @param palette RColorBrewer qualitative palette
power_plot_delta_scale <- function(name = "Difference in mean DAOH90",
                                   palette = "Dark2",
                                   ...) {
  list(
    ggplot2::scale_colour_brewer(palette = palette, name = name, ...),
    ggplot2::scale_fill_brewer(palette = palette, name = name, ...)
  )
}


#' Caption for the power figure
#'
#' Records what a reader cannot recover from the figure: what the reference
#' lines mark, what the envelope spans and what it does not mean, what the
#' dashed lines are, and why the whole calculation is likely a little
#' optimistic.
#'
#' @param n_planned trial size marked by the vertical reference line
#' @param p_observed proportion Māori observed in this cohort
#' @param p_protocol proportion Māori assumed in the protocol
#' @param alpha significance level
#' @param envelope whether the uncertainty envelope is drawn
#' @param protocol whether the protocol assumption line is drawn
#' @param extra additional sentence for figure-specific caveats
power_plot_caption <- function(n_planned = 300,
                               p_observed = NULL,
                               p_protocol = 0.20,
                               alpha = 0.05,
                               envelope = TRUE,
                               protocol = TRUE,
                               extra = NULL) {
  
  pct <- function(x) paste0(round(x * 100), "%")
  
  txt <- paste0(
    "Two-sided comparison of mean DAOH90 at alpha ", alpha,
    ", from pwr::pwr.t2n.test(). Solid curves use the standard deviations and ",
    "the M\u0101ori proportion",
    if (!is.null(p_observed)) paste0(" (", pct(p_observed), ")") else "",
    " estimated in this study.")
  
  if (isTRUE(envelope))
    txt <- paste(txt, paste0(
      "The shaded envelope spans the power implied by the confidence ",
      "intervals around both of those estimates. It is a conservative ",
      "sensitivity range, not a joint confidence interval: both inputs ",
      "sitting simultaneously at their extremes is less likely than either ",
      "alone."))
  
  if (isTRUE(protocol))
    txt <- paste(txt, paste0(
      "Dashed curves use the ", pct(p_protocol),
      " M\u0101ori proportion assumed in the protocol, for comparison."))
  
  txt <- paste(txt, paste0(
    "Horizontal lines mark 80% and 90% power",
    if (!is.null(n_planned))
      paste0("; the vertical marks the ", n_planned,
             " participants anticipated for ARISE FLUIDS") else "",
    ". The calculation compares means and assumes approximate normality of ",
    "each group mean; DAOH90 has a large mass at zero and a ceiling at 90 ",
    "days, so power at smaller sample sizes is likely to be slightly ",
    "overstated. Rank-based, quantile and ordinal analyses may have different ",
    "power on this outcome and are not evaluated here."))
  
  if (!is.null(extra)) txt <- paste(txt, extra)
  
  ggplot2::labs(caption = txt)
}

#' Colour and fill scales for ethnicity contrasts against the population average
#'
#' `avg_predictions()` with a population-reference hypothesis returns contrast
#' labels of the form "Māori vs population". That is precise but repeats "vs
#' population" once per legend key, which is four times in a four-group legend
#' and reads as noise. This strips the suffix from the keys and states the
#' comparison once, in the legend title.
#'
#' Colour and fill are returned together because every contrast layer uses both
#' (line and ribbon), and setting one without the other produces a legend split
#' across two keys.
#'
#' `drop = FALSE` keeps a group's colour stable across panels. Māori and Asian
#' strata are small enough that a model may be non-estimable in one panel and
#' not another; without this, colours would shift between panels of the same
#' figure.
#'
#' Level order is fixed rather than alphabetical, so Māori appears first
#' throughout. The secondary objective of this study concerns Māori outcomes
#' specifically, and the figure should not bury that group in the middle of a
#' legend sorted by accident of spelling.
#'
#' @param name legend title; states the comparison once so the keys need not
#' @param palette RColorBrewer qualitative palette
#' @param levels contrast labels in the order they should appear in the legend
#' @param strip_suffix text removed from each key label
#' @param drop drop unused levels
regression_ethnicity_contrast_scale <- function(
    name = "Compared with the population average",
    palette = "Dark2",
    levels = c("M\u0101ori", "Pacific Peoples", "Asian", "European/Other"),
    strip_suffix = " vs population",
    drop = FALSE,
    ...) {
  
  # Keys are relabelled rather than the data being edited, so the underlying
  # contrast names stay intact for joining and for the tables.
  relabel <- function(x) sub(strip_suffix, "", x, fixed = TRUE)
  
  full_levels <- paste0(levels, strip_suffix)
  
  list(
    ggplot2::scale_colour_brewer(
      palette = palette, name = name, drop = drop,
      limits = full_levels, labels = relabel, ...),
    ggplot2::scale_fill_brewer(
      palette = palette, name = name, drop = drop,
      limits = full_levels, labels = relabel, ...)
  )
}


#' Colour and fill scales for the ARISE eligibility contrast
#'
#' A single contrast, so the legend carries one key and the title would repeat
#' it. The title is dropped and the key label left to say what it is.
#'
#' @param palette RColorBrewer qualitative palette
regression_arise_contrast_scale <- function(palette = "Dark2", ...) {
  list(
    ggplot2::scale_colour_brewer(palette = palette, name = NULL, ...),
    ggplot2::scale_fill_brewer(palette = palette, name = NULL, ...)
  )
}


power_plot_delta_fill_scale <- function(name = "Difference in mean DAOH90",
                                        palette = "Dark2", ...) {
  ggplot2::scale_fill_brewer(palette = palette, name = name, ...)
}