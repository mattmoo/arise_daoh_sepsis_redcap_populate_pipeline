#' Plot exposure effect against quantile
#'
#' Effect on y, tau on x, one line per contrast with a confidence ribbon. The
#' linear model has no tau, so its estimate is drawn as a horizontal reference
#' band across the panel: the mean difference is a single number the quantile
#' curve can be read against, and where the curve crosses it is usually the most
#' informative feature of the figure.
#'
#' Panels are laid out one per row at the output stage. A grid would compress
#' each panel's y range into a column width, and since the point of the figure
#' is the shape of the effect across tau, vertical space is worth more than
#' horizontal. Height therefore grows linearly with the number of models;
#' `max_height_in = Inf` permits a figure taller than a page.
#'
#' Returns geometry only. Scales, faceting, theme, axis titles, legend titles
#' and the caption are all applied at the output stage. Axis and legend titles
#' come from the `name` argument of the corresponding scale; the caption comes
#' from a `labs()` object supplied alongside the scales.
#'
#' The one exception is the plot title, which identifies the branch: exposure,
#' population and model set. That is branch identity rather than styling, and it
#' has to be built here because this is where those values are known. Without it
#' the twenty-odd near-identical PDFs this target produces are indistinguishable
#' once opened, and only recoverable from the filename.
#'
#' The shape aesthetic is coerced to character so it matches a manual scale
#' keyed on "TRUE" and "FALSE". Mapping the logical directly produces a discrete
#' scale whose levels are logicals, which will not match.
#'
#' @param effect_dt long effect table from extract_regression_effects()
#' @param exposure_var exposure to plot; named to avoid colliding with the
#'   `exposure` column during data.table evaluation
#' @param covariate_sets rung ids to include, in the order they should appear
#' @param populations population slugs to include; NULL for all present
#' @param labels named list of variable labels, used only to build the title
#' @param population_labels named list mapping population slugs to names, used
#'   only to build the subtitle
#' @param covariate_set_labels named list mapping rung ids to descriptions, used
#'   in the subtitle when a single model is plotted
#' @param identify_branch add a title naming the exposure, population and models
#' @param show_lm draw the linear model estimate as a horizontal reference band
#' @param panel_height_in vertical space allowed per panel
#' @param max_height_in cap on total height; Inf permits a multi-page figure
#'
#' @return list with `plot`, `height_in`, `n_panels`, `n_unfittable`, `flagged`,
#'   `truncated_height`
build_regression_tau_plot <- function(effect_dt,
                                      exposure_var,
                                      covariate_sets,
                                      populations = NULL,
                                      labels = NULL,
                                      population_labels = NULL,
                                      covariate_set_labels = NULL,
                                      identify_branch = TRUE,
                                      show_lm = TRUE,
                                      panel_height_in = 1.7,
                                      max_height_in = 9.5) {

  d <- data.table::as.data.table(effect_dt)
  d <- d[exposure == exposure_var & covariate_set %chin% covariate_sets]
  if (!is.null(populations)) d <- d[population_slug %chin% populations]

  if (!nrow(d))
    stop("build_regression_tau_plot: no rows for '", exposure_var, "' with ",
         paste(covariate_sets, collapse = ", "))

  # Panel order follows the supplied vector so the figure reads in the same
  # sequence as the corresponding table.
  d[, covariate_set := factor(covariate_set,
                              levels = intersect(covariate_sets,
                                                 unique(covariate_set)))]

  rq_d <- d[model_type == "rq"]
  lm_d <- d[model_type == "lm" & estimable == TRUE]

  p <- ggplot2::ggplot(rq_d, ggplot2::aes(x = tau, y = estimate,
                                          colour = contrast, fill = contrast))

  if (isTRUE(show_lm) && nrow(lm_d)) {
    p <- p +
      ggplot2::geom_rect(
        data = lm_d,
        ggplot2::aes(xmin = -Inf, xmax = Inf,
                     ymin = conf_low, ymax = conf_high, fill = contrast),
        inherit.aes = FALSE, alpha = 0.10) +
      ggplot2::geom_hline(
        data = lm_d,
        ggplot2::aes(yintercept = estimate, colour = contrast),
        linetype = "dashed", linewidth = 0.4)
  }

  p <- p +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
    ggplot2::geom_ribbon(
      data = rq_d[estimable == TRUE & !is.na(conf_low)],
      ggplot2::aes(ymin = conf_low, ymax = conf_high, group = contrast),
      alpha = 0.15, colour = NA) +
    ggplot2::geom_line(
      data = rq_d[estimable == TRUE],
      ggplot2::aes(group = contrast), linewidth = 0.5) +
    # Hollow points mark a non-unique rq solution: the objective has a flat
    # region and the plotted value is one vertex of an interval of solutions.
    ggplot2::geom_point(
      data = rq_d[estimable == TRUE & tau_role == "primary"],
      ggplot2::aes(shape = as.character(nonunique_solution)),
      size = 1.8, stroke = 0.6) +
    ggplot2::geom_rug(
      data = unique(rq_d[estimable == FALSE],
                    by = c("population_slug", "covariate_set", "tau")),
      ggplot2::aes(x = tau), inherit.aes = FALSE,
      sides = "b", colour = "grey55", linewidth = 0.3)

  if (isTRUE(identify_branch)) {
    lk <- function(x, l) if (!is.null(l[[x]])) as.character(l[[x]]) else x

    pops <- unique(as.character(d$population_slug))
    rungs <- levels(droplevels(d$covariate_set))

    # A single model is named; several are counted, since listing six rung
    # labels in a subtitle is longer than the panel strips that already show
    # them.
    model_txt <- if (length(rungs) == 1L)
      lk(rungs, covariate_set_labels)
      else paste(length(rungs), "models")

    p <- p + ggplot2::ggtitle(
      label = lk(exposure_var, labels),
      subtitle = paste0(
        paste(vapply(pops, lk, "", population_labels), collapse = "; "),
        " \u00b7 ", model_txt))
  }

  # One panel per row, so height is linear in panel count. Fixed allowance is
  # for the axis, legend and caption added at output.
  n_panels <- nrow(unique(rq_d, by = c("population_slug", "covariate_set")))
  wanted <- 2.4 + n_panels * panel_height_in

  list(plot = p,
       height_in = min(wanted, max_height_in),
       n_panels = n_panels,
       n_unfittable = rq_d[estimable == FALSE, .N],
       flagged = rq_d[nonunique_solution == TRUE | nonpositive_fis == TRUE, .N],
       truncated_height = wanted > max_height_in)
}
