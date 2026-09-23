#' Forest plot of average marginal effects across the ladder and quantiles
#'
#' The manuscript counterpart to `build_regression_forest_plot()`. That figure
#' shows raw coefficients and belongs with the diagnostics; this one shows
#' interpretable effects and belongs in the paper.
#'
#' The differences are worth stating because a reader comparing the two will
#' notice them. M3 appears here as a single average slope rather than two
#' uninterpretable spline basis rows. Ethnicity is expressed relative to the
#' population average rather than to a reference level, matching the exposure
#' tables. Terms entering linearly with no interaction are identical in both.
#'
#' Layout follows the coefficient forest: terms on y, effects with intervals on
#' x, ladder rungs coloured and dodged within a term, one facet column per
#' estimator. Faceting on the estimator rather than encoding it within a panel
#' is deliberate: a mean effect and a quantile effect are not commensurable and
#' should not sit on a shared axis inviting comparison.
#'
#' Returns geometry and faceting only. Scales, theme, axis titles and the
#' caption are applied at the output stage; this function sets no labels other
#' than the title, which identifies the branch.
#'
#' @param ame_dt long table from build_regression_ame_dt()
#' @param population population slug to plot
#' @param covariate_sets rung ids to include, in the order they should appear
#' @param covariate_set_labels named list mapping rung ids to legend labels
#' @param exposure_variables variables drawn in the upper row of panels
#' @param taus quantiles to include as facet columns; NULL for all present
#' @param include_lm include the linear model as the first facet column
#' @param title branch-identifying title
#'
#' @return list with `plot`, `height_in`, `n_terms`, `n_models`, `n_facets`
build_regression_ame_forest_plot <- function(ame_dt,
                                             population,
                                             covariate_sets,
                                             covariate_set_labels = NULL,
                                             exposure_variables = NULL,
                                             taus = NULL,
                                             include_lm = TRUE,
                                             title = NULL) {

  d <- data.table::as.data.table(ame_dt)
  d <- d[population_slug == population & covariate_set %chin% covariate_sets &
           !is.na(estimate)]

  if (!is.null(taus)) d <- d[model_type == "lm" | tau %in% taus]
  if (!isTRUE(include_lm)) d <- d[model_type != "lm"]

  if (!nrow(d))
    stop("build_regression_ame_forest_plot: no effects for ", population)

  d[, estimator := data.table::fifelse(
    model_type == "lm", "Mean",
    paste0("\u03c4 = ", format(tau, nsmall = 2)))]
  est_levels <- c(if (any(d$model_type == "lm")) "Mean",
                  sort(unique(d[model_type != "lm", estimator])))
  d[, estimator := factor(estimator, levels = est_levels)]

  lv <- intersect(covariate_sets, unique(d$covariate_set))
  d[, model_label := factor(
    label_or_self(covariate_set, covariate_set_labels),
    levels = label_or_self(lv, covariate_set_labels))]

  # Term order: first appearance down the ladder, reversed for the y axis.
  first_in <- d[, .(first = min(match(covariate_set, lv))), by = term_label]
  data.table::setorder(first_in, first, term_label)
  d[, term_label := factor(term_label, levels = rev(first_in$term_label))]

  has_rows <- !is.null(exposure_variables)
  if (has_rows)
    d[, panel_row := factor(
      data.table::fifelse(variable %chin% exposure_variables,
                          "Exposures", "Covariates"),
      levels = c("Exposures", "Covariates"))]

  dodge <- ggplot2::position_dodge(width = 0.7)

  p <- ggplot2::ggplot(
    d, ggplot2::aes(x = estimate, y = term_label, colour = model_label)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = conf_low, xmax = conf_high),
      orientation = "y", width = 0, linewidth = 0.4, position = dodge) +
    ggplot2::geom_point(
      ggplot2::aes(shape = as.character(nonunique_solution)),
      size = 1.5, stroke = 0.5, position = dodge)

  p <- p + if (has_rows)
    ggplot2::facet_grid(rows = ggplot2::vars(panel_row),
                        cols = ggplot2::vars(estimator),
                        scales = "free_y", space = "free_y")
    else ggplot2::facet_grid(cols = ggplot2::vars(estimator))

  if (!is.null(title)) p <- p + ggplot2::ggtitle(title)

  n_terms  <- data.table::uniqueN(d$term_label)
  n_models <- data.table::uniqueN(d$model_label)

  list(plot = p,
       height_in = min(14, 2.4 + n_terms * n_models * 0.12),
       n_terms = n_terms,
       n_models = n_models,
       n_facets = length(est_levels))
}
