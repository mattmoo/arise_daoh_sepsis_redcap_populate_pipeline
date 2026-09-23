#' Forest plot of model estimates across the ladder and across quantiles
#'
#' Serves both estimate tables, which differ in what they contain but not in how
#' they should be drawn:
#'
#'   `build_regression_coef_dt()` gives raw coefficients. Categorical terms are
#'   relative to a reference level and a spline appears as two uninterpretable
#'   basis rows. Useful as a diagnostic, since it shows the model as fitted.
#'
#'   `build_regression_ame_dt()` gives average marginal effects. A spline
#'   collapses to one average slope and ethnicity is expressed relative to the
#'   population average. This is the one for a manuscript.
#'
#' They agree for a continuous term entering linearly with no interaction, and
#' diverge elsewhere, so the caption supplied at output has to say which is
#' being shown.
#'
#' Layout: terms on y, estimates with intervals on x, ladder rungs coloured and
#' dodged within a term, one facet column per estimator, and exposures split
#' into their own row of panels so a dozen covariate rows do not bury the terms
#' the analysis is about.
#'
#' Faceting on the estimator rather than encoding it within a panel is
#' deliberate: a mean effect and a quantile effect are not commensurable and
#' should not share an axis inviting comparison. Within a panel the ladder is
#' commensurable, so it takes colour and dodging.
#'
#' `group` is set in the top-level aesthetic. `position_dodge()` groups by the
#' interaction of every discrete aesthetic in a layer, so a point layer carrying
#' an extra shape mapping would dodge into more slots than the interval layer
#' and the two would separate vertically. Setting the group explicitly keeps
#' every layer on the same offsets.
#'
#' Returns geometry and faceting only. Scales, theme, axis titles and the
#' caption are applied at the output stage; this function sets no labels other
#' than the title, which identifies the branch.
#'
#' @param estimate_dt long table with `term_label`, `estimate`, `conf_low`,
#'   `conf_high` and the spec columns, from either extractor
#' @param population population slug to plot
#' @param covariate_sets rung ids to include, in the order they should appear
#' @param covariate_set_labels named list mapping rung ids to legend labels
#' @param term_groups named character vector mapping variable to a display
#'   block, from `regression_term_groups()`. Rows are grouped by block rather
#'   than split merely into exposures and covariates: the six vital sign
#'   components enter the model as one block and are an alternative to a single
#'   NEWS term, so reading them as six unrelated covariates misses the point.
#'   Blocks appear in the order of the vector's unique values.
#' @param exposure_terms terms drawn in the upper block when `term_groups` is
#'   not supplied; matched against `variable` where that column exists,
#'   otherwise as a prefix of `term`
#' @param taus quantiles to include as facet columns; NULL for all present
#' @param include_lm include the linear model as the first facet column
#' @param x_scales how the x axis is shared between panels. "free" gives every
#'   panel its own range, which is usually necessary: a single large covariate
#'   coefficient otherwise compresses every other term into a sliver, and
#'   continuous covariates measured per unit are orders of magnitude smaller
#'   than a categorical contrast in days. "free_row" shares a scale across a
#'   row, so estimators stay comparable within the exposure and covariate
#'   blocks. "fixed" shares one scale throughout, which is only readable when
#'   every term is of similar magnitude.
#' @param show_solution_flag mark non-unique quantile regression solutions with
#'   a hollow point. Off by default: it is common enough with a tied outcome
#'   that flagging it everywhere adds a legend and a second shape for something
#'   that does not change the estimate, and it is recorded in the model
#'   inventory either way.
#' @param title branch-identifying title
#'
#' @return list with `plot`, `height_in`, `n_terms`, `n_models`, `n_facets`
build_regression_forest_plot <- function(estimate_dt,
                                         population,
                                         covariate_sets,
                                         covariate_set_labels = NULL,
                                         term_groups = NULL,
                                         exposure_terms = NULL,
                                         taus = NULL,
                                         include_lm = TRUE,
                                         x_scales = c("free_row", "free",
                                                      "fixed"),
                                         show_solution_flag = FALSE,
                                         title = NULL) {
  
  x_scales <- match.arg(x_scales)
  
  d <- data.table::as.data.table(estimate_dt)
  
  req <- c("term_label", "estimate", "conf_low", "conf_high",
           "population_slug", "covariate_set", "model_type")
  missing_cols <- setdiff(req, names(d))
  if (length(missing_cols))
    stop("build_regression_forest_plot: missing columns: ",
         paste(missing_cols, collapse = ", "))
  
  d <- d[population_slug == population & covariate_set %chin% covariate_sets &
           !is.na(estimate)]
  
  if (!is.null(taus)) d <- d[model_type == "lm" | tau %in% taus]
  if (!isTRUE(include_lm)) d <- d[model_type != "lm"]
  
  if (!nrow(d))
    stop("build_regression_forest_plot: no estimates for ", population)
  
  # ---- facet columns: the mean first, then quantiles ascending -------------
  d[, estimator := data.table::fifelse(
    model_type == "lm", "Mean",
    paste0("\u03c4 = ", format(tau, nsmall = 2)))]
  est_levels <- c(if (any(d$model_type == "lm")) "Mean",
                  sort(unique(d[model_type != "lm", estimator])))
  d[, estimator := factor(estimator, levels = est_levels)]
  
  # ---- legend order follows the ladder, not the alphabet ------------------
  lv <- intersect(covariate_sets, unique(d$covariate_set))
  d[, model_label := factor(
    label_or_self(covariate_set, covariate_set_labels),
    levels = label_or_self(lv, covariate_set_labels))]
  
  # ---- term order: first appearance down the ladder -----------------------
  # Reversed for the y axis, which ggplot builds from the bottom up, so a
  # covariate added at a later rung sits lower.
  first_in <- d[, .(first = min(match(covariate_set, lv))), by = term_label]
  data.table::setorder(first_in, first, term_label)
  d[, term_label := factor(term_label, levels = rev(first_in$term_label))]
  
  # ---- row blocks ---------------------------------------------------------
  # Preferred: group by covariate block, so the vital signs sit together and a
  # reader sees the ladder's structure rather than a flat list of terms.
  # Fallback: the older exposure/covariate split, for callers that have not
  # supplied a mapping.
  var_of <- function() if ("variable" %chin% names(d)) d$variable else d$term
  
  has_rows <- !is.null(term_groups) || !is.null(exposure_terms)
  
  if (!is.null(term_groups)) {
    
    v <- var_of()
    grp <- term_groups[v]
    # Prefix match for coefficient tables, where the term carries a factor
    # level appended to the variable name.
    miss <- is.na(grp)
    if (any(miss)) {
      cand <- names(term_groups)[order(nchar(names(term_groups)),
                                       decreasing = TRUE)]
      grp[miss] <- vapply(v[miss], function(tm) {
        hit <- cand[startsWith(tm, cand)]
        if (length(hit)) term_groups[[hit[1L]]] else NA_character_
      }, "")
    }
    grp[is.na(grp)] <- "Other"
    
    d[, panel_row := factor(grp, levels = intersect(
      c(unique(term_groups), "Other"), unique(grp)))]
    
  } else {
    
    is_exp <- if ("variable" %chin% names(d))
      d$variable %chin% exposure_terms
    else
      Reduce(`|`, lapply(exposure_terms, \(v) startsWith(d$term, v)))
    d[, panel_row := factor(
      data.table::fifelse(is_exp, "Exposures", "Covariates"),
      levels = c("Exposures", "Covariates"))]
  }
  
  # Shape marks a non-unique quantile regression solution, off by default. The
  # flag does not change the estimate, only signals that an interval of
  # coefficient vectors fits the objective equally well, and with a tied
  # outcome it fires often enough that a second shape and a legend key are more
  # clutter than information. It remains in the model inventory.
  has_shape <- isTRUE(show_solution_flag) &&
    "nonunique_solution" %chin% names(d)
  if (has_shape) d[, solution := as.character(nonunique_solution)]
  
  dodge <- ggplot2::position_dodge(width = 0.7)
  
  p <- ggplot2::ggplot(
    d, ggplot2::aes(x = estimate, y = term_label,
                    colour = model_label, group = model_label)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = conf_low, xmax = conf_high),
      orientation = "y", width = 0, linewidth = 0.4, position = dodge)
  
  p <- p + if (has_shape)
    ggplot2::geom_point(ggplot2::aes(shape = solution),
                        size = 1.5, stroke = 0.5, position = dodge)
  else ggplot2::geom_point(size = 1.5, stroke = 0.5, position = dodge)
  
  # facet_grid can only free a scale across a whole row or column, so a truly
  # per-panel x scale needs facet_wrap, which in turn gives up the proportional
  # row heights that keep the exposure block from taking as much space as the
  # covariate block. The trade is worth it: with a shared x axis the small
  # estimates are invisible, which defeats the figure.
  p <- p + if (x_scales == "free" && has_rows) {
    ggplot2::facet_wrap(ggplot2::vars(panel_row, estimator),
                        scales = "free", ncol = length(est_levels))
  } else if (has_rows) {
    ggplot2::facet_grid(rows = ggplot2::vars(panel_row),
                        cols = ggplot2::vars(estimator),
                        scales = if (x_scales == "free_row") "free" else "free_y",
                        space = "free_y")
  } else if (x_scales == "fixed") {
    ggplot2::facet_grid(cols = ggplot2::vars(estimator))
  } else {
    ggplot2::facet_wrap(ggplot2::vars(estimator), scales = "free",
                        nrow = 1)
  }
  
  if (!is.null(title)) p <- p + ggplot2::ggtitle(title)
  
  n_terms  <- data.table::uniqueN(d$term_label)
  n_models <- data.table::uniqueN(d$model_label)
  
  list(plot = p,
       # Height is rows times dodged models: a term shown for six rungs needs
       # six times the vertical room of a single estimate.
       height_in = min(14, 2.4 + n_terms * n_models * 0.12),
       n_terms = n_terms,
       n_models = n_models,
       n_facets = length(est_levels))
}