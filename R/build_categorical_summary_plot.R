#' Build a proportion bar panel grid for categorical variables
#'
#' Returns geometry and faceting only; themes and scales are applied at the
#' output stage. Panels stack vertically with `space = "free_y"` so panel
#' height is proportional to level count, because category labels need
#' horizontal room and panels vary in how many levels they carry.
#'
#' Wilson intervals are drawn on every bar: a bar at 40% from twelve patients
#' otherwise looks identical to one from 136.
#'
#' Dichotomous variables are reduced to their affirmative level only, since
#' the two bars are mirror images and the second carries no information.
#'
#' @param dt data.table
#' @param vars categorical variables to plot; logicals and characters are
#'   coerced to factors
#' @param by_var grouping variable (NULL for ungrouped)
#' @param labels named list of variable labels, used for facet strips
#' @param max_levels variables with more levels than this are lumped, since a
#'   38-level panel is unreadable at 6 inches wide
#' @param conf coverage for the Wilson interval
#' @param positive_levels level names treated as the affirmative category of a
#'   dichotomous variable, in order of preference; the second factor level is
#'   used if none match
#'
#' @return list with `plot`, `height_in`, `n_panels`, `n_groups`, `binary_vars`
build_categorical_summary_plot <- function(dt,
                                           vars,
                                           by_var = NULL,
                                           labels = NULL,
                                           max_levels = 10L,
                                           conf = 0.95,
                                           positive_levels = c("Yes", "TRUE",
                                                               "Died", "Y")) {
  
  d <- data.table::as.data.table(dt)
  
  vars <- intersect(vars, names(d))
  if (!length(vars))
    stop("build_categorical_summary_plot: no requested variables in data")
  
  lab <- function(v) if (!is.null(labels[[v]])) labels[[v]] else v
  
  d <- d[, c(vars, by_var), with = FALSE]
  
  if (!is.null(by_var)) {
    d <- d[!is.na(get(by_var))]
    data.table::setnames(d, by_var, ".group")
    d[, .group := factor(.group)]
  } else {
    d[, .group := factor("All")]
  }
  
  n_groups <- data.table::uniqueN(d$.group)
  
  # ---- coerce to factors and lump long tails -------------------------------
  for (v in vars) {
    x <- d[[v]]
    if (is.logical(x))
      data.table::set(d, j = v, value = factor(x, levels = c(FALSE, TRUE),
                                               labels = c("No", "Yes")))
    else if (!is.factor(x))
      data.table::set(d, j = v, value = factor(x))
    
    # Drop unused levels before counting. A variable declared with five levels
    # but carrying only Yes/No in this population is dichotomous in practice,
    # and would otherwise be missed by the binary check below.
    data.table::set(d, j = v, value = droplevels(d[[v]]))
    
    if (data.table::uniqueN(stats::na.omit(d[[v]])) > max_levels)
      data.table::set(d, j = v,
                      value = forcats::fct_lump_n(d[[v]], n = max_levels,
                                                  other_level = "Other"))
  }
  
  # ---- identify dichotomous variables and their affirmative level ----------
  binary_vars <- vars[vapply(
    vars, \(v) data.table::uniqueN(stats::na.omit(d[[v]])) == 2L, TRUE)]
  
  binary_positive <- vapply(binary_vars, function(v) {
    lv  <- levels(d[[v]])
    hit <- intersect(positive_levels, lv)
    # Fall back to the second level: for a factor built from a logical that is
    # TRUE, and for an alphabetical two-level factor it is usually the
    # affirmative (No/Yes).
    if (length(hit)) hit[1L] else lv[2L]
  }, "")
  
  # ---- long format and within-group proportions ----------------------------
  long <- data.table::melt(
    d, id.vars = ".group", measure.vars = vars,
    variable.name = ".var", value.name = ".level", value.factor = FALSE
  )[!is.na(.level)]
  
  prop <- long[, .N, by = .(.var, .group, .level)]
  
  # Denominator is the group total within each variable, so percentages sum to
  # 100 down a panel. Computed before any rows are dropped, so the retained
  # affirmative bar of a dichotomous variable still reads as a share of the
  # whole group rather than of the surviving rows.
  prop[, `:=`(total = sum(N), prop = N / sum(N)), by = .(.var, .group)]
  
  z <- stats::qnorm(1 - (1 - conf) / 2)
  prop[, `:=`(
    lower = (prop + z^2 / (2 * total) -
               z * sqrt((prop * (1 - prop) + z^2 / (4 * total)) / total)) /
      (1 + z^2 / total),
    upper = (prop + z^2 / (2 * total) +
               z * sqrt((prop * (1 - prop) + z^2 / (4 * total)) / total)) /
      (1 + z^2 / total)
  )]
  
  # ---- drop the redundant negative bar -------------------------------------
  # Must run while .var still holds raw variable names, before relabelling.
  if (length(binary_vars)) {
    prop <- prop[!(.var %chin% binary_vars) |
                   .level == binary_positive[match(.var, binary_vars)]]
    # The facet strip already names the variable, so "Yes" on the axis is
    # redundant. Blanking it collapses the panel to a single unlabelled row,
    # which is what makes the space = "free_y" sizing pay off.
    prop[.var %chin% binary_vars, .level := ""]
  }
  
  # ---- labelling and ordering ----------------------------------------------
  prop[, .var := factor(vapply(as.character(.var), lab, ""),
                        levels = vapply(vars, lab, ""))]
  # Reversed so the first level sits at the top of each panel: ggplot builds
  # discrete y axes from the bottom up.
  prop[, .level := factor(.level, levels = rev(sort(unique(.level))))]
  
  caption <- sprintf(
    "Bars are percentages within each group; lines are %d%% Wilson intervals.",
    round(conf * 100))
  if (length(binary_vars))
    caption <- paste(caption,
                     "Binary variables show the affirmative category only.")
  
  p <- ggplot2::ggplot(prop, ggplot2::aes(x = prop, y = .level,
                                          fill = .group)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8),
                      width = 0.7) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = lower, xmax = upper),
      position = ggplot2::position_dodge(width = 0.8),
      height = 0, linewidth = 0.3, colour = "grey25") +
    ggplot2::facet_grid(rows = ggplot2::vars(.var), scales = "free_y",
                        space = "free_y", switch = "y") +
    ggplot2::labs(x = "Percentage within group", y = NULL, caption = caption)
  
  # Height scales with the total number of bar rows across panels, times the
  # number of dodged groups, plus a fixed allowance per panel for the strip
  # and spacing. Capped at 9.5 inches so nothing overflows an A4 page.
  n_rows <- prop[, data.table::uniqueN(.level), by = .var][, sum(V1)]
  
  list(plot = p,
       height_in = min(9.5, 1.4 + n_rows * n_groups * 0.16 +
                         length(vars) * 0.25),
       n_panels = length(vars),
       n_groups = n_groups,
       binary_vars = binary_vars)
}