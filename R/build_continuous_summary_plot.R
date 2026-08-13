#' Build a boxplot panel grid for continuous variables
#'
#' Returns geometry and faceting only; themes and scales are applied at the
#' output stage. Panels wrap into columns with free y scales, because every
#' panel shares the group axis and differs only in units.
#'
#' Jittered raw points are drawn over the boxes deliberately: at n = 272
#' falling to a dozen in the smallest strata, showing the observations makes
#' thin strata visually obvious in a way a box alone hides.
#'
#' @param dt data.table
#' @param vars continuous variables to plot
#' @param by_var grouping variable (NULL for ungrouped)
#' @param labels named list of variable labels, used for facet strips
#'
#' @return list with `plot`, `height_in`, `n_panels`, `n_groups`
build_continuous_summary_plot <- function(dt,
                                          vars,
                                          by_var = NULL,
                                          labels = NULL) {

  d <- data.table::as.data.table(dt)

  vars <- intersect(vars, names(d))
  if (!length(vars))
    stop("build_continuous_summary_plot: no requested variables in data")

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

  long <- data.table::melt(
    d, id.vars = ".group", measure.vars = vars,
    variable.name = ".var", value.name = ".value", variable.factor = FALSE
  )[!is.na(.value)]

  long[, .var := factor(vapply(.var, lab, ""), levels = vapply(vars, lab, ""))]

  ncol <- if (length(vars) <= 2L) 1L else 2L
  nrow <- ceiling(length(vars) / ncol)

  p <- ggplot2::ggplot(long, ggplot2::aes(x = .group, y = .value)) +
    ggplot2::geom_boxplot(outlier.shape = NA, width = 0.55,
                          fill = "grey92", colour = "grey30") +
    ggplot2::geom_jitter(width = 0.15, height = 0, alpha = 0.25, size = 0.7,
                         colour = "grey20") +
    ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 1.8,
                          fill = "white", colour = "black") +
    ggplot2::facet_wrap(~ .var, ncol = ncol, scales = "free_y") +
    ggplot2::labs(
      x = if (!is.null(by_var)) lab(by_var) else NULL,
      y = NULL,
      caption = "Box: median and IQR; whiskers 1.5 x IQR; diamond: mean."
    )

  list(plot = p,
       height_in = min(9.5, 1.1 + nrow * 1.8),
       n_panels = length(vars),
       n_groups = n_groups)
}
