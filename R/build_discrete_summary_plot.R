#' Build a distribution plot for discrete integer variables
#'
#' For bounded integer scores (NZDep decile, triage category, NEWS) a boxplot
#' hides the shape that matters: whether the distribution is flat, skewed, or
#' clumped at the ends. This draws the distribution directly.
#'
#' Groups are faceted into rows rather than dodged, because dodging ten deciles
#' across four ethnicity groups gives forty bars in one panel. Multiple
#' variables become columns, with free x scales since scores differ in range.
#'
#' Returns geometry and faceting only; theme and fill scale are applied at the
#' output stage. The x breaks are set here because they are derived from the
#' data's observed range rather than being a styling choice.
#'
#' @param dt data.table
#' @param vars discrete integer variables to plot
#' @param by_var grouping variable (NULL for ungrouped)
#' @param labels named list of variable labels, used for facet strips
#' @param as_proportion TRUE plots percentage within group, so groups of
#'   unequal size stay comparable; FALSE plots raw counts
#'
#' @return list with `plot`, `height_in`, `n_panels`, `n_groups`
build_discrete_summary_plot <- function(dt,
                                        vars,
                                        by_var = NULL,
                                        labels = NULL,
                                        as_proportion = TRUE) {

  d <- data.table::as.data.table(dt)

  vars <- intersect(vars, names(d))
  if (!length(vars))
    stop("build_discrete_summary_plot: no requested variables in data")

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

  # Coerce to numeric so factors storing integer-like levels (e.g. a decile
  # read in as a factor) still plot on a numeric axis with correct spacing.
  for (v in vars) {
    x <- d[[v]]
    if (is.factor(x))
      data.table::set(d, j = v, value = as.numeric(as.character(x)))
    else if (!is.numeric(x))
      data.table::set(d, j = v, value = as.numeric(x))
  }

  long <- data.table::melt(
    d, id.vars = ".group", measure.vars = vars,
    variable.name = ".var", value.name = ".value", variable.factor = FALSE
  )[!is.na(.value)]

  counts <- long[, .N, by = .(.var, .group, .value)]
  # Denominator is the group total within each variable, so a group of 36 and
  # a group of 136 are directly comparable.
  counts[, prop := N / sum(N), by = .(.var, .group)]
  counts[, y := if (as_proportion) prop else as.numeric(N)]

  # Breaks at every observed integer if the range is short enough to label,
  # otherwise let ggplot choose. Data-derived, hence set here rather than in
  # the shared scale targets.
  rng <- long[, .(lo = min(.value), hi = max(.value)), by = .var]
  brk <- if (nrow(rng) == 1L && (rng$hi - rng$lo) <= 12)
    seq(rng$lo, rng$hi, by = 1) else ggplot2::waiver()

  counts[, .var := factor(vapply(as.character(.var), lab, ""),
                          levels = vapply(vars, lab, ""))]

  p <- ggplot2::ggplot(counts, ggplot2::aes(x = .value, y = y,
                                            fill = .group)) +
    ggplot2::geom_col(width = 0.85) +
    ggplot2::facet_grid(rows = ggplot2::vars(.group),
                        cols = ggplot2::vars(.var),
                        scales = "free_x", switch = "y") +
    ggplot2::scale_x_continuous(breaks = brk) +
    ggplot2::labs(
      x = NULL,
      y = if (as_proportion) "Percentage within group" else "Patients",
      caption = if (as_proportion)
        "Bars are percentages within each group, so groups of unequal size are comparable."
        else "Bars are patient counts."
    )

  list(plot = p,
       height_in = min(9.5, 1.2 + n_groups * 1.15),
       n_panels = length(vars),
       n_groups = n_groups)
}
