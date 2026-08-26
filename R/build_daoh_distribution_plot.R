#' Build a DAOH distribution histogram stratified by group
#'
#' One bar per day (binwidth 1), stacked by 90-day vital status with deaths at
#' the base of each bar, so the mortality contribution to the zero spike is
#' visible rather than inferred. Quantile lines are drawn per facet with a
#' white underlay so they stay legible over dark bars.
#'
#' Returns geometry and faceting only. Scales, theme, axis titles, legend titles
#' and the caption are applied at the output stage; this function sets no
#' labels, including the y transform. Note that a non-linear y transform
#' breaks the proportionality of stacked segments: the drawn height of the
#' death segment is then not its share of the bar. The caption is written from
#' the caption function at output, so the figure states which axis it was given.
#'
#' @param dt data.table containing `daoh` and the mortality flag
#' @param by_var grouping variable for facet rows (NULL for a single panel)
#' @param labels named list of variable labels
#' @param mortality_var logical or two-level variable marking death by 90 days
#' @param quantile_probs quantiles to mark; NULL for none
#' @param title branch-identifying title, e.g. the population being plotted.
#'   Titles that identify which branch produced a figure are built here because
#'   this is where the branch is known; all other labels are applied at output.
#'
#' @return list with `plot`, `height_in`, `n_groups`, `quantiles_dt`
build_daoh_distribution_plot <- function(dt,
                                         by_var = NULL,
                                         labels = NULL,
                                         mortality_var = "mort.90.day",
                                         quantile_probs = c(0.25, 0.5, 0.75),
                                         title = NULL) {

  d <- data.table::as.data.table(dt)

  if (!"daoh" %chin% names(d)) stop("build_daoh_distribution_plot: no `daoh`")
  if (!mortality_var %chin% names(d))
    stop("build_daoh_distribution_plot: no `", mortality_var, "`")

  d <- d[, c("daoh", mortality_var, by_var), with = FALSE][!is.na(daoh)]

  if (!is.null(by_var)) {
    d <- d[!is.na(get(by_var))]
    data.table::setnames(d, by_var, ".group")
    d[, .group := factor(.group)]
  } else {
    d[, .group := factor("All")]
  }

  # Levels ordered alive-then-died because position_stack draws the first
  # level on top; this puts deaths at the base of each bar.
  d[, .status := factor(
    data.table::fifelse(as.logical(get(mortality_var)),
                        "Died within 90 days", "Alive at 90 days"),
    levels = c("Alive at 90 days", "Died within 90 days"))]

  n_groups <- data.table::uniqueN(d$.group)

  # type = 8 to match daoh_stats(), so the lines agree with the bootstrap table
  q_dt <- if (length(quantile_probs)) {
    d[, .(quantile = factor(paste0(quantile_probs * 100, "th"),
                            levels = paste0(sort(quantile_probs) * 100, "th")),
          value = stats::quantile(daoh, quantile_probs, type = 8,
                                  names = FALSE)),
      by = .group]
  } else NULL

  p <- ggplot2::ggplot(d, ggplot2::aes(x = daoh)) +
    ggplot2::geom_histogram(
      ggplot2::aes(fill = .status),
      binwidth = 1, alpha = 0.9, colour = "grey20", linewidth = 0.15)

  if (!is.null(q_dt)) {
    p <- p +
      # White underlay so the quantile lines read over the black death segment
      ggplot2::geom_vline(
        data = q_dt,
        ggplot2::aes(xintercept = value, group = quantile),
        colour = "white", alpha = 0.85, linewidth = 0.8,
        show.legend = FALSE) +
      ggplot2::geom_vline(
        data = q_dt,
        ggplot2::aes(xintercept = value, linetype = quantile, group = quantile),
        colour = "grey15", linewidth = 0.4)
  }

  p <- p +
    ggplot2::facet_grid(rows = ggplot2::vars(.group), scales = "free_y",
                        switch = "y")

  if (!is.null(title)) p <- p + ggplot2::ggtitle(title)

  list(plot = p,
       height_in = min(9.5, 1.4 + n_groups * 1.5),
       n_groups = n_groups,
       quantiles_dt = q_dt)
}
