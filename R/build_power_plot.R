#' Plot power against total sample size
#'
#' One curve per difference in mean DAOH90, in a single panel. The curve is
#' power at the point estimates of the inputs; the ribbon is the bootstrap
#' percentile interval, obtained by resampling patients and recomputing the
#' standard deviations and the Māori proportion from each resample. Unlike a
#' range taken across the corners of marginal intervals, that is an interval
#' with a defined coverage, and it carries any dependence between the three
#' parameters without having to model it.
#'
#' The proportion assumed in the protocol is drawn as a separate dashed curve
#' where supplied. It is a different assumption rather than a bound on this
#' cohort's estimate, so it does not belong inside the interval.
#'
#' Reference lines mark 80% and 90% power, and a vertical marks the trial size
#' currently anticipated, so a reader can drop a line from either and read off
#' the answer.
#'
#' Returns geometry only. Scales, theme, axis titles and the caption are applied
#' at the output stage; this function sets no labels other than the title, which
#' identifies the branch. Both a colour and a fill scale are needed at output,
#' sharing the same `name`, or the curve and the ribbon produce two legends.
#'
#' @param power_dt output of build_power_boot_dt()
#' @param protocol_dt optional second output computed at the protocol's assumed
#'   proportion, drawn dashed for comparison
#' @param power_marks horizontal reference lines
#' @param n_marks vertical reference line, e.g. the planned trial size
#' @param show_interval draw the bootstrap interval ribbon
#' @param show_mean draw the bootstrap mean, i.e. power expected once parameter
#'   uncertainty is allowed for; off by default since it sits close to the point
#'   estimate and a third line per colour crowds the panel
#' @param title branch-identifying title
#'
#' @return list with `plot`, `height_in`, `has_interval`, `has_protocol`
build_power_plot <- function(power_dt,
                             protocol_dt = NULL,
                             power_marks = c(0.8, 0.9),
                             n_marks = 300,
                             show_interval = TRUE,
                             show_mean = FALSE,
                             title = NULL) {
  
  d <- data.table::as.data.table(power_dt)[!is.na(power)]
  if (!nrow(d)) stop("build_power_plot: no estimable rows")
  
  lab_levels <- paste0(sort(unique(d$delta)), " days")
  d[, delta_lab := factor(paste0(delta, " days"), levels = lab_levels)]
  
  p <- ggplot2::ggplot(d, ggplot2::aes(x = n_total, y = power,
                                       colour = delta_lab, fill = delta_lab)) +
    ggplot2::geom_hline(yintercept = power_marks, colour = "grey60",
                        linetype = "dashed", linewidth = 0.3)
  
  if (!is.null(n_marks))
    p <- p + ggplot2::geom_vline(xintercept = n_marks, colour = "grey60",
                                 linetype = "dotted", linewidth = 0.3)
  
  has_interval <- isTRUE(show_interval) &&
    all(c("power_lower", "power_upper") %chin% names(d)) &&
    d[, any(!is.na(power_lower))]
  
  if (has_interval)
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = power_lower, ymax = power_upper),
      colour = NA, alpha = 0.15)
  
  has_protocol <- !is.null(protocol_dt) && nrow(protocol_dt) > 0L
  if (has_protocol) {
    pd <- data.table::as.data.table(protocol_dt)[!is.na(power)]
    pd[, delta_lab := factor(paste0(delta, " days"), levels = lab_levels)]
    p <- p + ggplot2::geom_line(
      data = pd,
      ggplot2::aes(x = n_total, y = power, colour = delta_lab),
      inherit.aes = FALSE, linetype = "22", linewidth = 0.4)
  }
  
  if (isTRUE(show_mean) && "power_mean" %chin% names(d))
    p <- p + ggplot2::geom_line(
      ggplot2::aes(y = power_mean), linetype = "dotted", linewidth = 0.4)
  
  p <- p + ggplot2::geom_line(linewidth = 0.7)
  
  if (!is.null(title)) p <- p + ggplot2::ggtitle(title)
  
  list(plot = p,
       height_in = 5.2,
       has_interval = has_interval,
       has_protocol = has_protocol,
       has_mean = isTRUE(show_mean))
}