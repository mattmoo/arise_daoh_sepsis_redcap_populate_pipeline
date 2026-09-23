#' Build the power table
#'
#' Rows are total sample sizes, columns are differences in mean DAOH90. Cells
#' carry power with its bootstrap percentile interval, obtained by resampling
#' patients and recomputing the standard deviations and the Māori proportion
#' from each resample.
#'
#' A final column gives the difference detectable at the target power, which is
#' usually the more useful number in a feasibility report: it answers "what
#' could this trial see" without requiring the reader to pick an effect size
#' first. It is computed at the point estimates and carries no interval.
#'
#' `value` selects what the cell reports:
#'
#'   "point"  power at the point estimates of the inputs. Matches the curve
#'            drawn in the figure.
#'   "mean"   mean power over the bootstrap distribution, i.e. the power to be
#'            expected once uncertainty in the inputs is allowed for. Lower than
#'            the point estimate wherever power is concave in the parameters,
#'            which it is over most of this range, so it is the more
#'            conservative and arguably the more honest number.
#'
#' Both are conditional on the true difference being exactly the column
#' heading; the effect size is assumed, not estimated.
#'
#' @param power_dt output of build_power_boot_dt()
#' @param n_show total sample sizes to tabulate; the full grid is for the figure
#' @param value "point" or "mean"
#' @param show_interval append the bootstrap interval to each cell
#' @param footnote extra sentence for the footer
#'
#' @return list with `data`, `flextable`, and the inputs used
build_power_table <- function(power_dt,
                              n_show = c(200, 300, 400, 500, 600, 800),
                              value = c("point", "mean"),
                              show_interval = TRUE,
                              footnote = NULL) {
  
  value <- match.arg(value)
  d <- data.table::as.data.table(power_dt)[n_total %in% n_show & !is.na(power)]
  
  if (!nrow(d))
    stop("build_power_table: no estimable rows at the requested sample sizes")
  
  est <- if (value == "mean") d$power_mean else d$power
  
  d[, cell := sprintf("%.2f", est)]
  if (isTRUE(show_interval) &&
      all(c("power_lower", "power_upper") %chin% names(d)))
    d[!is.na(power_lower),
      cell := sprintf("%.2f [%.2f, %.2f]", est[!is.na(power_lower)],
                      power_lower[!is.na(power_lower)],
                      power_upper[!is.na(power_lower)])]
  
  d[, col := paste0(delta, " days")]
  col_order <- paste0(sort(unique(d$delta)), " days")
  
  wide <- data.table::dcast(d, n_total + n_group + n_other + mde ~ col,
                            value.var = "cell")
  data.table::setcolorder(wide, c("n_total", "n_group", "n_other",
                                  col_order, "mde"))
  wide[, mde := sprintf("%.1f", mde)]
  data.table::setorder(wide, n_total)
  
  # Single values by construction: these describe the cohort, not the row.
  one <- function(x, nm) {
    u <- unique(x)
    if (length(u) != 1L)
      stop("build_power_table: expected one value of ", nm, ", got ", length(u))
    u
  }
  sd_g   <- one(d$sd_group, "sd_group")
  sd_o   <- one(d$sd_other, "sd_other")
  p_used <- one(d$p_group, "p_group")
  R_used <- one(d$R, "R")
  conf   <- one(d$conf, "conf")
  target_pct <- round(one(d$target_power, "target_power") * 100)
  
  p_note <- if (identical(unique(d$p_source), "fixed"))
    "a fixed assumed proportion" else "the proportion observed in this cohort"
  
  footer <- paste0(
    "Power for a two-sided comparison of mean DAOH90 at alpha = ",
    one(d$alpha, "alpha"), ". Cells are ",
    if (value == "mean")
      "mean power over the bootstrap distribution, that is, the power to be expected once uncertainty in the inputs is allowed for"
    else "power at the point estimates of the inputs",
    if (isTRUE(show_interval))
      paste0(", with ", round(conf * 100), "% bootstrap percentile intervals")
    else "",
    ". Intervals come from ", R_used, " resamples of the cohort, with the ",
    "standard deviations and the M\u0101ori proportion recomputed from each, so ",
    "the joint uncertainty in all three inputs is carried through. Inputs: ",
    sprintf("%.1f", sd_g), " days standard deviation for M\u0101ori, ",
    sprintf("%.1f", sd_o), " days for others, and ", round(p_used * 100),
    "% M\u0101ori (", p_note, "). ",
    "All figures are conditional on the true difference being exactly the ",
    "value in the column heading; the effect size is assumed, not estimated. ",
    "The calculation compares means and assumes approximate normality of each ",
    "group mean; DAOH90 has a large mass at zero and a ceiling at 90 days, so ",
    "power at the smaller sample sizes is likely to be slightly overstated. ",
    "Rank-based, quantile and ordinal analyses may have different power on ",
    "this outcome and are not evaluated here.")
  
  if (!is.null(footnote)) footer <- paste(footer, footnote)
  
  ft <- flextable::flextable(wide) |>
    flextable::set_header_labels(
      n_total = "Total n", n_group = "M\u0101ori", n_other = "Other",
      mde = paste0("Detectable difference at ", target_pct,
                   "% power (days)")) |>
    flextable::add_header_row(
      values = c("Sample size", "Power to detect a difference of", ""),
      colwidths = c(3, length(col_order), 1)) |>
    flextable::add_footer_lines(footer) |>
    flextable::align(j = seq(2, ncol(wide)), align = "right", part = "all") |>
    flextable::bold(part = "header") |>
    flextable::autofit()
  
  list(data = wide[], flextable = ft,
       sd_group = sd_g, sd_other = sd_o, p_group = p_used, R = R_used)
}