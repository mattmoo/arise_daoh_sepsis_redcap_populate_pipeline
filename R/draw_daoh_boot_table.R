#' Render a bootstrap DAOH summary as a flextable
#'
#' Takes the long-format output of `boot_daoh()` returned by `groupingsets()`
#' and draws one table for a single stratum variable, with that variable's
#' levels as columns plus a Total column (the grand-total row, where every
#' `by` column is NA).
#'
#' @param boot_dt long bootstrap results; one row per statistic per stratum
#' @param stratum_var the stratum variable to draw (must appear in `by_vars`)
#' @param by_vars all stratum variables present in `boot_dt`
#' @param labels named list of variable labels
#' @param digits decimal places for estimates and interval bounds
#'
#' @return a list with `stratum_var`, `label`, `data` (the wide table) and
#'   `flextable`, so dynamic branches remain identifiable after collection
draw_daoh_boot_table <- function(boot_dt,
                                 stratum_var,
                                 by_vars,
                                 labels = NULL,
                                 digits = 1L) {
  
  stopifnot(stratum_var %in% by_vars)
  
  d <- data.table::copy(boot_dt)
  
  # Drop rows belonging to the other stratum variables, keeping this
  # variable's levels and the grand total (all by columns NA).
  other <- setdiff(by_vars, stratum_var)
  if (length(other)) {
    keep <- Reduce(`&`, lapply(other, function(v) is.na(d[[v]])))
    d <- d[keep]
  }
  
  d[, col := data.table::fifelse(is.na(get(stratum_var)), "Total",
                                 as.character(get(stratum_var)))]
  
  stat_lab <- c(mean = "Mean", sd = "SD", sem = "SEM",
                q10 = "10th centile", q25 = "25th centile", median = "Median",
                q75 = "75th centile", q90 = "90th centile")
  present <- stat_lab[names(stat_lab) %in% d$statistic]
  
  fmt  <- paste0("%.", digits, "f")
  cell <- paste0(fmt, " [", fmt, ", ", fmt, "]")
  
  d[, `:=`(
    statistic = factor(stat_lab[statistic], levels = present),
    value = data.table::fifelse(
      estimable,
      sprintf(cell, estimate, lower, upper),
      paste0(sprintf(fmt, estimate), " \u2020")
    )
  )]
  
  lv   <- setdiff(unique(d$col), "Total")
  
  if (anyDuplicated(d[, .(statistic, col)]))
    stop("draw_daoh_boot_table: non-unique statistic/column combinations for '",
         stratum_var, "'. Check that by_vars holds all stratum variables.")
  
  wide <- data.table::dcast(d, statistic ~ col, value.var = "value")
  data.table::setcolorder(wide, c("statistic", lv, "Total"))
  
  cols <- c(lv, "Total")
  ns   <- d[, .(n = n[1]), by = col]
  hdr  <- stats::setNames(
    c("", sprintf("%s\n(n = %d)", cols, ns$n[match(cols, ns$col)])),
    names(wide)
  )
  
  var_label <- if (!is.null(labels[[stratum_var]])) labels[[stratum_var]]
  else stratum_var
  
  ft <- flextable::flextable(wide) |>
    flextable::set_header_labels(values = as.list(hdr)) |>
    flextable::add_header_lines(
      sprintf("Days alive and out of hospital to 90 days, by %s",
              tolower(var_label))) |>
    flextable::add_footer_lines(paste(
      "Values are point estimate [95% bootstrap BCa confidence interval].",
      "\u2020 Interval not estimable: the statistic was invariant across",
      "bootstrap resamples."
    )) |>
    flextable::align(j = seq_along(wide)[-1], align = "right", part = "all") |>
    flextable::bold(part = "header") |>
    flextable::autofit()
  
  list(
    stratum_var = stratum_var,
    label       = var_label,
    levels      = cols,
    data        = wide[],
    flextable   = ft
  )
}