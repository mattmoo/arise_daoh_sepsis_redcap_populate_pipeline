#' Table of average marginal effects across the covariate ladder
#'
#' One row per term, one column per rung, cells "estimate (lower, upper)".
#' Companion to the coefficient tables: same layout, but on the marginal
#' effect scale, so the M3 spline is a single slope and ethnicity is relative
#' to the population average.
build_regression_ame_table <- function(ame_dt, population_slug, model_type, tau,
                                       covariate_sets, covariate_set_labels = NULL,
                                       digits = 1) {
  
  pop <- population_slug; mt <- model_type; tt <- tau
  d <- data.table::as.data.table(ame_dt)[
    population_slug == pop & model_type == mt & covariate_set %chin% covariate_sets]
  if (mt == "rq") d <- d[abs(tau - tt) < 1e-8]
  if (!nrow(d)) stop("build_regression_ame_table: no rows for ",
                     population_slug, " / ", model_type)
  
  fmt <- function(x) formatC(x, format = "f", digits = digits)
  d[, cell := data.table::fifelse(
    is.na(estimate), "",
    paste0(fmt(estimate), " (", fmt(conf_low), ", ", fmt(conf_high), ")"))]
  
  # Rows in order of first appearance up the ladder
  d[, covariate_set := factor(covariate_set, levels = covariate_sets)]
  data.table::setorder(d, covariate_set)
  term_order <- unique(d$term_label)
  
  w <- data.table::dcast(d, term_label ~ covariate_set, value.var = "cell", fill = "")
  w <- w[match(term_order, term_label)]
  rungs <- intersect(covariate_sets, names(w))
  data.table::setcolorder(w, c("term_label", rungs))
  
  hdr <- c(term_label = "",
           stats::setNames(vapply(rungs, \(r) if (!is.null(covariate_set_labels[[r]]))
             as.character(covariate_set_labels[[r]]) else r, ""), rungs))
  
  ft <- flextable::flextable(w) |>
    flextable::set_header_labels(values = as.list(hdr)) |>
    flextable::align(j = rungs, align = "center", part = "all") |>
    flextable::add_footer_lines(paste(
      "Average marginal effects in days of DAOH90, with 95% delta-method intervals",
      if (model_type == "rq") "from a bootstrapped covariance matrix."
      else "from HC3 standard errors.",
      "Continuous covariates are average slopes per unit; the M3 spline is",
      "collapsed to a single average slope. Ethnicity is each group's",
      "standardised mean relative to the population average."))
  
  list(flextable = ft)
}