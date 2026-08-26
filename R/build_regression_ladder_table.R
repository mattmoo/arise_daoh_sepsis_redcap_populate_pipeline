#' Build a regression results table for a named set of models
#'
#' Rows are covariate sets in the order supplied; columns are the linear model
#' (mean difference) followed by each primary quantile. Cells carry the estimate
#' and its confidence interval.
#'
#' The rung set is passed explicitly rather than inferred, because the same
#' function serves three different tables that differ only in which models they
#' contain and how they are captioned:
#'
#'   main      m0, m1, m2, m3_news, m3_comp, m4 - the covariate ladder, with the
#'             decomposed severity model included so it can be read against the
#'             others. Note that m3_comp is an alternative to m3_news rather
#'             than a further step, so it sits out of sequence by design.
#'   severity  m3_news, m3_comp - a direct head-to-head of the two severity
#'             specifications, where the question is whether decomposing NEWS
#'             into its components changes the exposure effect.
#'   mediation m3_news, m5 - the mediator-adjusted comparison. m3_news is the
#'             comparator because m5 is exactly m3_news plus triage category,
#'             so the difference between the two rows is the triage adjustment
#'             and nothing else.
#'
#' Estimates from fits reporting a non-unique quantile regression solution are
#' marked with a dagger, and thin fits (few observations per parameter) with a
#' double dagger, so a flagged number cannot be quoted without the caveat
#' travelling with it.
#'
#' @param effect_dt long effect table, filtered to primary taus
#' @param exposure_var exposure to tabulate
#' @param population population slug to tabulate
#' @param covariate_sets rung ids to include, in the order they should appear
#' @param covariate_set_labels named list mapping rung ids to descriptions
#' @param digits decimal places for estimates
#' @param footnote extra sentence appended to the table footer, used to carry
#'   the caveat specific to each table role
#'
#' @return list with `data` (wide data.table) and `flextable`
build_regression_ladder_table <- function(effect_dt,
                                          exposure_var,
                                          population,
                                          covariate_sets,
                                          covariate_set_labels = NULL,
                                          digits = 1L,
                                          footnote = NULL) {

  d <- data.table::as.data.table(effect_dt)
  d <- d[exposure == exposure_var &
           population_slug == population &
           tau_role == "primary" &
           covariate_set %chin% covariate_sets]

  if (!nrow(d))
    stop("build_regression_ladder_table: no rows for ", exposure_var, " / ",
         population, " / ", paste(covariate_sets, collapse = ", "))

  fmt <- paste0("%.", digits, "f")

  d[, cell := data.table::fifelse(
    estimable & !is.na(estimate),
    paste0(sprintf(fmt, estimate),
           data.table::fifelse(
             is.na(conf_low), "",
             paste0(" [", sprintf(fmt, conf_low), ", ",
                    sprintf(fmt, conf_high), "]")),
           data.table::fifelse(nonunique_solution, " \u2020", ""),
           data.table::fifelse(thin %in% TRUE, " \u2021", "")),
    "not estimable")]

  # Linear model first, then quantiles ascending
  d[, col := data.table::fifelse(
    model_type == "lm", "Mean difference",
    paste0("tau = ", format(tau, nsmall = 2)))]
  col_order <- c("Mean difference", sort(unique(d[model_type == "rq", col])))

  # Row order follows the supplied vector, not the data or the alphabet
  rung_levels <- intersect(covariate_sets, unique(d$covariate_set))
  d[, model_label := factor(
    label_or_self(as.character(covariate_set), covariate_set_labels),
    levels = label_or_self(rung_levels, covariate_set_labels))]

  has_contrasts <- data.table::uniqueN(d$contrast) > 1L
  lhs <- if (has_contrasts) "contrast + model_label" else "model_label"

  wide <- data.table::dcast(d, stats::as.formula(paste(lhs, "~ col")),
                            value.var = "cell")
  data.table::setcolorder(
    wide, c(setdiff(names(wide), col_order), intersect(col_order, names(wide))))

  n_used <- sort(unique(d[estimable == TRUE, n]))
  n_text <- if (!length(n_used)) "not estimable"
  else if (length(n_used) == 1L) as.character(n_used)
  else paste(range(n_used), collapse = " to ")

  footer <- paste0(
    "Values are the difference in days alive and out of hospital to 90 days, ",
    "with 95% confidence intervals. Estimates are average marginal contrasts ",
    "standardised over the observed covariate distribution. ",
    "\u2020 quantile regression solution was not unique. ",
    "\u2021 fewer than ten observations per parameter; interpret with caution. ",
    "n = ", paste(range(n_text), collapse = " to "), ".")
  if (!is.null(footnote)) footer <- paste(footer, footnote)

  ft <- flextable::flextable(wide) |>
    flextable::set_header_labels(model_label = "Model",
                                 contrast = "Comparison") |>
    flextable::add_footer_lines(footer)

  if (has_contrasts) ft <- flextable::merge_v(ft, j = "contrast")

  ft <- ft |>
    flextable::align(j = seq(2, ncol(wide)), align = "right", part = "all") |>
    flextable::bold(part = "header") |>
    flextable::autofit()

  list(data = wide[], flextable = ft,
       exposure = exposure_var, population_slug = population,
       covariate_sets = rung_levels)
}
