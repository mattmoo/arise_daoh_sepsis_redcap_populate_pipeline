#' Build the model inventory table
#'
#' Not intended for publication. This is the table to look at before reading any
#' estimate, and the one to send a co-author or reviewer who asks how the models
#' behaved.
#'
#' Observations per parameter is the column that matters most: below about ten,
#' a model can be rank-sufficient and still far too thin to interpret. The
#' complete-case count matters second, because the covariate ladder shrinks the
#' analysed sample as it is climbed, so coefficients across rungs are not
#' strictly comparable if the drop is material.
#'
#' Only primary specs are listed by default. The tau grid adds hundreds of rows
#' that say the same thing, and the non-estimable ones are summarised by reason
#' instead.
#'
#' @param inventory_dt inventory table from the fitted model list
#' @param covariate_set_labels named list mapping rung ids to descriptions
#' @param population_labels named list mapping population slugs to names
#' @param primary_only restrict to specs appearing in tables and text
#'
#' @return list with `data`, `flextable` and `failure_summary`
build_regression_inventory_table <- function(inventory_dt,
                                             covariate_set_labels = NULL,
                                             population_labels = NULL,
                                             primary_only = TRUE) {

  d <- data.table::as.data.table(inventory_dt)

  # Failures across the whole grid, summarised before any filtering, so the
  # count of non-estimable quantiles is not lost by restricting to primaries.
  failure_summary <- d[estimable == FALSE, .N,
                       by = .(population_slug, reason)][order(population_slug, -N)]

  if (isTRUE(primary_only)) d <- d[tau_role == "primary"]

  d[, `:=`(
    population = label_or_self(population_slug, population_labels),
    model      = label_or_self(covariate_set, covariate_set_labels),
    fit        = data.table::fifelse(
      model_type == "lm", "Linear",
      paste0("Quantile, tau = ", format(tau, nsmall = 2))),
    flags = trimws(paste(
      data.table::fifelse(isTRUE(nonunique_solution), "non-unique", ""),
      data.table::fifelse(isTRUE(nonpositive_fis), "nonpositive fis", ""),
      data.table::fifelse(isTRUE(thin), "thin", "")))
  )]

  out <- d[, .(population, model, fit, n, n_dropped, n_params,
               epp = round(epp, 1), flags,
               status = data.table::fifelse(estimable, "fitted", reason))]

  ft <- flextable::flextable(out) |>
    flextable::set_header_labels(
      population = "Population", model = "Model", fit = "Estimator",
      n = "n analysed", n_dropped = "n dropped", n_params = "Parameters",
      epp = "Obs. per parameter", flags = "Flags", status = "Status") |>
    flextable::add_footer_lines(paste(
      "n dropped are complete-case exclusions for that model's covariate set.",
      "Fewer than ten observations per parameter is flagged as thin.",
      "A non-unique solution means an interval of coefficient vectors fits the",
      "quantile regression objective equally well. Nonpositive fis indicates",
      "unreliable local density estimation, which affects analytic standard",
      "errors but not the point estimate.")) |>
    flextable::merge_v(j = c("population", "model")) |>
    flextable::bold(part = "header") |>
    flextable::autofit()

  list(data = out[], flextable = ft, failure_summary = failure_summary[])
}
