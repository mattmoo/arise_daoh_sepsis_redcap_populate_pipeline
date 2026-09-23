#' Build a side-by-side coefficient table across the covariate ladder
#'
#' One column per model in the ladder, coefficients stacked with confidence
#' intervals beneath, and a goodness-of-fit block carrying n, AIC and the
#' residual scale. Complements the marginal-effects table rather than replacing
#' it: this shows the model, that one shows the estimand.
#'
#' The two will not agree for ethnicity, and should not be expected to. The
#' marginal-effects table reports each group's standardised mean as a deviation
#' from the population average; this table reports raw coefficients against the
#' factor's reference level. Both are correct answers to different questions,
#' and the caption says so.
#'
#' Only one estimator appears per table. Putting `lm` and `rq` columns side by
#' side would invite reading across them, but a linear coefficient is a
#' difference in means and a quantile coefficient is a difference in the tau-th
#' quantile, and their fit statistics are not comparable either.
#'
#' On the goodness-of-fit rows
#' --------------------------
#' AIC is comparable down a column block only, and only where the models were
#' fitted on the same rows. The ladder takes complete cases per model, so a rung
#' adding a covariate with missing values is fitted on fewer patients and its
#' AIC is not comparable with the rungs above it. `nobs` is therefore always
#' shown, and should be checked before any AIC is interpreted.
#'
#' For quantile regression, AIC uses the quantile objective and is comparable
#' only within a single tau, never across taus.
#'
#' @param fit_list the fitted model list
#' @param population_slug population to tabulate
#' @param model_type "lm" or "rq"
#' @param tau quantile, required when model_type is "rq"
#' @param covariate_sets rung ids to include, in the order they should appear
#' @param covariate_set_labels named list mapping rung ids to column headings
#' @param labels named list of variable labels, used to rename coefficient rows
#' @param conf_level interval coverage
#' @param gof_map goodness-of-fit rows to include
#'
#' @return list with `models`, `flextable` and the spec fields
build_regression_coef_table <- function(fit_list,
                                        population_slug,
                                        model_type = "rq",
                                        tau = NULL,
                                        covariate_sets,
                                        covariate_set_labels = NULL,
                                        labels = NULL,
                                        conf_level = 0.95,
                                        gof_map = c("nobs", "aic", "bic")) {
  
  stopifnot(requireNamespace("modelsummary", quietly = TRUE))
  
  keep <- vapply(fit_list, function(m)
    isTRUE(m$estimable) &&
      identical(m$population_slug, population_slug) &&
      identical(m$model_type, model_type) &&
      (model_type != "rq" || isTRUE(all.equal(m$tau, tau))) &&
      m$covariate_set %chin% covariate_sets,
    TRUE)
  
  fits <- fit_list[keep]
  if (!length(fits))
    stop("build_regression_coef_table: no estimable fits for ",
         population_slug, " / ", model_type,
         if (!is.null(tau)) paste0(" / tau ", tau) else "")
  
  # Column order follows the supplied vector so the table reads in the same
  # sequence as the ladder figure and the marginal-effects table.
  ord <- order(match(vapply(fits, `[[`, "", "covariate_set"), covariate_sets))
  fits <- fits[ord]
  
  nm <- vapply(fits, function(m) {
    cs <- m$covariate_set
    if (!is.null(covariate_set_labels[[cs]]))
      as.character(covariate_set_labels[[cs]]) else cs
  }, "")
  
  models <- stats::setNames(lapply(fits, `[[`, "fit"), nm)
  
  # Coefficient rows are relabelled from the variable label list. See
  # label_model_terms() for why prefix matching alone is not sufficient:
  # variables that prefix one another, factor levels glued to the variable
  # name, and spline wrappers all need handling.
  all_terms <- unique(unlist(lapply(models, \(m) names(stats::coef(m)))))
  cmap <- label_model_terms(all_terms, labels)
  
  # modelsummary uses coef_map to set row ORDER as well as row names, so terms
  # absent from a given model are simply omitted from its column. Keeping the
  # order of first appearance across the ladder means a covariate added at a
  # later rung appears below those already present.
  
  n_used <- vapply(fits, `[[`, 0, "n")
  
  note <- paste0(
    "Coefficients with ", round(conf_level * 100), "% confidence intervals. ",
    if (model_type == "rq")
      paste0("Quantile regression at tau = ", format(tau, nsmall = 2),
             "; coefficients are differences in the ",
             round(tau * 100), "th percentile of DAOH90, fitted to the ",
             "dithered outcome. AIC uses the quantile objective and is ",
             "comparable only within this table, not against other taus. ")
    else "Linear regression; coefficients are differences in mean DAOH90. ",
    "Categorical coefficients are relative to the first factor level, which ",
    "differs from the marginal-effects tables where ethnicity is expressed ",
    "relative to the population average. ",
    "Models are fitted on complete cases for their own covariate set, so n ",
    "differs down the row of columns (",
    paste(range(n_used), collapse = " to "),
    "); AIC is comparable only between columns sharing the same n.")
  
  ft <- modelsummary::modelsummary(
    models,
    output = "flextable",
    statistic = "conf.int",
    conf_level = conf_level,
    coef_map = cmap,
    gof_map = gof_map,
    notes = note
  )
  
  list(models = models,
       flextable = ft,
       population_slug = population_slug,
       model_type = model_type,
       tau = tau,
       covariate_sets = covariate_sets,
       n = n_used)
}