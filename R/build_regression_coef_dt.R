#' Extract tidy coefficients from the fitted models
#'
#' Confidence intervals are built from the covariance matrices computed by
#' `build_regression_vcov()` rather than from each model's default. For
#' quantile regression the default analytic covariance rests on a sparsity
#' estimate that failed across much of this grid, reported by quantreg as
#' "non-positive fis"; the bootstrapped covariance sidesteps that. Reusing it
#' here also means the coefficient tables and forest plots cannot disagree with
#' the marginal-effects output about what uncertainty was assumed.
#'
#' Intervals are normal approximations on the coefficient scale. That is
#' adequate for a forest plot whose job is to show pattern across a ladder of
#' models, but it is weaker than the bootstrap intervals reported for the
#' marginal contrasts, and the two should not be quoted interchangeably.
#'
#' Terms are relabelled through `label_model_terms()`, so factor levels, spline
#' bases and variables that prefix one another all come out readable.
#'
#' @param model_list one element of the fitted model list
#' @param vcov_list the matching element of the covariance list
#' @param labels named list of variable labels
#' @param conf_level interval coverage
#' @param drop_intercept omit the intercept, which is rarely of interest and
#'   whose magnitude compresses everything else on a shared axis
#'
#' @return data.table of terms with estimates and intervals, or an empty table
#'   where the model was not estimable
build_regression_coef_dt <- function(model_list,
                                     vcov_list = NULL,
                                     labels = NULL,
                                     conf_level = 0.95,
                                     drop_intercept = TRUE) {

  if (!isTRUE(model_list$estimable)) return(data.table::data.table())

  cf <- stats::coef(model_list$fit)
  if (!length(cf)) return(data.table::data.table())

  V <- if (!is.null(vcov_list) && isTRUE(vcov_list$available))
    vcov_list$vcov else tryCatch(stats::vcov(model_list$fit),
                                 error = function(e) NULL)

  se <- if (is.null(V)) rep(NA_real_, length(cf)) else {
    d <- diag(V)
    if (!is.null(names(d))) d <- d[names(cf)]
    sqrt(d)
  }

  z <- stats::qnorm(1 - (1 - conf_level) / 2)

  out <- data.table::data.table(
    term      = names(cf),
    estimate  = as.numeric(cf),
    std_error = as.numeric(se),
    conf_low  = as.numeric(cf) - z * as.numeric(se),
    conf_high = as.numeric(cf) + z * as.numeric(se)
  )

  if (isTRUE(drop_intercept))
    out <- out[!term %chin% c("(Intercept)", "Intercept")]
  if (!nrow(out)) return(data.table::data.table())

  out[, term_label := label_model_terms(term, labels)]

  # Spec metadata, so a branch stays self-describing after collection
  for (nm in c("spec_id", "population_slug", "covariate_set", "m3_form",
               "model_type", "tau", "tau_role", "mediator_adjusted",
               "n", "n_params", "epp", "thin",
               "nonunique_solution", "nonpositive_fis"))
    data.table::set(out, j = nm, value = model_list[[nm]])

  out[, vcov_method := if (is.null(vcov_list)) NA_character_
                       else vcov_list$method]
  out[]
}
