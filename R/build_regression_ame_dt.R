#' Average marginal effects for every term in a fitted model
#'
#' Companion to `build_regression_coef_dt()`, which extracts raw coefficients.
#' The two agree for a continuous covariate entering linearly with no
#' interactions, and diverge everywhere else. Two divergences matter here.
#'
#' M3 enters as a restricted cubic spline, so it has no single coefficient: the
#' two basis functions are uninterpretable individually and neither is "the
#' effect of M3". `avg_slopes()` returns one number, the average marginal effect
#' over the observed distribution of M3, which is the quantity a reader wants.
#'
#' Ethnicity is expressed relative to the population average rather than to a
#' reference level, matching the exposure tables, so that no group is positioned
#' as the standard against which others are measured.
#'
#' More generally a coefficient is conditional, holding everything else fixed,
#' whereas an average marginal effect is standardised over the observed
#' covariate distribution. For a model with no interactions and an identity
#' link most terms coincide; the spline is the exception, and it is the reason
#' this function exists.
#'
#' Everything is on the scale of the fitted model: days of DAOH90 for `lm` (a
#' difference in means, or a slope per unit) and for `rq` (the same in the
#' tau-th quantile).
#'
#' Uncertainty follows the same rule as the exposure effects: the covariance
#' supplied by `build_regression_vcov()` is used for the delta method, since the
#' analytic covariance for quantile regression rests on a sparsity estimate
#' that fails across much of this grid. These are delta-method intervals rather
#' than the bootstrap intervals reported for the primary exposure contrasts, and
#' should not be quoted interchangeably with them.
#'
#' @param model_list one element of the fitted model list
#' @param vcov_list the matching element of the covariance list
#' @param population_reference variables contrasted against the population
#'   average rather than a reference level
#' @param labels named list of variable labels
#' @param conf_level interval coverage
#'
#' @return data.table of terms with average marginal effects and intervals
build_regression_ame_dt <- function(model_list,
                                    vcov_list = NULL,
                                    population_reference =
                                      "priority.ethnicity.desc.L1",
                                    labels = NULL,
                                    conf_level = 0.95) {

  if (!isTRUE(model_list$estimable)) return(data.table::data.table())

  meta <- model_list[c("spec_id", "population_slug", "covariate_set",
                       "m3_form", "model_type", "tau", "tau_role",
                       "mediator_adjusted", "n", "n_params", "epp", "thin",
                       "nonunique_solution", "nonpositive_fis")]

  V <- if (!is.null(vcov_list) && isTRUE(vcov_list$available))
    vcov_list$vcov else NULL

  # Variables in the model, recovered from the formula rather than from the
  # coefficient names, so a spline contributes one variable and not two.
  rhs <- all.vars(model_list$formula)[-1]
  mf <- stats::model.frame(model_list$fit)
  rhs <- intersect(rhs, names(mf))
  if (!length(rhs)) return(data.table::data.table())

  one_term <- function(v) {

    args <- list(model_list$fit, conf_level = conf_level)
    if (!is.null(V)) args$vcov <- V

    res <- tryCatch({
      if (v %chin% population_reference && is.factor(mf[[v]])) {

        # Deviation of each group's standardised mean from the population
        # average, weighted by group size so a stratum of a dozen patients does
        # not carry the same weight as one of a hundred and thirty-six.
        g <- factor(mf[[v]])
        w <- as.numeric(table(g)) / length(g)
        dev_from_population <- function(x) {
          e <- x$estimate
          data.frame(term = paste0(levels(g), " vs population"),
                     estimate = e - sum(w * e))
        }
        args$by <- v
        args$hypothesis <- dev_from_population
        do.call(marginaleffects::avg_predictions, args)

      } else if (is.factor(mf[[v]]) || is.logical(mf[[v]]) ||
                 is.character(mf[[v]])) {

        args$variables <- v
        do.call(marginaleffects::avg_comparisons, args)

      } else {
        # Continuous: the average slope, which collapses a spline to one row
        args$variables <- v
        do.call(marginaleffects::avg_slopes, args)
      }
    }, error = function(e) e)

    if (inherits(res, "error"))
      return(data.table::data.table(
        variable = v, contrast = NA_character_, estimate = NA_real_,
        conf_low = NA_real_, conf_high = NA_real_,
        reason = conditionMessage(res)))

    out <- data.table::as.data.table(res)
    if ("conf.low"  %chin% names(out))
      data.table::setnames(out, "conf.low", "conf_low")
    if ("conf.high" %chin% names(out))
      data.table::setnames(out, "conf.high", "conf_high")
    if (!"contrast" %chin% names(out))
      out[, contrast := if ("term" %chin% names(out)) term else NA_character_]

    out[, variable := v]
    out[, reason := NA_character_]
    out[, .(variable, contrast, estimate, conf_low, conf_high, reason)]
  }

  out <- data.table::rbindlist(lapply(rhs, one_term), fill = TRUE)
  if (!nrow(out)) return(data.table::data.table())

  # A single-contrast term needs no suffix; a multi-level one does.
  out[, n_contrast := .N, by = variable]
  out[, term_label := data.table::fifelse(
    n_contrast == 1L | is.na(contrast),
    label_or_self(variable, labels),
    paste0(label_or_self(variable, labels), ": ",
           sub(" vs population$", "", contrast)))]
  out[, n_contrast := NULL]

  out[, estimand := data.table::fifelse(
    variable %chin% population_reference,
    "deviation from population average",
    "average marginal effect")]

  for (nm in names(meta)) data.table::set(out, j = nm, value = meta[[nm]])
  out[, vcov_method := if (is.null(vcov_list)) NA_character_
                       else vcov_list$method]
  out[]
}
