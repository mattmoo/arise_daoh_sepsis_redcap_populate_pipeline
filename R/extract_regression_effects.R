#' Extract exposure effects from a fitted model
#'
#' Two estimands are supported.
#'
#' `reference = "level"` gives average marginal contrasts against the first
#' factor level, via `avg_comparisons()`. Appropriate for a binary exposure
#' where one level is naturally the comparator.
#'
#' `reference = "population"` gives each group's standardised mean outcome as a
#' deviation from the population average, via `avg_predictions(by = exposure)`
#' with a custom hypothesis. Used for ethnicity so that no single group is
#' positioned as the standard against which others are measured. The population
#' average is weighted by group size; an unweighted mean of group means would
#' give a stratum of a dozen patients the same influence as one of a hundred and
#' thirty-six, reintroducing the equal-weighting problem that standardisation
#' over the observed covariate distribution is chosen to avoid. Contrasts
#' against this reference are correlated, since they share a comparator computed
#' from all groups.
#'
#' Both estimands are on the scale of the fitted model: days of DAOH for `lm`
#' (difference in means) and for `rq` (difference in the tau-th quantile). A
#' quantile contrast is a difference between group quantiles, not the effect on
#' patients sitting at that quantile.
#'
#' Inference follows tau_role. Primary specs, which appear in tables and text,
#' use a full bootstrap of the contrast. Grid specs, drawn only to show the
#' shape of the effect across tau, use the delta method with the covariance
#' matrix supplied in `vcov_list`. That matrix is bootstrapped for quantile
#' regression precisely because the analytic version depends on a sparsity
#' estimate that fails frequently with a zero-inflated outcome; passing it in
#' rather than letting marginaleffects derive one is what prevents the "unable
#' to extract a variance-covariance matrix" failure and the silent loss of
#' intervals that follows.
#'
#' Unfittable specs, and fittable ones with no usable covariance, return a
#' single NA row carrying the spec metadata and the reason, so a tau grid plots
#' with gaps rather than silently omitting points a reader would assume were
#' never attempted.
#'
#' @param model_list one element of the fitted model list
#' @param vcov_list matching element of the covariance list from
#'   `build_regression_vcov()`; NULL falls back to the model's own covariance
#' @param exposure exposure variable
#' @param reference "level" or "population"
#' @param boot_R bootstrap resamples for primary specs; NULL disables
#' @param conf_level interval coverage
#'
#' @return data.table of contrasts with spec metadata attached
extract_regression_effects <- function(model_list,
                                       vcov_list = NULL,
                                       exposure,
                                       reference = c("level", "population"),
                                       boot_R = 1000L,
                                       boot_method = "rsample",
                                       conf_level = 0.95) {

  reference <- match.arg(reference)

  if (!exposure %chin% model_list$exposures)
    return(data.table::data.table())

  # Guard against a misaligned branch: the covariance must belong to this fit
  if (!is.null(vcov_list) && !identical(vcov_list$spec_id, model_list$spec_id))
    stop("extract_regression_effects: vcov spec_id '", vcov_list$spec_id,
         "' does not match model spec_id '", model_list$spec_id, "'")

  meta <- list(
    spec_id            = model_list$spec_id,
    population_slug    = model_list$population_slug,
    covariate_set      = model_list$covariate_set,
    m3_form            = model_list$m3_form,
    model_type         = model_list$model_type,
    tau                = model_list$tau,
    tau_role           = model_list$tau_role,
    mediator_adjusted  = model_list$mediator_adjusted,
    exposure           = exposure,
    reference          = reference,
    n                  = model_list$n,
    n_params           = model_list$n_params,
    epp                = model_list$epp,
    thin               = model_list$thin,
    nonunique_solution = isTRUE(model_list$nonunique_solution),
    nonpositive_fis    = isTRUE(model_list$nonpositive_fis),
    dither_method      = model_list$dither_method,
    vcov_method        = if (is.null(vcov_list)) NA_character_
                         else vcov_list$method,
    estimable          = model_list$estimable,
    reason             = model_list$reason
  )

  na_row <- function(m) data.table::as.data.table(c(
    list(contrast = NA_character_, estimate = NA_real_,
         conf_low = NA_real_, conf_high = NA_real_,
         inference = NA_character_), m))

  if (!isTRUE(model_list$estimable)) return(na_row(meta))

  use_boot <- !is.null(boot_R) && identical(model_list$tau_role, "primary")

  # Delta-method intervals need a usable covariance. Without one, report the
  # point estimate with no interval rather than letting marginaleffects fall
  # back to something undocumented.
  V <- if (!is.null(vcov_list) && isTRUE(vcov_list$available))
    vcov_list$vcov else NULL

  if (!use_boot && is.null(V)) {
    meta$reason <- if (is.null(vcov_list)) "no covariance supplied"
                   else paste("covariance unavailable:", vcov_list$reason)
  }

  res <- tryCatch({

    args <- list(model_list$fit, conf_level = conf_level)
    # Bootstrap replaces the covariance entirely, so asking for one only
    # produces a failed extraction attempt on every refit
    args$vcov <- if (use_boot) FALSE else V

    if (reference == "level") {

      args$variables <- exposure
      x <- do.call(marginaleffects::avg_comparisons, args)

    } else {

      # Group sizes from the analysed sample, so the weights match the complete
      # cases actually fitted rather than the whole population.
      mf <- stats::model.frame(model_list$fit)
      g  <- factor(mf[[exposure]])
      w  <- as.numeric(table(g)) / length(g)

      # marginaleffects passes the estimate table to the hypothesis function
      # and propagates uncertainty through it, so the correlation between each
      # group mean and the shared population average is handled.
      dev_from_population <- function(x) {
        e <- x$estimate
        data.frame(term = paste0(levels(g), " vs population"),
                   estimate = e - sum(w * e))
      }

      args$by <- exposure
      args$hypothesis <- dev_from_population
      x <- do.call(marginaleffects::avg_predictions, args)
    }

    if (use_boot)
      x <- marginaleffects::inferences(x, method = boot_method, R = boot_R)
    x
  }, error = function(e) e)

  if (inherits(res, "error")) {
    meta$estimable <- FALSE
    meta$reason <- paste("effect extraction:", conditionMessage(res))
    return(na_row(meta))
  }

  out <- data.table::as.data.table(res)

  # Column names have shifted across marginaleffects versions
  if ("conf.low"  %chin% names(out)) data.table::setnames(out, "conf.low",  "conf_low")
  if ("conf.high" %chin% names(out)) data.table::setnames(out, "conf.high", "conf_high")
  # avg_predictions with a hypothesis returns `term`, avg_comparisons `contrast`
  if (!"contrast" %chin% names(out) && "term" %chin% names(out))
    out[, contrast := term]

  out[, inference := data.table::fcase(
    use_boot, "bootstrap",
    !is.null(V), paste0("delta (", meta$vcov_method, ")"),
    default = "none")]

  # No covariance means no defensible interval
  if (!use_boot && is.null(V))
    out[, `:=`(conf_low = NA_real_, conf_high = NA_real_)]

  for (nm in names(meta)) data.table::set(out, j = nm, value = meta[[nm]])

  out[]
}
