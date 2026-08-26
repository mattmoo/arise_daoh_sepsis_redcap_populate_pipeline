#' Build a covariance matrix for a fitted regression model
#'
#' Computed as its own target so the cost is visible, cached against the fit,
#' and parallelisable. Downstream consumers reuse the result rather than each
#' recomputing it, and changing the resample count invalidates only these
#' targets rather than every model.
#'
#' For quantile regression the default analytic covariance depends on an
#' estimate of the sparsity function, the local density of the response at the
#' fitted quantile. With a heavily tied, zero-inflated outcome that estimate
#' frequently fails, reported by quantreg as "non-positive fis". The xy-pair
#' bootstrap does not use the sparsity estimate at all, so it sidesteps the
#' problem rather than working around it, and is also robust to
#' heteroscedasticity.
#'
#' Note that a delta-method interval built on this matrix is still a normal
#' approximation. For quantile contrasts on a bounded outcome at modest sample
#' size the sampling distribution is not symmetric, so these are suitable for
#' showing the shape of an effect across tau, not as the reported inference.
#' Primary estimates use a full bootstrap of the contrast instead.
#'
#' Linear models return the HC3 sandwich estimator, which needs no resampling
#' and handles the heteroscedasticity that a zero-inflated outcome guarantees.
#'
#' Failures are soft, matching `fit_regression_model()`, so an unusable
#' covariance leaves a gap rather than killing the branch.
#'
#' @param model_list one element of the fitted model list
#' @param R bootstrap resamples for quantile regression; a covariance matrix
#'   needs fewer than an interval would, but several hundred is a sensible
#'   floor for a model with a dozen or more parameters
#' @param bsmethod resampling scheme passed to quantreg::boot.rq via
#'   summary.rq; "xy" pairs is the safe default under heteroscedasticity
#'
#' @return list with `vcov` (matrix or NULL), `method`, `available`, `reason`
#'   and the spec_id it belongs to
build_regression_vcov <- function(model_list,
                                  R = 500L,
                                  bsmethod = "xy") {

  out <- list(
    spec_id   = model_list$spec_id,
    method    = NA_character_,
    R         = NA_integer_,
    available = FALSE,
    reason    = NA_character_,
    vcov      = NULL
  )

  if (!isTRUE(model_list$estimable)) {
    out$reason <- "model not estimable"
    return(out)
  }

  fit <- model_list$fit

  res <- tryCatch({

    if (model_list$model_type == "lm") {

      out$method <- "HC3"
      sandwich::vcovHC(fit, type = "HC3")

    } else if (model_list$model_type == "rq") {

      out$method <- paste0("bootstrap (", bsmethod, ")")
      out$R <- as.integer(R)
      # summary.rq returns the bootstrap covariance in $cov when se = "boot"
      s <- summary(fit, se = "boot", R = R, bsmethod = bsmethod,
                   covariance = TRUE)
      s$cov

    } else {
      stop("Unknown model_type: ", model_list$model_type)
    }
  }, error = function(e) e)

  if (inherits(res, "error")) {
    out$reason <- conditionMessage(res)
    return(out)
  }

  if (is.null(res) || !is.matrix(res)) {
    out$reason <- "covariance not returned as a matrix"
    return(out)
  }

  # A covariance matrix with non-positive diagonal entries is unusable and
  # would produce NaN standard errors downstream; catch it here rather than
  # letting it propagate into an interval that looks computed.
  if (any(!is.finite(diag(res))) || any(diag(res) <= 0)) {
    out$reason <- "non-finite or non-positive variances on the diagonal"
    return(out)
  }

  # Coefficient names must match for marginaleffects to align the matrix with
  # the model; quantreg occasionally returns an unnamed matrix.
  cf <- stats::coef(fit)
  if (is.null(dimnames(res)) && nrow(res) == length(cf))
    dimnames(res) <- list(names(cf), names(cf))

  out$available <- TRUE
  out$vcov <- res
  out
}
