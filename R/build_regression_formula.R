#' Assemble the model formula for one regression spec
#'
#' Exposures are placed first so they are identifiable in coefficient output
#' regardless of how many covariates follow.
#'
#' M3 enters either linearly or as a restricted cubic spline with three knots,
#' which is the form Stanley and Sarfati recommend for the index. At n = 272
#' with most scores at or near zero the spline may be over-flexible, which is
#' why both forms are fitted and compared rather than one being assumed.
#'
#' @param spec one row of build_regression_spec_dt()
#'
#' @return a formula
build_regression_formula <- function(spec) {

  stopifnot(nrow(spec) == 1L)

  covs <- unlist(spec$covariates, use.names = FALSE)
  exps <- unlist(spec$exposures, use.names = FALSE)

  if (spec$m3_form == "spline" && "m3_score" %chin% covs)
    covs[covs == "m3_score"] <- "rms::rcs(m3_score, 3)"

  terms <- c(exps, covs)
  if (!length(terms)) stop("build_regression_formula: no terms for ",
                           spec$spec_id)

  stats::as.formula(paste(spec$outcome, "~", paste(terms, collapse = " + ")))
}
