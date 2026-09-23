#' Covariate blocks
#'
#' Single definition of which variables belong to which block, used both to
#' assemble the model formulae and to group terms in the forest plots. Defined
#' once so a variable cannot be in one block for fitting and another for
#' display, and so adding a covariate is a single edit.
#'
#' `build_regression_spec_dt()` should take its blocks from here rather than
#' holding its own copy.
regression_blocks <- function() {
  list(
    demography    = c("age_years", "gender"),
    socio_comorb  = c("nzdep2023_int", "m3_score"),
    severity_news = "news",
    severity_comp = c("first_sbp", "first_heart_rate", "first_resp_rate",
                      "first_temperature", "first_spo2", "first_avpu"),
    perfusion     = "first_lactate",
    care_process  = "triage_category"
  )
}


#' Display labels for the covariate blocks
#'
#' Ordered as they should appear down a figure: exposures first, then the
#' blocks in the order the ladder adds them.
regression_block_labels <- function() {
  list(
    exposure      = "Exposures",
    demography    = "Demography",
    socio_comorb  = "Deprivation and comorbidity",
    severity_news = "Severity (NEWS)",
    severity_comp = "Severity (vital signs)",
    perfusion     = "Perfusion",
    care_process  = "Care process",
    other         = "Other"
  )
}


#' Map model variables to their display block
#'
#' Returns a named character vector from variable name to block label, suitable
#' for grouping rows in a forest plot. Grouping the six vital sign components
#' together is the main gain: they enter as one block, they are alternatives to
#' a single NEWS term, and reading them as six unrelated covariates misses that.
#'
#' @param exposures exposure variables, which take their own block at the top
#' @param blocks block definitions; defaults to `regression_blocks()`
#' @param block_labels display labels; defaults to `regression_block_labels()`
#'
#' @return named character vector, variable -> block label
regression_term_groups <- function(exposures = character(0),
                                   blocks = regression_blocks(),
                                   block_labels = regression_block_labels()) {

  lab <- function(b) {
    l <- block_labels[[b]]
    if (is.null(l)) b else as.character(l)
  }

  out <- unlist(lapply(names(blocks), function(b)
    stats::setNames(rep(lab(b), length(blocks[[b]])), blocks[[b]])))

  if (length(exposures))
    out <- c(stats::setNames(rep(lab("exposure"), length(exposures)),
                             exposures), out)

  # An exposure that also appears in a block keeps the exposure label, since
  # that is where a reader will look for it.
  out[!duplicated(names(out))]
}
