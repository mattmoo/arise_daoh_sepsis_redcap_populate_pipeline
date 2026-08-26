#' Fit one regression spec
#'
#' Failures are soft: an unfittable spec returns `fit = NULL` with
#' `estimable = FALSE` and a `reason`, rather than stopping. This is deliberate
#' because the tau grid includes quantiles that cannot be estimated in every
#' population, and a hard failure there would kill the whole branch instead of
#' leaving a gap in the plotted curve.
#'
#' Two quantile-regression warnings are treated as flags rather than failures,
#' because both are expected with a heavily tied outcome and neither invalidates
#' the point estimate:
#'
#'   nonunique_solution  The rq objective has a flat region, so an interval of
#'                       solutions fits equally well and the reported one is a
#'                       vertex of that interval. Common with ties; the estimate
#'                       is still a valid tau-th regression quantile, but it is
#'                       not the only one.
#'   nonpositive_fis     Estimated sparsity (the density of the response at the
#'                       fitted quantile) came out non-positive for some
#'                       observations, so the local density estimate is
#'                       unreliable there. Affects standard errors and any
#'                       delta-method interval, not the coefficient itself.
#'                       Bootstrap inference sidesteps it.
#'
#' Both are surfaced so that a curve drawn from flagged fits can be marked in
#' the figure rather than silently presented as clean.
#'
#' Dither options, following Machado & Santos Silva (2005):
#'
#'   "column"  use a pre-jittered column (daoh_jittered), fixed across all
#'             specs, so every model sees the same perturbation
#'   "rq"      apply quantreg::dither() at fit time
#'   "none"    fit the raw outcome and accept the tied objective
#'
#' Dithering is random. A single dither gives one draw from a distribution of
#' estimates; the recommended practice is to average over several. `dither_reps`
#' does this, refitting with independent dithers and averaging the coefficients.
#' Note that the returned `fit` is then the last replicate, so downstream
#' marginal effects use that one; the averaged coefficients are returned
#' separately for comparison.
#'
#' @param spec one row of build_regression_spec_dt()
#' @param dt data for this spec's population
#' @param min_n minimum usable sample after complete cases
#' @param min_epp minimum observations per parameter; below this the fit is
#'   returned but flagged thin
#' @param dither_method "column", "rq" or "none"
#' @param dither_value width passed to quantreg::dither() when method is "rq"
#' @param dither_reps independent dithers to average over; 1 for a single draw
#'
#' @return list with fit (or NULL), spec fields, diagnostics and flags
fit_regression_model <- function(spec,
                                 dt,
                                 min_n = 20L,
                                 min_epp = 10,
                                 dither_method = c("column", "rq", "none"),
                                 dither_value = 1,
                                 dither_reps = 1L) {
  
  stopifnot(nrow(spec) == 1L)
  dither_method <- match.arg(dither_method)
  
  out <- list(
    spec_id            = spec$spec_id,
    population_slug    = spec$population_slug,
    covariate_set      = spec$covariate_set,
    m3_form            = spec$m3_form,
    model_type         = spec$model_type,
    tau                = spec$tau,
    tau_role           = spec$tau_role,
    mediator_adjusted  = spec$mediator_adjusted,
    severity_form      = spec$severity_form,
    exposures          = unlist(spec$exposures, use.names = FALSE),
    formula            = NULL,
    dither_method      = if (spec$model_type == "rq") dither_method else NA_character_,
    dither_reps        = if (spec$model_type == "rq") dither_reps else NA_integer_,
    n                  = NA_integer_,
    n_dropped          = NA_integer_,
    n_params           = NA_integer_,
    epp                = NA_real_,
    thin               = NA,
    nonunique_solution = FALSE,
    nonpositive_fis    = FALSE,
    other_warnings     = character(0),
    coef_averaged      = NULL,
    estimable          = FALSE,
    reason             = NA_character_,
    fit                = NULL
  )
  
  # ---- outcome, adjusted for the chosen dither -----------------------------
  # The spec names daoh_jittered for rq; override it when a different dither
  # method is requested so the spec table does not have to know about this.
  outcome <- spec$outcome
  if (spec$model_type == "rq" && dither_method != "column")
    outcome <- sub("_jittered$", "", outcome)
  
  spec_local <- data.table::copy(spec)
  spec_local[, outcome := ..outcome]
  
  f <- tryCatch(build_regression_formula(spec_local), error = function(e) NULL)
  if (is.null(f)) {
    out$reason <- "formula could not be built"
    return(out)
  }
  out$formula <- f
  
  # ---- model frame ---------------------------------------------------------
  d <- data.table::as.data.table(dt)
  vars <- intersect(all.vars(f), names(d))
  mf <- d[, vars, with = FALSE]
  keep <- stats::complete.cases(mf)
  out$n_dropped <- sum(!keep)
  mf <- mf[keep]
  out$n <- nrow(mf)
  
  if (nrow(mf) < min_n) {
    out$reason <- paste0("n = ", nrow(mf), " after complete cases")
    return(out)
  }
  
  # ---- zero-mass guard for rq ---------------------------------------------
  # Taus at or below the mass at zero are not estimable in any useful sense:
  # the fitted quantile is pinned at the floor for every covariate pattern.
  # Assessed on the raw outcome, since a dithered value is no longer exactly 0.
  if (spec$model_type == "rq") {
    raw <- sub("_jittered$", "", spec$outcome)
    p_zero <- if (raw %chin% names(d)) mean(d[[raw]] == 0, na.rm = TRUE)
    else mean(mf[[outcome]] <= 0.5, na.rm = TRUE)
    if (spec$tau <= p_zero + 0.02) {
      out$reason <- sprintf("tau %.2f at or below zero mass (%.2f)",
                            spec$tau, p_zero)
      return(out)
    }
  }
  
  # ---- fit, capturing rq's diagnostic warnings ----------------------------
  warn_msgs <- character(0)
  
  fit_once <- function(dat) {
    withCallingHandlers(
      switch(spec$model_type,
             lm = stats::lm(f, data = dat),
             rq = quantreg::rq(f, tau = spec$tau, data = dat),
             stop("Unknown model_type: ", spec$model_type)),
      warning = function(w) {
        warn_msgs <<- c(warn_msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
  }
  
  reps <- if (spec$model_type == "rq" && dither_method == "rq")
    max(1L, as.integer(dither_reps)) else 1L
  
  coef_mat <- NULL
  fit <- NULL
  
  for (i in seq_len(reps)) {
    dat <- data.table::copy(mf)
    if (spec$model_type == "rq" && dither_method == "rq")
      data.table::set(dat, j = outcome,
                      value = quantreg::dither(dat[[outcome]],
                                               type = "symmetric",
                                               value = dither_value))
    
    fit_i <- tryCatch(fit_once(dat), error = function(e) e)
    if (inherits(fit_i, "error")) {
      out$reason <- conditionMessage(fit_i)
      out$other_warnings <- warn_msgs
      return(out)
    }
    
    cf <- stats::coef(fit_i)
    coef_mat <- if (is.null(coef_mat)) matrix(cf, nrow = 1,
                                              dimnames = list(NULL, names(cf)))
    else rbind(coef_mat, cf)
    fit <- fit_i
  }
  
  # ---- classify the captured warnings -------------------------------------
  out$nonunique_solution <- any(grepl("nonunique|non-unique", warn_msgs,
                                      ignore.case = TRUE))
  out$nonpositive_fis    <- any(grepl("nonpositive fis|non-positive fis",
                                      warn_msgs, ignore.case = TRUE))
  out$other_warnings <- unique(warn_msgs[
    !grepl("nonunique|non-unique|nonpositive fis|non-positive fis",
           warn_msgs, ignore.case = TRUE)])
  
  cf <- stats::coef(fit)
  if (anyNA(cf)) {
    out$reason <- "rank deficient: aliased coefficients"
    return(out)
  }
  
  if (reps > 1L) out$coef_averaged <- colMeans(coef_mat)
  
  out$n_params  <- length(cf)
  out$epp       <- out$n / out$n_params
  out$thin      <- out$epp < min_epp
  out$estimable <- TRUE
  out$fit       <- fit
  out
}