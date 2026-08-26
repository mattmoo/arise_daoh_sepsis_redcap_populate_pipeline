#' Build the regression specification table
#'
#' One row per model to fit. Formulae are assembled from a nested covariate
#' ladder crossed with model type, M3 functional form and population.
#'
#' The ladder is cumulative so coefficients can be read as "what happens when
#' this block is added":
#'
#'   m0        exposures only, unadjusted
#'   m1        + age, gender
#'   m2        + deprivation, comorbidity
#'   m3_news   + NEWS                    (parsimonious severity)
#'   m3_comp   + NEWS components         (ALTERNATIVE to m3_news, not a rung)
#'   m4        m3_news + lactate
#'   m5        m3_news + triage category (MEDIATOR-adjusted, see note)
#'
#' m5 adjusts for triage category, which plausibly sits on the causal path from
#' ethnicity to DAOH. Its exposure coefficient is a direct effect holding triage
#' constant, NOT a total effect, and is flagged `mediator_adjusted`.
#'
#' Two tau sets are produced. `tau_role == "primary"` is the small set reported
#' in tables and text and given bootstrap intervals; `tau_role == "grid"` is a
#' denser net used only to draw effect-versus-tau plots, with delta-method
#' intervals because bootstrapping the whole grid is not affordable.
#'
#' @param exposures exposure variables, always placed first in the formula
#' @param taus_primary quantiles reported in tables and text
#' @param taus_grid quantiles used for plotting; the primary set is added
#'   automatically so the two are drawn on the same curve
#' @param populations population slugs
#' @param arise_max_rung rungs permitted in the ARISE-eligible population,
#'   which has roughly half the sample and cannot support the wider models
#'
#' @return data.table, one row per model
build_regression_spec_dt <- function(
    exposures = c("arise_eligible", "priority.ethnicity.desc.L1"),
    taus_primary = c(0.5, 0.75, 0.9),
    taus_grid = seq(0.05, 0.95, by = 0.05),
    populations = c("all_sepsis", "arise_eligible"),
    arise_max_rung = c("m0", "m1", "m2")) {

  # Cumulative blocks, so a change to a block propagates to every rung
  # containing it.
  blocks <- list(
    demography    = c("age_years", "gender"),
    socio_comorb  = c("nzdep2023_int", "m3_score"),
    severity_news = "news",
    severity_comp = c("first_sbp", "first_heart_rate", "first_resp_rate",
                      "first_temperature", "first_spo2", "first_avpu"),
    perfusion     = "first_lactate",
    care_process  = "triage_category"
  )

  ladder <- list(
    m0      = character(0),
    m1      = c("demography"),
    m2      = c("demography", "socio_comorb"),
    m3_news = c("demography", "socio_comorb", "severity_news"),
    m3_comp = c("demography", "socio_comorb", "severity_comp"),
    m4      = c("demography", "socio_comorb", "severity_news", "perfusion"),
    m5      = c("demography", "socio_comorb", "severity_news", "care_process")
  )

  rung_dt <- data.table::data.table(
    covariate_set = names(ladder),
    covariates = I(lapply(ladder, \(b) unlist(blocks[b], use.names = FALSE))),
    mediator_adjusted = names(ladder) == "m5",
    severity_form = data.table::fcase(
      names(ladder) %chin% c("m3_news", "m4", "m5"), "news",
      names(ladder) == "m3_comp", "components",
      default = "none")
  )

  # M3 enters from m2 onward, so only those rungs get both functional forms.
  rung_dt[, has_m3 := vapply(covariates, \(v) "m3_score" %chin% v, TRUE)]
  rung_dt <- rbind(
    rung_dt[has_m3 == FALSE][, m3_form := "none"],
    rung_dt[has_m3 == TRUE][, m3_form := "linear"],
    rung_dt[has_m3 == TRUE][, m3_form := "spline"]
  )

  # Primary taus are folded into the grid so the plotted curve passes through
  # the points reported in the tables rather than running alongside them.
  taus_all <- sort(unique(c(taus_primary, taus_grid)))

  model_dt <- data.table::rbindlist(list(
    data.table::data.table(model_type = "lm", tau = NA_real_,
                           tau_role = "primary"),
    data.table::data.table(
      model_type = "rq", tau = taus_all,
      tau_role = data.table::fifelse(taus_all %in% taus_primary,
                                     "primary", "grid"))
  ))

  spec <- data.table::CJ(population_slug = populations,
                         rung_i = rung_dt[, .I],
                         model_i = model_dt[, .I],
                         sorted = FALSE)

  spec <- cbind(spec[, .(population_slug)],
                rung_dt[spec$rung_i],
                model_dt[spec$model_i])

  # The ARISE-eligible population cannot support the wider models, and
  # arise_eligible is constant within it so it cannot be an exposure there.
  spec <- spec[population_slug != "arise_eligible" |
                 covariate_set %chin% arise_max_rung]

  spec[, exposures := I(lapply(population_slug, \(p)
    if (p == "arise_eligible") setdiff(exposures, "arise_eligible")
    else exposures))]

  # rq is fitted to the jittered outcome: the objective is degenerate with the
  # heavy ties at zero (Machado & Santos Silva 2005). lm uses the raw value.
  spec[, outcome := data.table::fifelse(model_type == "rq",
                                        "daoh_jittered", "daoh")]

  spec[, spec_id := paste0(
    population_slug, "_", covariate_set,
    data.table::fifelse(m3_form == "none", "", paste0("_m3", m3_form)),
    "_", model_type,
    data.table::fifelse(is.na(tau), "",
                        sprintf("_tau%03d", round(tau * 100))))]

  data.table::setcolorder(spec, c("spec_id", "population_slug",
                                  "covariate_set", "m3_form", "model_type",
                                  "tau", "tau_role", "outcome"))
  spec[, has_m3 := NULL]
  spec[]
}
