#' Power for a Māori versus non-Māori difference in mean DAOH90
#'
#' Uses `pwr::pwr.t2n.test()`, which evaluates the noncentral t distribution for
#' a two-sample comparison with unequal group sizes: a published implementation,
#' slightly more conservative than a normal approximation at the small group
#' sizes that matter here, and citable.
#'
#' The purpose is to inform the design of the proposed ARISE FLUIDS sub-study,
#' not to power the present analysis, which is descriptive. The contribution is
#' the standard deviation. Before this study the available estimates came from
#' nine Māori and fifty non-Māori patients, and the protocol notes they were
#' suspected to be extreme.
#'
#' Uncertainty in the standard deviation
#' -------------------------------------
#' The Māori standard deviation is estimated from around thirty patients, so it
#' carries real uncertainty, and a power curve drawn from the point estimate
#' alone implies a precision the input does not have. Supplying the bootstrap
#' interval bounds produces three curves: the point estimate, and the curves
#' implied by pairing the two lower bounds and the two upper bounds.
#'
#' Those outer curves are a sensitivity band, NOT a confidence interval for
#' power. Pairing both lower bounds and both upper bounds is a deliberate
#' worst-and-best case; the joint sampling distribution of the two standard
#' deviations is not accounted for and a true interval would be narrower. The
#' band answers "how much does this conclusion depend on the SD estimate", which
#' for a feasibility report is the useful question.
#'
#' Note the direction: a smaller standard deviation gives greater power, so the
#' curve labelled `lower` is the upper edge of the band. The scenario labels
#' refer to the standard deviation, not to the power.
#'
#' Three further caveats.
#'
#' `pwr` parameterises the effect as Cohen's d, which assumes a common standard
#' deviation. The group estimates here are close enough for that to be
#' reasonable and are pooled below. It would not be reasonable for the prior
#' estimates in the protocol (43.9 and 20.7 days).
#'
#' The calculation compares means and assumes approximate normality of each
#' group mean. DAOH90 has a large point mass at zero and a ceiling at 90 days,
#' so at the smaller Māori sample sizes power is likely to be slightly
#' overstated. `build_quantile_power_dt()` quantifies this by resampling from
#' the observed distribution.
#'
#' Rank-based and ordinal analyses may have different power on this outcome and
#' are not evaluated here.
#'
#' @param n_total total sample sizes to evaluate
#' @param p_group proportion of the sample in the smaller group; the protocol
#'   assumed 0.20, this study observed 0.15 among ARISE-eligible patients
#' @param delta differences in mean DAOH90, in days
#' @param sd_group standard deviations for the smaller group: either a single
#'   number, or a named vector or list with elements `estimate`, `lower` and
#'   `upper` as returned by the bootstrap
#' @param sd_other the same for the comparison group
#' @param alpha two-sided significance level
#' @param target_power power at which the detectable difference is reported
#'
#' @return data.table of power by scenario, long over `sd_scenario`
build_power_dt <- function(n_total = seq(100, 900, by = 25),
                           p_group = c(0.15, 0.20),
                           delta = c(5, 7, 10, 15),
                           sd_group,
                           sd_other,
                           alpha = 0.05,
                           target_power = 0.80) {
  
  stopifnot(requireNamespace("pwr", quietly = TRUE))
  
  need <- c("estimate", "lower", "upper")
  
  # Accepts a bare number so the function still works before the bootstrap is
  # available, or when a sensitivity band is not wanted.
  as_sd_vec <- function(x, what) {
    x <- unlist(x)
    if (length(x) == 1L && is.null(names(x)))
      return(stats::setNames(rep(as.numeric(x), 3L), need))
    if (!all(need %chin% names(x)))
      stop("build_power_dt: ", what,
           " must be a single number or have estimate, lower and upper")
    x <- as.numeric(x[need])
    names(x) <- need
    # Bounds are NA where a stratum was too small for the bootstrap to return
    # an interval; fall back to the point estimate rather than failing, so a
    # curve is still produced and the missing band is visible as a flat one.
    x[is.na(x)] <- x[["estimate"]]
    x
  }
  
  sd_group <- as_sd_vec(sd_group, "sd_group")
  sd_other <- as_sd_vec(sd_other, "sd_other")
  
  scen <- data.table::data.table(
    scen_i = 1:3,
    sd_scenario = factor(need, levels = need),
    sd_group = as.numeric(sd_group[need]),
    sd_other = as.numeric(sd_other[need])
  )
  
  grid <- data.table::CJ(n_total = n_total, p_group = p_group, delta = delta,
                         scen_i = scen$scen_i, sorted = FALSE)
  d <- merge(grid, scen, by = "scen_i", sort = FALSE)
  d[, scen_i := NULL]
  
  d[, n_group := round(n_total * p_group)]
  d[, n_other := n_total - n_group]
  d <- d[n_group >= 2 & n_other >= 2]
  
  # Pooled SD is what Cohen's d assumes; it depends on the group sizes, so it
  # is recomputed per row rather than taken once.
  d[, sd_pooled := sqrt(((n_group - 1) * sd_group^2 +
                           (n_other - 1) * sd_other^2) /
                          (n_group + n_other - 2))]
  d[, cohen_d := delta / sd_pooled]
  
  d[, power := mapply(
    \(n1, n2, dd) pwr::pwr.t2n.test(n1 = n1, n2 = n2, d = dd,
                                    sig.level = alpha)$power,
    n_group, n_other, cohen_d)]
  
  # Difference detectable at the target power. Usually the number a reader of a
  # feasibility report wants: it answers "what could this trial see" without
  # requiring an effect size to be assumed first. Computed once per distinct
  # group-size and pooled-SD combination rather than per row.
  mde_key <- unique(d[, .(n_group, n_other, sd_pooled)])
  mde_key[, mde := mapply(
    \(n1, n2, sp) pwr::pwr.t2n.test(n1 = n1, n2 = n2, power = target_power,
                                    sig.level = alpha)$d * sp,
    n_group, n_other, sd_pooled)]
  
  d <- merge(d, mde_key, by = c("n_group", "n_other", "sd_pooled"),
             all.x = TRUE, sort = FALSE)
  
  d[, `:=`(alpha = alpha, target_power = target_power)]
  
  data.table::setorder(d, p_group, delta, sd_scenario, n_total)
  d[]
}