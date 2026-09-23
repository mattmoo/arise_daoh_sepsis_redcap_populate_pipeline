#' Power with a bootstrap confidence interval, from jointly resampled inputs
#'
#' Covers two estimands.
#'
#' `estimand = "difference"` is the simple two-group comparison: does mean
#' DAOH90 differ between Māori and non-Māori. This is what the present study's
#' secondary objective concerns.
#'
#' `estimand = "interaction"` is the difference in differences: is the
#' Māori/non-Māori gap smaller among trial participants than among comparable
#' patients treated outside the trial. This is the estimand implied by the
#' protocol's aim of informing "a sub-study investigating whether trial
#' participation reduces inequities". It requires a comparison group, which the
#' present cohort could supply.
#'
#' The two differ sharply in cost. The standard error of an interaction
#' involves all four cell sizes rather than two, so for equal cells it is twice
#' that of a simple difference and the sample required for the same effect is
#' roughly four times larger. Because the Māori cells are the small ones and
#' there are now two of them, the penalty here is worse than that. Showing both
#' curves together makes that cost visible rather than asserted.
#'
#' Uncertainty in the inputs
#' -------------------------
#' Power depends on three quantities estimated from this cohort: the standard
#' deviation of DAOH90 among Māori, the standard deviation among others, and
#' the proportion of patients who are Māori. All come from a few dozen
#' patients. Rather than combining their marginal confidence intervals, which
#' gives a rectangle with no defined coverage and needs an arbitrary pairing of
#' the two standard deviation bounds, patients are resampled and all three
#' parameters recomputed from each resample. Dependence between them is then
#' carried through without being modelled, and the percentile interval of the
#' resulting power values is a confidence interval for power.
#'
#' Two caveats belong in every caption drawn from this.
#'
#' The interval is conditional on the true effect being exactly `delta`. The
#' effect size is assumed, not estimated, so no uncertainty attaches to it.
#' Averaging over a prior on the effect as well would give assurance, which is
#' a different and stronger claim than anything computed here.
#'
#' The calculation compares means and assumes approximate normality of each
#' cell mean. DAOH90 has a large point mass at zero and a ceiling, so at the
#' smaller Māori cell sizes power is likely to be slightly overstated, and the
#' bootstrap propagates that bias rather than correcting it.
#'
#' @param dt cohort data containing `daoh` and `maori`
#' @param n_total total participants; for the interaction this is the trial arm
#'   and the comparison group is sized by `control_ratio`
#' @param delta effects in days: a difference in means for "difference", a
#'   reduction in the gap for "interaction"
#' @param estimand "difference" or "interaction"
#' @param control_ratio size of the comparison group relative to `n_total`;
#'   ignored for "difference". A contemporaneous non-trial cohort may be
#'   considerably larger than the trial, which is the cheapest way to recover
#'   some of the interaction's lost precision.
#' @param R bootstrap resamples
#' @param alpha two-sided significance level
#' @param conf coverage of the reported interval
#' @param target_power power at which the detectable effect is reported
#' @param p_fixed optional fixed Māori proportion, e.g. the protocol's
#'   assumption, in which case only the standard deviations are resampled
#' @param min_cell_n smallest cell retained; below this the calculation is not
#'   meaningful whatever it returns
#'
#' @return data.table with the point estimate, bootstrap mean and percentile
#'   interval of power, by sample size and effect size
build_power_boot_dt <- function(dt,
                                n_total = seq(100, 900, by = 25),
                                delta = c(5, 7, 10, 15),
                                estimand = c("difference", "interaction"),
                                control_ratio = 1,
                                R = 2000L,
                                alpha = 0.05,
                                conf = 0.95,
                                target_power = 0.80,
                                p_fixed = NULL,
                                min_cell_n = 10L) {
  
  estimand <- match.arg(estimand)
  
  d <- data.table::as.data.table(dt)[!is.na(daoh) & !is.na(maori)]
  y <- d$daoh
  g <- as.character(d$maori)
  n <- length(y)
  if (n < 30L) stop("build_power_boot_dt: only ", n, " usable patients")
  
  maori_lab <- grep("^non", unique(g), value = TRUE, invert = TRUE)
  if (length(maori_lab) != 1L)
    stop("build_power_boot_dt: could not identify the M\u0101ori level from: ",
         paste(unique(g), collapse = ", "))
  
  params <- function(idx) {
    gi <- g[idx]; yi <- y[idx]
    a <- yi[gi == maori_lab]; b <- yi[gi != maori_lab]
    if (length(a) < 3L || length(b) < 3L) return(NULL)
    c(sd_group = stats::sd(a), sd_other = stats::sd(b),
      p = length(a) / length(idx))
  }
  
  obs <- params(seq_len(n))
  if (is.null(obs)) stop("build_power_boot_dt: too few in one group")
  
  # Patients are resampled, not parameters, so the three estimates come from
  # the same resample and their dependence is preserved.
  boot_par <- matrix(NA_real_, nrow = R, ncol = 3,
                     dimnames = list(NULL, names(obs)))
  for (r in seq_len(R)) {
    pr <- params(sample.int(n, n, replace = TRUE))
    if (!is.null(pr)) boot_par[r, ] <- pr
  }
  boot_par <- boot_par[stats::complete.cases(boot_par), , drop = FALSE]
  if (!nrow(boot_par)) stop("build_power_boot_dt: no usable resamples")
  
  if (!is.null(p_fixed)) {
    boot_par[, "p"] <- p_fixed
    obs[["p"]] <- p_fixed
  }
  
  # ---- cell sizes, pooled SD and the contrast standard error --------------
  cells <- function(n_tot, p) {
    n1 <- round(n_tot * p)
    if (estimand == "difference") return(c(n1, n_tot - n1))
    nc <- round(n_tot * control_ratio)
    nc1 <- round(nc * p)
    c(n1, n_tot - n1, nc1, nc - nc1)
  }
  
  # The two Māori cells share one standard deviation and the two comparison
  # cells the other.
  pooled_sd <- function(nn, sd_g, sd_o) {
    k <- length(nn)
    num <- if (k == 2L)
      (nn[1] - 1) * sd_g^2 + (nn[2] - 1) * sd_o^2
    else
      (nn[1] - 1) * sd_g^2 + (nn[2] - 1) * sd_o^2 +
      (nn[3] - 1) * sd_g^2 + (nn[4] - 1) * sd_o^2
    den <- sum(nn) - k
    if (den <= 0) return(NA_real_)
    sqrt(num / den)
  }
  
  # All four cells contribute to an interaction, which is where the cost lies.
  contrast_se <- function(nn, sp) sp * sqrt(sum(1 / nn))
  
  pow <- function(n_tot, dlt, sd_g, sd_o, p) {
    nn <- cells(n_tot, p)
    if (any(nn < min_cell_n)) return(NA_real_)
    sp <- pooled_sd(nn, sd_g, sd_o)
    if (!is.finite(sp) || sp <= 0) return(NA_real_)
    se  <- contrast_se(nn, sp)
    df  <- sum(nn) - length(nn)
    ncp <- dlt / se
    crit <- stats::qt(1 - alpha / 2, df)
    stats::pt(-crit, df, ncp) + (1 - stats::pt(crit, df, ncp))
  }
  
  # Detectable effect, solved numerically: the noncentral t has no simple
  # inverse in the effect size.
  mde <- function(n_tot, sd_g, sd_o, p) {
    nn <- cells(n_tot, p)
    if (any(nn < min_cell_n)) return(NA_real_)
    f <- function(dl) pow(n_tot, dl, sd_g, sd_o, p) - target_power
    lo <- f(0.01); hi <- f(200)
    if (!is.finite(lo) || !is.finite(hi)) return(NA_real_)
    if (lo > 0) return(0.01)
    if (hi < 0) return(NA_real_)
    stats::uniroot(f, c(0.01, 200))$root
  }
  
  probs <- c((1 - conf) / 2, 1 - (1 - conf) / 2)
  grid <- data.table::CJ(n_total = n_total, delta = delta, sorted = FALSE)
  
  res <- data.table::rbindlist(lapply(grid[, .I], function(i) {
    
    nt <- grid$n_total[i]; dl <- grid$delta[i]
    nn <- cells(nt, obs[["p"]])
    
    pv <- vapply(seq_len(nrow(boot_par)), function(r)
      pow(nt, dl, boot_par[r, "sd_group"], boot_par[r, "sd_other"],
          boot_par[r, "p"]), 0)
    pv <- pv[is.finite(pv)]
    
    if (!length(pv))
      return(data.table::data.table(
        n_total = nt, delta = dl, power = NA_real_, power_mean = NA_real_,
        power_lower = NA_real_, power_upper = NA_real_,
        n_group = NA_integer_, n_other = NA_integer_,
        n_control_group = NA_integer_, n_control_other = NA_integer_,
        mde = NA_real_, R_used = 0L))
    
    ci <- stats::quantile(pv, probs, names = FALSE, type = 8)
    
    data.table::data.table(
      n_total = nt, delta = dl,
      power = pow(nt, dl, obs[["sd_group"]], obs[["sd_other"]], obs[["p"]]),
      # Mean over the bootstrap distribution: power to be expected once
      # parameter uncertainty is allowed for. Not assurance, which would also
      # average over a prior on the effect size.
      power_mean = mean(pv),
      power_lower = ci[1], power_upper = ci[2],
      n_group = nn[1], n_other = nn[2],
      n_control_group = if (length(nn) > 2L) nn[3] else NA_integer_,
      n_control_other = if (length(nn) > 2L) nn[4] else NA_integer_,
      mde = mde(nt, obs[["sd_group"]], obs[["sd_other"]], obs[["p"]]),
      R_used = length(pv))
  }))
  
  res[, `:=`(
    estimand = estimand,
    control_ratio = if (estimand == "interaction") control_ratio else NA_real_,
    alpha = alpha, conf = conf, target_power = target_power,
    R = nrow(boot_par),
    sd_group = obs[["sd_group"]], sd_other = obs[["sd_other"]],
    p_group = obs[["p"]],
    p_source = if (is.null(p_fixed)) "observed" else "fixed",
    n_cohort = n)]
  
  data.table::setorder(res, delta, n_total)
  res[]
}