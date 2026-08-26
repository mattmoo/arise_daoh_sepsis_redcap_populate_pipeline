#' Caption for effect-versus-tau figures
#'
#' A caption is a label, so it is supplied at the output stage alongside the
#' scales rather than set inside the plotting function. `labs()` returns an
#' object that can be added to a plot in the same way a scale can, so it travels
#' in the same list.
#'
#' The text records the three things a reader cannot recover from the figure
#' itself: which points carry which kind of interval, what the hollow points
#' mean, and what the rug marks at the foot represent. The final sentence
#' guards against the most common misreading of quantile regression.
#'
#' @param interval_note description of how intervals were obtained
#' @param extra additional sentence appended, for figure-specific caveats
regression_tau_caption <- function(
    interval_note = paste(
      "Points mark the taus reported in tables, which carry bootstrap",
      "intervals; the remainder of each curve uses delta-method intervals",
      "from a bootstrapped covariance matrix."),
    extra = NULL) {

  txt <- paste(
    interval_note,
    "Hollow points indicate a non-unique quantile regression solution.",
    "Dashed line and shaded band are the linear model estimate of the",
    "difference in means.",
    "Rug marks at the foot are taus that could not be fitted, at or below the",
    "proportion of patients with zero days at home.",
    "A quantile contrast is a difference between group quantiles, not the",
    "effect on patients at that quantile.")

  if (!is.null(extra)) txt <- paste(txt, extra)

  ggplot2::labs(caption = txt)
}
