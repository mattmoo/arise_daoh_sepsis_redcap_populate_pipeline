#' Effect axis for regression contrast plots
#'
#' Carries its own axis title, since axis titles are labels and are applied at
#' the output stage rather than set in the plotting function.
#'
#' Units are days of DAOH. Breaks are placed at whole days by default, because
#' fractional-day breaks invite over-reading of small differences in an outcome
#' measured in whole days.
#'
#' `symmetric = TRUE` centres the axis on zero so a positive and a negative
#' contrast of the same size occupy the same visual distance; an off-centre axis
#' makes an effect look larger in whichever direction has less room. The limit
#' function guards against panels containing no finite values, which occurs when
#' every model in a panel was non-estimable and would otherwise warn and return
#' an infinite range.
#'
#' @param name axis title
#' @param symmetric centre the axis on zero
#' @param by break interval in days; NULL lets ggplot choose
#' @param expand axis expansion
regression_effect_y_scale <- function(name = "Difference in DAOH, days",
                                      symmetric = TRUE,
                                      by = NULL,
                                      expand = ggplot2::expansion(c(0.05, 0.05)),
                                      ...) {

  brk <- if (is.null(by)) ggplot2::waiver()
         else function(lim) seq(floor(lim[1] / by) * by,
                                ceiling(lim[2] / by) * by, by = by)

  lim <- if (isTRUE(symmetric)) function(lim) {
    finite <- lim[is.finite(lim)]
    if (!length(finite)) return(c(-1, 1))
    m <- max(abs(finite))
    if (!is.finite(m) || m == 0) c(-1, 1) else c(-1, 1) * m
  } else ggplot2::waiver()

  ggplot2::scale_y_continuous(
    name   = name,
    breaks = brk,
    limits = lim,
    expand = expand,
    ...
  )
}
