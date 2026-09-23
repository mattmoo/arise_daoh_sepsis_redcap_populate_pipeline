#' Quantile axis for effect-versus-tau plots
#'
#' Carries its own axis title, since axis titles are labels and are applied at
#' the output stage rather than set in the plotting function.
#'
#' Tau is a proportion, so it is labelled as a percentage. Limits are left to
#' the data rather than fixed at 0 to 1, because the estimable range differs
#' between populations and a fixed axis would devote most of the panel to
#' quantiles that could never be fitted.
#'
#' @param name axis title
#' @param by break interval on the tau scale
#' @param accuracy rounding for the percentage labels
#' @param expand axis expansion
regression_tau_x_scale <- function(name = "Quantile of DAOH (tau)",
                                   breaks = seq(0, 1, by = 0.25),
                                   accuracy = 1,
                                   expand = ggplot2::expansion(c(0.02, 0.02)),
                                   ...) {
  ggplot2::scale_x_continuous(
    name   = name,
    breaks = breaks,
    labels = scales::percent_format(accuracy = accuracy),
    expand = expand,
    ...
  )
}
