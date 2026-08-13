#' Base theme for summary plots
#'
#' Grid orientation flips with plot type: continuous plots have a categorical
#' x axis and continuous y, so horizontal gridlines help; categorical plots
#' are the other way round.
#'
#' @param n_groups number of groups on the x axis; drives label rotation
#' @param legend TRUE for categorical plots, which carry a fill legend
#' @param base_size base font size
summary_plot_theme <- function(n_groups = 2L, legend = FALSE, base_size = 9) {

  rotate <- n_groups > 3L

  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = if (legend) ggplot2::element_line(linewidth = 0.2)
                           else ggplot2::element_blank(),
      panel.grid.major.y = if (legend) ggplot2::element_blank()
                           else ggplot2::element_line(linewidth = 0.2),
      strip.text         = ggplot2::element_text(face = "bold", hjust = 0),
      strip.placement    = "outside",
      strip.text.y.left  = ggplot2::element_text(angle = 0, face = "bold",
                                                 hjust = 0),
      axis.text.x        = ggplot2::element_text(
        angle = if (rotate) 30 else 0,
        hjust = if (rotate) 1 else 0.5),
      legend.position    = if (legend) "top" else "none",
      legend.key.size    = ggplot2::unit(0.4, "cm"),
      plot.caption       = ggplot2::element_text(size = base_size - 2,
                                                 colour = "grey40", hjust = 0)
    )
}
