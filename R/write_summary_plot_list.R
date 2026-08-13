#' Write a branched summary plot to a structured output path
#'
#' Takes one branch of a `*_plot_list` target and writes it to
#' <path>/<population_slug>/<plot_family>_by_<by_slug>.<ext>. Height comes
#' from the plot list rather than an aspect ratio, so panel-heavy plots get
#' the room they need; passing both width and height means `write_plot`'s
#' `aspect_ratio` default never applies.
#'
#' @param plot_list one branch: plot_family, population_slug, by_slug, plot,
#'   height_in
#' @param path root plot output directory
#' @param width_in output width in inches (6 for A4 with margins)
#' @param plot_theme ggplot theme object applied at output
#' @param plot_scales list of ggplot scale objects applied at output
#' @param device_ext file extension; pdf routes through cairo_pdf
#'
#' @return the written file path
write_summary_plot_list <- function(plot_list,
                                    path,
                                    width_in = 6,
                                    plot_theme = NULL,
                                    plot_scales = list(),
                                    device_ext = "pdf") {

  required <- c("plot_family", "population_slug", "by_slug", "plot",
                "height_in")
  missing_fields <- setdiff(required, names(plot_list))
  if (length(missing_fields))
    stop("write_summary_plot_list: missing fields: ",
         paste(missing_fields, collapse = ", "))

  write_plot(
    plot          = plot_list$plot,
    filename      = sprintf("%s_by_%s.%s", plot_list$plot_family,
                            plot_list$by_slug, device_ext),
    path          = file.path(path, plot_list$population_slug),
    width_in      = width_in,
    height_in     = plot_list$height_in,
    ggplot_theme  = plot_theme,
    ggplot_scales = plot_scales
  )
}
