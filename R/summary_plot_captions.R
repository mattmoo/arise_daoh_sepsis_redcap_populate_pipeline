#' Captions for the descriptive plot families
#'
#' Captions are labels, so they are supplied at the output stage alongside the
#' scales rather than set inside the plotting functions. `labs()` returns an
#' object that adds to a plot the same way a scale does, so it travels in the
#' same list.
#'
#' Grouped in one file because they are a single family serving one purpose, in
#' the same way the scale wrappers are grouped: each records the things a reader
#' cannot recover from the figure itself.

#' Caption for continuous boxplot panels
summary_plot_continuous_caption <- function(extra = NULL) {
  txt <- "Box: median and IQR; whiskers 1.5 x IQR; diamond: mean. Points are individual patients."
  if (!is.null(extra)) txt <- paste(txt, extra)
  ggplot2::labs(caption = txt)
}


#' Caption for categorical proportion panels
#'
#' @param conf coverage of the Wilson intervals
#' @param binary TRUE when dichotomous variables have been reduced to their
#'   affirmative category, which the reader cannot otherwise infer
summary_plot_categorical_caption <- function(conf = 0.95, binary = TRUE,
                                             extra = NULL) {
  txt <- sprintf(
    "Bars are percentages within each group; lines are %d%% Wilson intervals.",
    round(conf * 100))
  if (isTRUE(binary))
    txt <- paste(txt, "Binary variables show the affirmative category only.")
  if (!is.null(extra)) txt <- paste(txt, extra)
  ggplot2::labs(caption = txt)
}


#' Caption for discrete distribution panels
#'
#' @param as_proportion TRUE when bars are percentages rather than counts
summary_plot_discrete_caption <- function(as_proportion = TRUE, extra = NULL) {
  txt <- if (isTRUE(as_proportion))
    "Bars are percentages within each group, so groups of unequal size are comparable."
    else "Bars are patient counts."
  if (!is.null(extra)) txt <- paste(txt, extra)
  ggplot2::labs(caption = txt)
}


#' Caption for the DAOH distribution histogram
#'
#' @param y_trans_note description of the y transform applied at output, or
#'   NULL if the axis is untransformed. A non-linear y transform breaks the
#'   proportionality of stacked segments, which the reader must be told.
summary_plot_daoh_caption <- function(y_trans_note = NULL, extra = NULL) {
  txt <- "One bar per day. Deaths within 90 days are stacked at the base of each bar."
  if (!is.null(y_trans_note))
    txt <- paste0(txt, " The y axis is ", y_trans_note,
                  ", so stacked segment heights are not proportional to their share.")
  if (!is.null(extra)) txt <- paste(txt, extra)
  ggplot2::labs(caption = txt)
}
