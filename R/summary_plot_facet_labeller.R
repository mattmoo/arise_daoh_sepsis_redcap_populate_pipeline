#' Labeller for regression facet strips
#'
#' ggplot2 labellers receive a data.frame of one column per faceting variable
#' and must return a list of character vectors of the same shape. This looks up
#' each variable in a named list of label lists, falling back to the raw value
#' where no label exists, so an unlabelled new covariate set shows its id rather
#' than an error.
#'
#' The same lists are reused for table row and column headers, so a label is
#' defined once and appears identically in both.
#'
#' @param ... named label lists, one per faceting variable, e.g.
#'   `covariate_set = covariate_set_label_list`
#' @param sep separator when several faceting variables are combined into one
#'   strip by the calling facet spec
#'
#' @return a function suitable for `labeller =`
summary_plot_facet_labeller <- function(..., sep = "\n") {

  lists <- list(...)

  function(df) {
    out <- lapply(names(df), function(v) {
      vals <- as.character(df[[v]])
      lk <- lists[[v]]
      if (is.null(lk)) return(vals)
      vapply(vals, function(x)
        if (!is.null(lk[[x]])) as.character(lk[[x]]) else x, "")
    })
    names(out) <- names(df)
    out
  }
}


#' Look up a single label with fallback
#'
#' Small helper used by the table builders, where a labeller function is
#' unwieldy but the same fallback behaviour is wanted.
#'
#' @param x values to label
#' @param label_list named list of labels
label_or_self <- function(x, label_list = NULL) {
  x <- as.character(x)
  if (is.null(label_list)) return(x)
  vapply(x, function(v)
    if (!is.null(label_list[[v]])) as.character(label_list[[v]]) else v, "")
}
