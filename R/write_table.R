#' Write a table to docx
#'
#' Accepts a gtsummary, huxtable, flextable or gt table. The file is built in
#' a local temporary location and then copied to `path`, because assembling the
#' docx zip archive directly on a network share can fail at close.
#'
#' @title Write a table to docx
#' @param table gtsummary, huxtable, flextable or gt table
#' @param filename output file name, including extension
#' @param path output directory, created if absent
#' @param width_in total table width in inches
#' @param font_size font size in points for all parts; NULL leaves sizes as set
#' @param font font family for all parts; NULL leaves fonts as set
#' @param caption table caption; NULL for none
#' @param padding cell padding in points (top, bottom, left)
#' @param padding_right right cell padding in points
#' @param ... passed to flextable::save_as_docx(), e.g. pr_section
#' @return the written file path
write_table <-
  function(
    table,
    filename,
    path,
    width_in = 6,
    font_size = NULL,
    font = 'Arial',
    caption = NULL,
    padding = 1,
    padding_right = 2.5,
    ...
  ) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    if (!dir.exists(path)) {
      stop("write_table: could not create directory ", path)
    }

    full_path <- file.path(path, filename)

    ft <- NULL
    if (inherits(table, 'gtsummary')) {
      ft <- gtsummary::as_flex_table(table)
    } else if (inherits(table, 'huxtable')) {
      ft <- huxtable::as_flextable(table)
    } else if (inherits(table, 'flextable')) {
      ft <- table
    } else if (!inherits(table, 'gt_tbl')) {
      # Previously fell through and returned a path to a file never written
      stop(
        "write_table: unsupported table class: ",
        paste(class(table), collapse = ", ")
      )
    }

    # Build locally, then copy. tempfile() rather than a fixed name so parallel
    # workers cannot collide; the extension is kept because gtsave() picks its
    # output format from it.
    tmp_path <- tempfile(fileext = paste0(".", tools::file_ext(filename)))
    on.exit(unlink(tmp_path), add = TRUE)

    if (!is.null(ft)) {
      ft <- flextable::set_table_properties(ft, layout = "autofit")
      ft <- flextable::width(
        ft,
        width = dim(ft)$widths * width_in / flextable::flextable_dim(ft)$widths
      )

      ft <- flextable::padding(
        ft,
        padding = padding,
        padding.right = padding_right,
        part = 'all'
      )

      if (!is.null(caption)) {
        ft <- flextable::set_caption(
          ft,
          caption = caption,
          style = "Table Caption"
        )
      }

      if (!is.null(font_size)) {
        ft <- flextable::fontsize(ft, size = font_size, part = 'all')
      }
      if (!is.null(font)) {
        ft <- flextable::font(ft, fontname = font, part = 'all')
      }

      flextable::save_as_docx(ft, path = tmp_path, ...)
    } else {
      gt::gtsave(
        table,
        filename = basename(tmp_path),
        path = dirname(tmp_path),
        caption_location = c("top"),
        caption_align = 'center'
      )
    }

    if (!file.exists(tmp_path)) {
      stop("write_table: nothing was written for ", filename)
    }
    if (!file.copy(tmp_path, full_path, overwrite = TRUE)) {
      stop(
        "write_table: could not copy to ",
        full_path,
        ". Is the file open in Word?"
      )
    }

    return(full_path)
  }
