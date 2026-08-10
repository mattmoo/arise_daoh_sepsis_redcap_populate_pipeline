#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param table
#' @param filename
#' @param path
write_table <-
  function(table,
           filename,
           path,
           width_in = 6,
           font_size = NULL,
           font = 'Arial',
           caption = NULL,
           padding = 1,
           ...) {
    
  dir.create(path, showWarnings = F, recursive = T)
  
  ft = NULL
  
  full_path = file.path(path, filename)
  
  if ('gtsummary' %in% class(table)) {
    ft = gtsummary::as_flex_table(table)
  }
  if ('huxtable' %in% class(table)) {
    ft = huxtable::as_flextable(table)
  }
  if (inherits(table, 'flextable')) {
    ft = table
  }
  
  if (!is.null(ft)) {
    # ft = ft %>% 
    #   autofit()
    ft = set_table_properties(ft, layout = "autofit")
    ft = flextable::width(ft, width = dim(ft)$widths*width_in /(flextable_dim(ft)$widths))
    
    ft = flextable::padding(ft, padding = 1, padding.right = 2.5, part = 'all')
    
    if (!is.null(caption)) {
      ft = flextable::set_caption(ft, caption = caption, 
                       style = "Table Caption")
    }
    
    if (!is.null(font_size)) {
      ft = flextable::fontsize(ft, size = font_size)
    }
    # if (!is.null(font)) {
      ft = flextable::font(ft, fontname = font, part = 'all')
    # }
    
    flextable::save_as_docx(ft, path = full_path, ...)
  } else if ('gt_tbl' %in% class(table)) {
    gt::gtsave(
      table,
      filename = filename,
      path = path,
      caption_location = c("top"),
      caption_align = 'center'
    )
  }
  
  return(full_path)
  
}
