write_plot <- function(plot,
                       filename,
                       path,
                       width_in = 6,
                       height_in = NULL,
                       aspect_ratio = 1.5,
                       use_showtext = FALSE,
                       embed_fonts = FALSE, 
                       ggplot_theme = NULL,
                       ggplot_scales = c(),
                       thematic_theme_func = NULL,
                       bg_img_raster = NULL,
                       underlay_geom = NULL,
                       dpi = 300,
                       ...) {
  
  dir.create(path, showWarnings = F, recursive = T)
  full_path <- file.path(path, filename)
  
  # Calculate dimensions
  if (is.null(height_in) & !is.null(width_in)) {
    height_in = width_in/aspect_ratio
  }
  if (!is.null(height_in) & is.null(width_in)) {
    width_in = height_in * aspect_ratio
  }
  
  # 1. Activate showtext (Converts to paths)
  if (use_showtext) {
    showtext::showtext_auto()
    showtext::showtext_opts(dpi = dpi)
  }
  
  # ==========================================
  # 2. The Consort Trapdoor
  # ==========================================
  if (inherits(plot, "consort")) {
    
    ext <- tolower(tools::file_ext(filename))
    
    if (ext == "pdf") {
      pdf(full_path, width = width_in, height = height_in, ...)
    } else if (ext == "png") {
      png(full_path, width = width_in, height = height_in, units = "in", res = dpi, bg = "transparent", ...)
    } else if (ext == "svg") {
      svg(full_path, width = width_in, height = height_in, ...)
    } else if (ext == "eps") {
      cairo_ps(full_path, width = width_in, height = height_in, fallback_resolution = dpi, ...)
    } else {
      stop("For consort plots, write_plot only supports .pdf, .png, .svg, or .eps extensions.")
    }
    
    plot(plot)
    dev.off()
    
    if (use_showtext) showtext::showtext_auto(FALSE) 
    
    # ---> POST-PROCESSING: Embed the fonts <---
    if (embed_fonts && ext %in% c("pdf", "eps")) {
      if (!requireNamespace("extrafont", quietly = TRUE)) {
        warning("The 'extrafont' package is required to embed fonts. File saved without embedding.")
      } else {
        extrafont::embed_fonts(full_path)
      }
    }
    
    return(full_path)
  }
  
  # ==========================================
  # 3. Existing ggplot2 code continues
  # ==========================================
  
  # Add themes
  if (!is.null(ggplot_theme)) {
    if (inherits(plot, 'patchwork')) {
      plot = plot & ggplot_theme
    } else {
      plot = plot + ggplot_theme
    }
  }
  
  if (!is.null(thematic_theme_func)) {
    thematic_theme_func()
  }
  
  # Add scales
  if (length(ggplot_scales) > 0) {
    for (ggplot_scale in ggplot_scales) {
      if (inherits(plot, 'patchwork')) {
        plot = plot & ggplot_scale
      } else {
        plot = plot + ggplot_scale
      }
    }
  }
  
  # Add underlay geom
  if (!is.null(underlay_geom)) {
    if (inherits(plot, "patchwork")) {
      plot <- plot & underlay_geom
    } else {
      plot <- plot %underlay% underlay_geom
    }
  }
  
  # Add background raster
  if (!is.null(bg_img_raster)) {
    plot$layers = c(annotation_custom(grid::rasterGrob(bg_img_raster)), plot$layers)
  }
  
  # Pre-process for vector
  plot_ext <- tolower(tools::file_ext(filename))
  custom_device <- NULL
  
  if (plot_ext == "pdf") {
    custom_device <- grDevices::cairo_pdf
  } else if (plot_ext == "eps") {
    custom_device <- grDevices::cairo_ps
  }
  
  ggsave(plot = plot,
         filename = filename,
         path = path,
         width = width_in,
         height = height_in,
         bg = "transparent",
         dpi = dpi,
         device = custom_device,
         ...)
  
  if (!is.null(thematic_theme_func)) {
    thematic::thematic_off()
  }
  
  # Embed the fonts
  if (embed_fonts && plot_ext %in% c("pdf", "eps")) {
    if (!requireNamespace("extrafont", quietly = TRUE)) {
      warning("The 'extrafont' package is required to embed fonts. File saved without embedding.")
    } else {
      extrafont::embed_fonts(full_path)
    }
  }
  
  return(full_path)
}