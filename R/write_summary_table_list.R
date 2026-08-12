#' Write a branched summary table to a structured output path
#'
#' Takes one branch of a `*_gt_list` target and writes it to
#' <path>/<population_slug>/<table_family>_by_<by_slug>.docx, creating the
#' directory if needed. Returns the file path so the calling target can use
#' `format = "file"`.
#'
#' @param table_list one branch: a list with table_family, population_slug,
#'   population_label, by_var, by_slug and table
#' @param path root table output directory
#' @param labels named list of variable labels, used for the caption
write_summary_table_list <- function(table_list, path, labels = NULL) {

  required <- c("table_family", "population_slug", "population_label",
                "by_var", "by_slug", "table")
  missing_fields <- setdiff(required, names(table_list))
  if (length(missing_fields))
    stop("write_summary_table_list: missing fields: ",
         paste(missing_fields, collapse = ", "))

  dir_path <- file.path(path, table_list$population_slug)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)

  filename <- sprintf("%s_by_%s.docx", table_list$table_family,
                      table_list$by_slug)

  by_label <- if (!is.null(labels[[table_list$by_var]]))
    labels[[table_list$by_var]] else table_list$by_var

  family_label <- c(
    demographics = "Baseline demographics",
    severity     = "Presenting severity",
    infection    = "Infection source and microbiology",
    treatments   = "Treatments",
    outcomes     = "Outcomes",
    daoh_bootstrap = "Days alive and out of hospital to 90 days"
  )[table_list$table_family]
  if (is.na(family_label)) family_label <- table_list$table_family

  caption <- sprintf("%s: %s, by %s", family_label,
                     table_list$population_label, tolower(by_label))

  write_table(
    table    = table_list$table,
    filename = filename,
    path     = dir_path,
    caption  = caption
  )
}
