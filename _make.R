library(targets)
library(data.table)

# Timestamp taken before the run, so the warning report below covers only
# targets rebuilt by this call. tar_meta() holds one record per target from its
# last successful build and never prunes, so without this filter the report
# returns historical warnings from targets that were skipped.
run_started_at <- Sys.time()

tar_make(names = c(
  table_demographics_docx_file_list,
  table_severity_docx_file_list,
  table_infection_docx_file_list,
  table_treatment_docx_file_list,
  table_outcome_docx_file_list,
  daoh_boot_docx_file_list,
  attrition_table_docx_file,
  
  plot_demographics_continuous_pdf_file_list,
  plot_demographics_categorical_pdf_file_list,
  plot_severity_continuous_pdf_file_list,
  plot_severity_categorical_pdf_file_list,
  plot_infection_pdf_file_list,
  plot_treatment_continuous_pdf_file_list,
  plot_treatment_categorical_pdf_file_list,
  plot_outcome_pdf_file_list,
  plot_deprivation_pdf_file_list,
  plot_daoh_distribution_pdf_file_list,
  
  regression_ladder_table_docx_file_list,
  regression_inventory_docx_file,
  regression_group_plot_pdf_file_list,
  regression_single_plot_pdf_file_list,
  
  arise_sepsis_eligibility_xlsx_file,
  arise_sepsis_xlsx_file
))


#' Warnings and errors from the targets rebuilt since a given time
#'
#' Branch hashes are stripped so a warning repeated across two hundred branches
#' collapses to one row with a count, which is the difference between a readable
#' report and a screenful of identical lines.
#'
#' @param since only report targets built after this time
report_run_messages <- function() {
  
  prog <- as.data.table(tar_progress())
  built <- prog[progress %chin% c("built", "errored"), name]
  
  m <- as.data.table(tar_meta(fields = c("warnings", "error", "seconds"),
                              complete_only = TRUE))
  recent <- m[name %chin% built]
  
  cat("\n===== RUN SUMMARY =====\n")
  print(prog[, .N, by = progress])
  cat(sprintf("Total time : %.1f s\n", sum(recent$seconds, na.rm = TRUE)))
  
  err <- recent[!is.na(error),
                .(target = sub("_[0-9a-f]{16}$", "", name), error)]
  cat(sprintf("\n===== ERRORS (%d branches) =====\n", nrow(err)))
  if (nrow(err)) print(err[, .N, by = .(target, error)][order(-N)]) else cat("None.\n")
  
  wrn <- recent[!is.na(warnings),
                .(target = sub("_[0-9a-f]{16}$", "", name), warnings)]
  cat(sprintf("\n===== WARNINGS (%d branches) =====\n", nrow(wrn)))
  if (nrow(wrn)) print(wrn[, .N, by = .(target, warnings)][order(-N)]) else cat("None.\n")
  
  cat("\n===== SLOWEST =====\n")
  print(recent[, .(seconds = sum(seconds, na.rm = TRUE), branches = .N),
               by = .(target = sub("_[0-9a-f]{16}$", "", name))][
                 order(-seconds)][seq_len(min(10, .N))])
  
  invisible(list(errors = err, warnings = wrn))
}

report_run_messages()