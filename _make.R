library(targets)
library(data.table)

# =============================================================================
# Build the analysis outputs and report on the run.
#
# tar_make() is scoped to the output targets rather than run bare, so that
# exploratory or superseded targets left in _targets.R do not rebuild. Upstream
# dependencies build automatically.
# =============================================================================

tar_make(names = tidyselect::matches("_(docx|pdf|xlsx)_file(_list)?$|_readme_file"))

#' Report on the targets built by the run that has just finished
#'
#' Scoped by `tar_progress()` rather than by build time. `tar_meta()` holds one
#' record per target from its last successful build and never prunes, so a
#' report filtered on time returns historical warnings from targets that were
#' skipped, and the time field is not reliably the build time in any case.
#' `tar_progress()` resets each run and reports exactly what was attempted.
#'
#' Branch hashes are stripped so a warning repeated across two hundred branches
#' collapses to one row with a count, which is the difference between a readable
#' report and a screenful of identical lines.
#'
#' @return invisibly, a list of the error and warning tables
report_run_messages <- function() {
  
  strip_branch <- function(x) sub("_[0-9a-f]{16}$", "", x)
  
  prog <- as.data.table(tar_progress())
  attempted <- prog[progress %chin% c("completed", "errored"), name]
  
  meta <- as.data.table(tar_meta(
    fields = c("warnings", "error", "seconds"), complete_only = TRUE))
  recent <- meta[name %chin% attempted]
  
  cat("\n================ RUN SUMMARY ================\n")
  if (nrow(prog)) print(prog[, .N, by = progress][order(-N)])
  cat(sprintf("\nTotal build time: %.1f s\n",
              sum(recent$seconds, na.rm = TRUE)))
  
  # ---- errors --------------------------------------------------------------
  err <- recent[!is.na(error), .(target = strip_branch(name), error)]
  cat(sprintf("\n---- ERRORS (%d branches) ----\n", nrow(err)))
  if (nrow(err)) {
    print(err[, .N, by = .(target, error)][order(-N)])
  } else {
    cat("None.\n")
  }
  
  # ---- warnings ------------------------------------------------------------
  wrn <- recent[!is.na(warnings), .(target = strip_branch(name), warnings)]
  cat(sprintf("\n---- WARNINGS (%d branches) ----\n", nrow(wrn)))
  if (nrow(wrn)) {
    print(wrn[, .N, by = .(target, warnings)][order(-N)])
  } else {
    cat("None.\n")
  }
  
  # ---- slowest -------------------------------------------------------------
  slow <- recent[, .(seconds = round(sum(seconds, na.rm = TRUE), 1),
                     branches = .N),
                 by = .(target = strip_branch(name))][order(-seconds)]
  cat("\n---- SLOWEST TARGETS ----\n")
  print(slow[seq_len(min(10L, .N))])
  
  # ---- provenance ----------------------------------------------------------
  # Printed so the hash can be checked against the one embedded in the output
  # folder READMEs and the xlsx filenames. Outputs carrying different hashes
  # were built from different data and should not be quoted together.
  cat("\n---- PROVENANCE ----\n")
  hash <- tryCatch(tar_read(analysis_data_hash), error = function(e) NA)
  cat("Analysis data hash:", if (is.na(hash)) "unavailable" else hash, "\n")
  cat("Run completed     :",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n")
  
  # ---- stale output guard --------------------------------------------------
  # A failed run leaves whatever was written before the failure in the output
  # folders, where a co-author will open it without knowing the run did not
  # finish. Say so loudly rather than relying on the console scrollback.
  if (nrow(prog[progress == "errored"])) {
    cat("\n")
    warning("Pipeline errored. Output folders may contain files from a ",
            "partial run and should not be circulated until it completes ",
            "cleanly.", call. = FALSE, immediate. = TRUE)
  }
  
  invisible(list(errors = err, warnings = wrn, slowest = slow))
}

report_run_messages()