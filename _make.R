library(targets)
library(data.table)

# =============================================================================
# Build the analysis outputs and report on the run.
#
# tar_make() is scoped to the output targets rather than run bare, so that
# exploratory or superseded targets left in _targets.R do not rebuild. Upstream
# dependencies build automatically.
# =============================================================================

tar_make(
  names = tidyselect::matches("_(docx|pdf|xlsx)_file(_list)?$|_readme_file"),
  use_crew = TRUE
)


#' Report on the targets built by the run that has just finished
#'
#' Scoped by `tar_progress()`, which resets each run, because `tar_meta()`
#' keeps warnings from each target's last build indefinitely.
#'
#' `complete_only` must stay FALSE: with several fields it keeps only rows with
#' no missing value in any of them, i.e. targets that both warned and errored.
#'
#' Branches are reported by their position in the pattern, so a warning can be
#' inspected with `tar_read(target, branches = i)`.
#'
#' @param max_branches most branch indices to list per message
#' @param collapse_numbers replace numbers in warnings with N, so messages
#'   differing only in a count are merged
#' @return invisibly, a list of the error, warning and timing tables
report_run_messages <- function(max_branches = 20L, collapse_numbers = TRUE) {
  prog <- as.data.table(tar_progress())
  attempted <- prog[progress %chin% c("completed", "errored"), name]

  meta <- as.data.table(tar_meta(
    fields = tidyselect::any_of(c("warnings", "error", "seconds", "children")),
    complete_only = FALSE
  ))

  # Map each branch to its parent target and its position in the pattern
  kids <- meta[, .(name = unlist(children)), by = .(target = name)]
  kids <- kids[!is.na(name)]
  kids[, branch := seq_len(.N), by = target]

  recent <- merge(
    meta[name %chin% attempted, .(name, warnings, error, seconds)],
    kids,
    by = "name",
    all.x = TRUE
  )
  recent[is.na(target), target := name]

  fmt_branches <- function(i) {
    i <- sort(i[!is.na(i)])
    if (!length(i)) {
      return("not branched")
    }
    shown <- paste(head(i, max_branches), collapse = ", ")
    if (length(i) > max_branches) {
      shown <- paste0(shown, ", ... (", length(i), " in total)")
    }
    shown
  }

  # targets stores a target's warnings as one string joined by ". ", so they
  # are split back out and counted. Numbers are collapsed to N for warnings so
  # messages differing only in a count merge; errors are left verbatim.
  print_messages <- function(d, col, heading, split = FALSE) {
    d <- d[!is.na(get(col)) & nzchar(get(col))]
    cat(sprintf("\n---- %s (%d targets or branches) ----\n", heading, nrow(d)))
    if (!nrow(d)) {
      cat("None.\n")
      return(invisible(d))
    }

    long <- d[,
      .(
        msg = if (split) {
          unlist(strsplit(get(col), ". ", fixed = TRUE))
        } else {
          get(col)
        }
      ),
      by = .(target, branch, name)
    ]
    long[, msg := sub("\\.$", "", trimws(msg))]
    if (split && collapse_numbers) {
      long[, msg := gsub("\\b[0-9]+(\\.[0-9]+)?\\b", "N", msg)]
    }

    grouped <- long[,
      .(n = .N, branches = fmt_branches(unique(branch))),
      by = .(target, msg)
    ][order(target, -n)]

    for (tg in unique(grouped$target)) {
      g <- grouped[target == tg]
      cat("\n", tg, "\n", sep = "")
      cat(
        sprintf("  %4dx  %s  [branches: %s]\n", g$n, g$msg, g$branches),
        sep = ""
      )
    }
    invisible(d)
  }

  cat("\n================ RUN SUMMARY ================\n")
  if (nrow(prog)) {
    print(prog[, .N, by = progress][order(-N)])
  }
  cat(sprintf(
    "\nTotal build time: %.1f s\n",
    sum(recent$seconds, na.rm = TRUE)
  ))

  err <- print_messages(recent, "error", "ERRORS")
  wrn <- print_messages(recent, "warnings", "WARNINGS", split = TRUE)

  slow <- recent[,
    .(seconds = round(sum(seconds, na.rm = TRUE), 1), branches = .N),
    by = target
  ][order(-seconds)]
  cat("\n---- SLOWEST TARGETS ----\n")
  print(slow[seq_len(min(10L, .N))])

  # Printed so the hash can be checked against the one in the output folder
  # READMEs and the xlsx filenames.
  cat("\n---- PROVENANCE ----\n")
  hash <- tryCatch(tar_read(analysis_data_hash), error = function(e) NA)
  cat("Analysis data hash:", if (is.na(hash)) "unavailable" else hash, "\n")
  cat("Run completed     :", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n")

  # A failed run leaves earlier files in the output folders, where a co-author
  # will open them without knowing the run did not finish.
  if (nrow(prog[progress == "errored"])) {
    cat("\n")
    warning(
      "Pipeline errored. Output folders may contain files from a ",
      "partial run and should not be circulated until it completes ",
      "cleanly.",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  invisible(list(errors = err, warnings = wrn, slowest = slow))
}

report_run_messages()
