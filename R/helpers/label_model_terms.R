#' Turn model coefficient names into readable labels
#'
#' Coefficient names carry the variable, and for factors the level, glued
#' together with no separator: `genderMale`, `priority.ethnicity.desc.L1Māori`.
#' Splines add a wrapper: `rms::rcs(m3_score, 3)m3_score` for the linear basis
#' and the same with a trailing apostrophe for the nonlinear one. Interactions
#' join two such terms with a colon.
#'
#' Naive prefix matching against a label list gets this wrong in three ways,
#' all of which this handles:
#'
#'   Variables that prefix one another. `news` is a prefix of `news_temperature`,
#'   so matching in list order lets the shorter name capture the longer one's
#'   coefficients. Candidates are therefore tried longest first.
#'
#'   Factor levels. Leaving the level glued to the label gives "GenderMale".
#'   The level is separated out and joined with `level_sep`.
#'
#'   Spline bases. The wrapper is stripped and the two basis functions are
#'   distinguished, since a reader needs to know that two rows describe one
#'   variable and that only the first is a slope in the usual sense.
#'
#' Anything unmatched is returned unchanged rather than dropped, so a new
#' covariate shows its raw name instead of vanishing from the table.
#'
#' @param terms coefficient names
#' @param labels named list of variable labels
#' @param level_sep separator between a variable label and a factor level
#' @param spline_suffix labels for the linear and nonlinear spline bases
#' @param intercept_label label for the intercept row
#'
#' @return named character vector suitable for `modelsummary::coef_map`
label_model_terms <- function(terms,
                              labels = NULL,
                              level_sep = ": ",
                              spline_suffix = c(" (spline, linear)",
                                                " (spline, nonlinear)"),
                              intercept_label = "Intercept") {

  vars <- names(labels)
  # Longest first, so a variable that is a prefix of another cannot capture it.
  if (length(vars)) vars <- vars[order(nchar(vars), decreasing = TRUE)]

  lab_of <- function(v) if (!is.null(labels[[v]])) as.character(labels[[v]]) else v

  label_one <- function(tm) {

    if (tm %chin% c("(Intercept)", "Intercept")) return(intercept_label)

    # Splines: rms::rcs(x, k)x  and  rms::rcs(x, k)x'
    m <- regmatches(tm, regexec("^(?:rms::)?rcs\\(([^,\\)]+)[^\\)]*\\)(.*)$", tm))[[1]]
    if (length(m) == 3L) {
      v <- trimws(m[2])
      nonlinear <- grepl("'", m[3], fixed = TRUE)
      return(paste0(lab_of(v),
                    spline_suffix[if (nonlinear) 2L else 1L]))
    }

    # Other transformations applied in the formula, e.g. log(x)
    m <- regmatches(tm, regexec("^([A-Za-z.]+)\\(([^,\\)]+)[^\\)]*\\)(.*)$", tm))[[1]]
    if (length(m) == 4L && trimws(m[3]) %chin% vars) {
      v <- trimws(m[3])
      lvl <- m[4]
      base <- paste0(m[2], "(", lab_of(v), ")")
      return(if (nzchar(lvl)) paste0(base, level_sep, lvl) else base)
    }

    # Plain variable, possibly with a factor level appended
    hit <- vars[startsWith(tm, vars)]
    if (length(hit)) {
      v <- hit[1L]
      lvl <- substring(tm, nchar(v) + 1L)
      return(if (nzchar(lvl)) paste0(lab_of(v), level_sep, lvl) else lab_of(v))
    }

    tm
  }

  out <- vapply(terms, function(tm) {
    # Interactions: label each side and rejoin
    parts <- strsplit(gsub("::", "\u0001", tm), ":", fixed = TRUE)[[1]]
    parts <- gsub("\u0001", "::", parts, fixed = TRUE)
    # A colon inside a spline wrapper would be split wrongly; rcs() has none,
    # so this is safe for the formulae used here.
    paste(vapply(parts, label_one, ""), collapse = " \u00d7 ")
  }, "")

  stats::setNames(out, terms)
}
