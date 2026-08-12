#' Build a gtsummary summary table stratified by a grouping variable
#'
#' Wraps the tbl_summary / add_p / add_overall pattern so the same table can be
#' produced for any by-variable. The continuous test is chosen automatically:
#' Wilcoxon rank-sum for two groups, Kruskal-Wallis for three or more (the
#' k-group generalisation, which reduces to Wilcoxon at k = 2).
#'
#' @param dt data.table
#' @param vars character vector of variables to summarise
#' @param by_var single grouping variable
#' @param labels named list of variable labels (e.g. label_list)
#' @param skewed_vars variables reported as median [IQR] rather than mean (SD)
#' @param continuous_vars variables to force to continuous (e.g. numeric-looking
#'   factors such as an NZDep decile)
#' @param digits named list or formula list of decimal places
#' @param simulate_fisher simulate Fisher p-values; needed when sparse cells
#'   make the network algorithm fail
#'
#' @return a gtsummary tbl_summary object
build_summary_table <- function(dt,
                                vars,
                                by_var,
                                labels = NULL,
                                skewed_vars = character(),
                                continuous_vars = character(),
                                digits = NULL,
                                simulate_fisher = FALSE,
                                pvalue_digits = 3L) {

  stopifnot(length(by_var) == 1L)

  d <- data.table::as.data.table(dt)[, c(vars, by_var), with = FALSE]

  missing_vars <- setdiff(c(vars, by_var), names(dt))
  if (length(missing_vars))
    stop("Not in data: ", paste(missing_vars, collapse = ", "))

  # k = 2 -> Wilcoxon; k >= 3 -> Kruskal-Wallis
  k <- data.table::uniqueN(stats::na.omit(d[[by_var]]))
  cont_test <- if (k <= 2L) "wilcox.test" else "kruskal.test"

  # statistic: skewed variables as median [IQR], everything else mean (SD)
  stat_list <- c(
    list(gtsummary::all_continuous() ~ "{mean} ({sd})"),
    lapply(intersect(skewed_vars, vars),
           \(v) stats::as.formula(paste0(v, ' ~ "{median} [{p25}, {p75}]"'))),
    list(gtsummary::all_categorical() ~ "{n} ({p}%)")
  )

  type_list <- lapply(intersect(continuous_vars, vars),
                      \(v) stats::as.formula(paste0(v, ' ~ "continuous"')))
  if (!length(type_list)) type_list <- NULL

  tbl <- gtsummary::tbl_summary(
    d,
    by = dplyr::all_of(by_var),
    label = if (!is.null(labels)) labels[intersect(vars, names(labels))] else NULL,
    type = type_list,
    statistic = stat_list,
    digits = digits,
    missing = "ifany",
    missing_text = "Missing"
  ) |>
    gtsummary::add_n() |>
    gtsummary::add_p(
      test = list(
        gtsummary::all_continuous()  ~ cont_test,
        gtsummary::all_categorical() ~ "fisher.test"
      ),
      test.args = if (simulate_fisher)
        gtsummary::all_categorical() ~ list(simulate.p.value = TRUE, B = 1e5)
        else NULL,
      pvalue_fun = \(x) gtsummary::style_pvalue(x, digits = pvalue_digits)
    ) |>
    gtsummary::add_overall(last = TRUE, col_label = "**Total**  \nN = {N}") |>
    gtsummary::modify_header(
      gtsummary::all_stat_cols(FALSE) ~ "**{level}**  \nN = {n}") |>
    gtsummary::modify_spanning_header(
      gtsummary::all_stat_cols(FALSE) ~
        paste0("**", if (!is.null(labels[[by_var]])) labels[[by_var]] else by_var,
               "**")) |>
    gtsummary::bold_labels()

  tbl
}
