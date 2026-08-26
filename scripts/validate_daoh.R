# =============================================================================
# Manual DAOH validation for a single index event
#
# Conventions assumed, inferred from the data and confirmed against the
# component sums (dih + dd + daoh == 90 for every row):
#
#   * The window runs from day 0 (daoh_period_start, the ED presentation date)
#     to day 89 (daoh_period_end), i.e. 90 calendar days inclusive.
#   * Any part of a calendar day spent in hospital makes that whole day an
#     in-hospital day. Same for dead.
#   * Day 0 counts as in hospital.
#   * Dead takes precedence over in hospital on the day of death, so a death
#     on day d gives dd = 90 - d, and dih can only accrue over days 0..d-1.
#
# Run interactively after tar_load(); nothing here writes to disk.
#
#   source("scripts/validate_daoh.R")
#   daoh_sanity_check()             # run this first
#   validate_daoh(1987)             # by index_event_id
#   validate_daoh(pms = "6183329")  # or by PMS identifier
# =============================================================================

library(data.table)
library(targets)

tar_load(c(analysis_dt, hospitalisation_dt, daoh_dt, daoh_limits))

DAOH_WINDOW_DAYS <- diff(daoh_limits) + 1L   # 0..89 inclusive = 90


#' Print and recompute DAOH for one index event
#'
#' @param id index_event_id to check
#' @param pms alternative lookup by pms_unique_identifier
validate_daoh <- function(id = NULL, pms = NULL) {
  
  row <- if (!is.null(id)) {
    idv <- id
    analysis_dt[index_event_id == idv]
  } else {
    pmsv <- as.character(pms)
    analysis_dt[pms_unique_identifier == pmsv]
  }
  
  if (!nrow(row))      stop("No matching index event.")
  if (nrow(row) > 1L)  stop("Identifier matched more than one row.")
  
  nhi   <- row$PRIM_HCU
  start <- as.IDate(row$daoh_period_start)
  end   <- as.IDate(row$daoh_period_end)
  dod   <- if (is.na(row$DOD)) NA else as.IDate(row$DOD)
  
  cat("\n===== INDEX EVENT =====\n")
  print(row[, .(index_event_id, pms_unique_identifier, nmds_event_id,
                ed_presentation_date, arise_eligible, mort.90.day)])
  
  cat("\n===== WINDOW =====\n")
  cat(sprintf("Day 0  (start)       : %s\n", start))
  cat(sprintf("Day 89 (end)         : %s\n", end))
  cat(sprintf("Days inclusive       : %d  (expected %d)\n",
              as.integer(end - start) + 1L, DAOH_WINDOW_DAYS))
  
  # ---- day-by-day construction -------------------------------------------
  # Building the whole window explicitly is slower than arithmetic but makes
  # the precedence rules visible and impossible to get subtly wrong.
  days <- data.table(day = 0:(DAOH_WINDOW_DAYS - 1L))
  days[, date := start + day]
  
  adm <- hospitalisation_dt[PRIM_HCU == nhi]
  setorder(adm, EVSTDATE)
  
  cat("\n===== ALL HOSPITAL EVENTS FOR THIS PATIENT =====\n")
  if (nrow(adm)) {
    print(adm[, .(EVSTDATE, EVENDATE,
                  overlaps_window = EVSTDATE <= end & EVENDATE >= start)])
  } else {
    cat("None. If the index admission is missing, the linkage is the problem,\n")
    cat("not the DAOH arithmetic.\n")
  }
  
  # Any overlap with a calendar day makes that day in-hospital
  days[, in_hospital := vapply(date, function(dt)
    any(adm$EVSTDATE <= dt & adm$EVENDATE >= dt), TRUE)]
  
  # Dead from the day of death onward, and dead wins over in-hospital
  days[, dead := !is.na(dod) & date >= dod]
  days[dead == TRUE, in_hospital := FALSE]
  
  days[, at_home := !dead & !in_hospital]
  
  dih_manual  <- days[, sum(in_hospital)]
  dd_manual   <- days[, sum(dead)]
  daoh_manual <- days[, sum(at_home)]
  
  cat("\n===== FIRST AND LAST 10 DAYS =====\n")
  print(rbind(head(days, 10), tail(days, 10)))
  
  cat("\n===== TRANSITIONS =====\n")
  # Only the days where status changes, which is where errors live
  days[, state := fifelse(dead, "dead",
                          fifelse(in_hospital, "hospital", "home"))]
  chg <- days[state != shift(state, fill = "")]
  print(chg[, .(day, date, state)])
  
  cat("\n===== MORTALITY =====\n")
  if (is.na(dod)) {
    cat("Alive, or death not recorded.\n")
  } else {
    cat(sprintf("Date of death        : %s (day %d)\n", dod,
                as.integer(dod - start)))
    if (dod > end) cat("Death is after the window; dd should be 0.\n")
    if (dod < start) cat("WARNING: death precedes the window start.\n")
  }
  
  cat("\n===== COMPARISON =====\n")
  comp <- data.table(
    quantity = c("Days in hospital (dih)", "Days dead (dd)", "DAOH", "Sum"),
    pipeline = c(row$dih, row$dd, row$daoh, row$dih + row$dd + row$daoh),
    manual   = c(dih_manual, dd_manual, daoh_manual,
                 dih_manual + dd_manual + daoh_manual)
  )
  comp[, agrees := pipeline == manual]
  print(comp)
  
  if (!all(comp$agrees)) {
    cat("\nMISMATCH. Check, in this order:\n")
    cat("  1. Whether the day of death counts as dead or as in-hospital.\n")
    cat("  2. Whether a discharge day counts as in-hospital or at home.\n")
    cat("  3. Transfers recorded as separate NMDS events with abutting dates.\n")
    cat("  4. Hospital events missing from hospitalisation_dt for this NHI.\n")
  }
  
  invisible(days[])
}


#' Cohort-wide consistency checks
daoh_sanity_check <- function() {
  
  cat("\n===== COMPONENT SUMS =====\n")
  s <- analysis_dt[, dih + dd + daoh]
  cat(sprintf("Distinct values of dih + dd + daoh: %s  (expected %d only)\n",
              paste(sort(unique(s)), collapse = ", "), DAOH_WINDOW_DAYS))
  if (length(unique(s)) > 1L)
    print(analysis_dt[dih + dd + daoh != DAOH_WINDOW_DAYS,
                      .(index_event_id, dih, dd, daoh, total = dih + dd + daoh)])
  
  cat("\n===== WINDOW LENGTHS =====\n")
  w <- analysis_dt[, as.integer(as.IDate(daoh_period_end) -
                                  as.IDate(daoh_period_start)) + 1L]
  cat(sprintf("Distinct window lengths: %s\n",
              paste(sort(unique(w)), collapse = ", ")))
  
  cat("\n===== DAYS DEAD VS DATE OF DEATH =====\n")
  # dd should equal 90 minus the day index of death, for deaths inside window
  chk <- analysis_dt[!is.na(DOD)][
    , death_day := as.integer(as.IDate(DOD) - as.IDate(daoh_period_start))][
      death_day >= 0 & death_day < DAOH_WINDOW_DAYS][
        , dd_expected := DAOH_WINDOW_DAYS - death_day]
  bad <- chk[dd != dd_expected]
  if (nrow(bad)) {
    cat(sprintf("%d rows where dd does not match the date of death:\n",
                nrow(bad)))
    print(head(bad[, .(index_event_id, daoh_period_start, DOD, death_day,
                       dd, dd_expected)], 20))
  } else {
    cat("All deaths inside the window reconcile with dd.\n")
  }
  
  cat("\n===== EDGE CASES WORTH EYEBALLING =====\n")
  
  cat("\nDied by 90 days but DAOH > 60 (died late, mostly at home):\n")
  print(analysis_dt[mort.90.day == TRUE & daoh > 60,
                    .(index_event_id, daoh, dih, dd, DOD)])
  
  cat("\nAlive at 90 days but DAOH = 0 (whole window in hospital):\n")
  print(analysis_dt[mort.90.day == FALSE & daoh == 0,
                    .(index_event_id, daoh, dih, dd)])
  
  cat("\ndih = 0 (no in-hospital day at all, including day 0):\n")
  print(analysis_dt[dih == 0, .(index_event_id, dih, dd, daoh, DOD)])
  cat("  Day 0 should be in hospital, so these are only valid if the patient\n")
  cat("  died on day 0, where dead takes precedence.\n")
  
  cat("\ndd > 0 but no date of death:\n")
  print(analysis_dt[dd > 0 & is.na(DOD), .(index_event_id, dd)])
  
  cat("\nDeath recorded but mort.90.day is FALSE and DOD within window:\n")
  print(analysis_dt[!is.na(DOD) & mort.90.day == FALSE &
                      as.IDate(DOD) <= as.IDate(daoh_period_end),
                    .(index_event_id, DOD, daoh_period_end, mort.90.day)])
  
  invisible(NULL)
}


#' A stratified sample of index events to hand-check
#'
#' Picks the interesting cases rather than a uniform sample: the extremes and
#' the boundaries are where convention errors show up.
daoh_check_sample <- function(n_each = 2L) {
  unique(c(
    analysis_dt[daoh == 0][seq_len(min(n_each, .N)), index_event_id],
    analysis_dt[daoh == max(daoh)][seq_len(min(n_each, .N)), index_event_id],
    analysis_dt[!is.na(DOD) & dd > 0][seq_len(min(n_each, .N)), index_event_id],
    analysis_dt[is.na(DOD) & daoh > 0 & dih > 1][
      seq_len(min(n_each, .N)), index_event_id],
    analysis_dt[dih == 0][seq_len(min(n_each, .N)), index_event_id]
  ))
}