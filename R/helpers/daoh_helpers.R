daoh_stats <- function(x) {
  x <- x[!is.na(x)]
  q <- quantile(x, c(0.1, 0.25, 0.5, 0.75, 0.9), type = 8, names = FALSE)
  c(mean = mean(x), sd = sd(x), sem = sd(x) / sqrt(length(x)),
    q10 = q[1], q25 = q[2], median = q[3], q75 = q[4], q90 = q[5])
}

boot_daoh <- function(x, R = 10000, conf = 0.95) {
  x <- x[!is.na(x)]
  
  out_template <- data.table::data.table(
    statistic = character(), n = integer(), estimate = numeric(),
    lower = numeric(), upper = numeric(), estimable = logical()
  )
  
  # groupingsets evaluates j once on a zero-row table to build a
  # column template; return the empty structure rather than erroring
  if (length(x) == 0L) return(out_template)
  
  if (length(x) < 20L)
    stop("boot_daoh: n = ", length(x), " after NA removal")
  
  bo <- boot::boot(x, \(d, i) daoh_stats(d[i]), R = R)
  
  data.table::rbindlist(lapply(seq_along(bo$t0), function(j) {
    est <- bo$t0[j]
    
    # BCa bias correction is qnorm(p); infinite when p is 0 or 1
    p <- mean(bo$t[, j] < est)
    if (p <= 0 || p >= 1)
      return(data.table::data.table(
        statistic = names(bo$t0)[j], n = length(x), estimate = est,
        lower = NA_real_, upper = NA_real_, estimable = FALSE))
    
    ci <- boot::boot.ci(bo, index = j, type = "bca", conf = conf)
    data.table::data.table(
      statistic = names(bo$t0)[j], n = length(x), estimate = est,
      lower = ci$bca[4], upper = ci$bca[5], estimable = TRUE)
  }))
}