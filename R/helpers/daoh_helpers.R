daoh_stats <- function(x) {
  x <- x[!is.na(x)]
  q <- quantile(x, c(0.1, 0.25, 0.5, 0.75, 0.9), type = 8, names = FALSE)
  c(mean = mean(x), sd = sd(x), sem = sd(x) / sqrt(length(x)),
    q10 = q[1], q25 = q[2], median = q[3], q75 = q[4], q90 = q[5])
}

boot_daoh <- function(x, R = 10000, conf = 0.95, min_n = 20L) {
  x <- x[!is.na(x)]
  
  out_template <- data.table::data.table(
    statistic = character(), n = integer(), estimate = numeric(),
    lower = numeric(), upper = numeric(), estimable = logical()
  )
  
  if (length(x) == 0L) return(out_template)
  
  # Too few to bootstrap: report point estimates, no intervals
  if (length(x) < min_n) {
    est <- daoh_stats(x)
    return(data.table::data.table(
      statistic = names(est), n = length(x), estimate = as.numeric(est),
      lower = NA_real_, upper = NA_real_, estimable = FALSE))
  }
  
  bo <- boot::boot(x, \(d, i) daoh_stats(d[i]), R = R)
  
  data.table::rbindlist(lapply(seq_along(bo$t0), function(j) {
    est <- bo$t0[j]
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