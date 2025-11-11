if (interactive()) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
  })

  old_opts <- options(c("wearableAnomaly.use_rcpp", "wearableAnomaly.rcpp_threshold"))
  on.exit(do.call(options, old_opts), add = TRUE)

  set.seed(2025)
  n <- 50000
  cadence <- "5 min"
  times <- seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"), by = cadence, length.out = n)
  values <- c(
    rnorm(20000, mean = 100, sd = 4),
    rnorm(15000, mean = 120, sd = 4),
    rnorm(15000, mean = 105, sd = 4)
  )
  data <- tibble(id = "id01", time = times, value = values)
  ts <- as_wearable_ts(data, id = id, time = time, value = value)

  options(wearableAnomaly.use_rcpp = FALSE, wearableAnomaly.rcpp_threshold = Inf)
  message("Benchmarking detect_changepoints_pelt (R implementation)...")
  elapsed_r <- numeric(3)
  res_r <- NULL
  for (i in seq_along(elapsed_r)) {
    gc()
    elapsed_r[i] <- system.time({
      res_r <- detect_changepoints_pelt(ts, cost = "meanvar", penalty = "MBIC", min_seg_len = 12)
    })[["elapsed"]]
  }
  message(sprintf("R implementation: %.2f ± %.2f seconds (n = %d, cps = %d)",
                  mean(elapsed_r), stats::sd(elapsed_r), n, nrow(res_r)))

  cpp_checker <- try(getFromNamespace(".pelt_cpp_available", "wearableAnomaly"), silent = TRUE)
  cpp_available <- !inherits(cpp_checker, "try-error") && isTRUE(cpp_checker())

  if (cpp_available) {
    message("Benchmarking detect_changepoints_pelt (Rcpp implementation)...")
    options(wearableAnomaly.use_rcpp = TRUE, wearableAnomaly.rcpp_threshold = 0L)
    elapsed_cpp <- numeric(3)
    res_cpp <- NULL
    for (i in seq_along(elapsed_cpp)) {
      gc()
      elapsed_cpp[i] <- system.time({
        res_cpp <- detect_changepoints_pelt(ts, cost = "meanvar", penalty = "MBIC", min_seg_len = 12)
      })[["elapsed"]]
    }
    speedup <- mean(elapsed_r) / mean(elapsed_cpp)
    message(sprintf("Rcpp implementation: %.2f ± %.2f seconds (speedup %.1fx, cps = %d)",
                    mean(elapsed_cpp), stats::sd(elapsed_cpp), speedup, nrow(res_cpp)))
  } else {
    message("Rcpp core not available; skipping PELT(Rcpp) benchmark.")
  }

  if (requireNamespace("changepoint", quietly = TRUE)) {
    message("Benchmarking changepoint::cpt.meanvar...")
    elapsed_cp <- numeric(3)
    res_cp <- NULL
    for (i in seq_along(elapsed_cp)) {
      gc()
      elapsed_cp[i] <- system.time({
        fit_cp <- changepoint::cpt.meanvar(values, method = "PELT", penalty = "MBIC", minseglen = 12)
        res_cp <- changepoint::cpts(fit_cp)
      })[["elapsed"]]
    }
    message(sprintf("changepoint::cpt.meanvar: %.2f ± %.2f seconds (cps = %d)",
                    mean(elapsed_cp), stats::sd(elapsed_cp), length(res_cp)))
  } else {
    message("Package 'changepoint' not installed; skipping comparison benchmark.")
  }

  if (!is.null(res_r) && nrow(res_r) > 0) {
    cp_lines <- res_r |> mutate(label = sprintf("CP (%s)", penalty))
    plot_data <- data |> mutate(index = seq_len(n))

    p <- ggplot(plot_data, aes(x = time, y = value)) +
      geom_line(alpha = 0.6) +
      geom_vline(data = cp_lines, aes(xintercept = cp_time), colour = "red", linetype = "dashed") +
      labs(title = "PELT changepoints", subtitle = "detect_changepoints_pelt vs signal",
           x = "Time", y = "Value")
    print(p)
  }

  message("Tip: set options(wearableAnomaly.use_rcpp = TRUE) (and optionally wearableAnomaly.rcpp_threshold = 0) to prioritise the Rcpp core.")
}
