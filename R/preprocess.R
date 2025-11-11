#' Resample a wearable time series onto an even cadence
#'
#' @param x A `wa_ts` tibble produced by [as_wearable_ts()].
#' @param by Character string understood by [base::seq.POSIXt()] that defines the
#'   target sampling cadence (default "5 min").
#' @param agg Aggregation strategy applied when multiple observations fall into
#'   the same resampling window. Choose from "mean", "last", or "median".
#'
#' @return A `wa_ts` tibble sampled on the requested cadence. Metadata attributes
#'   `lower`, `upper`, `cadence`, `tz`, and `dups_dropped` are preserved, with
#'   `cadence` updated to the new cadence in seconds.
#' @export
#'
#' @examples
#' data <- toy_cgm(n_id = 1, n = 8, by = "7 min")
#' ts <- as_wearable_ts(data, id = id, time = time, value = value)
#' resampled <- resample_series(ts, by = "5 min", agg = "mean")
resample_series <- function(x, by = "5 min", agg = c("mean", "last", "median")) {
  if (!inherits(x, "wa_ts")) {
    stop("`resample_series()` expects a `wa_ts` object.", call. = FALSE)
  }

  agg <- match.arg(agg)
  cadence_sec <- .parse_duration_sec(by)
  if (!is.finite(cadence_sec) || cadence_sec <= 0) {
    stop("`by` must be a positive time specification understood by `seq.POSIXt()`.",
         call. = FALSE)
  }

  if (nrow(x) == 0) {
    out <- x
    attr(out, "cadence") <- cadence_sec
    return(out)
  }

  tz <- attr(x, "tz")
  if (is.null(tz)) {
    tz <- "UTC"
  }

  split_idx <- split(seq_len(nrow(x)), x$id)
  resampled_list <- lapply(names(split_idx), function(id_val) {
    idx <- split_idx[[id_val]]
    data_id <- x[idx, , drop = FALSE]
    data_id <- data_id[order(data_id$time), , drop = FALSE]

    start_time <- min(data_id$time)
    end_time <- max(data_id$time)
    span_sec <- as.numeric(difftime(end_time, start_time, units = "secs"))
    n_bins <- if (span_sec <= 0) 0 else floor(span_sec / cadence_sec)
    if ((start_time + n_bins * cadence_sec) < end_time) {
      n_bins <- n_bins + 1
    }
    grid_bins <- seq.int(0, n_bins)

    grid <- dplyr::tibble(
      id = rep(as.character(data_id$id[1]), length(grid_bins)),
      bin = grid_bins,
      time = start_time + grid_bins * cadence_sec
    )
    attr(grid$time, "tzone") <- tz

    bin_index <- floor(as.numeric(difftime(data_id$time, start_time, units = "secs")) / cadence_sec)
    data_id$bin <- bin_index

    summary_tbl <- dplyr::group_by(data_id, bin)
    summary_tbl <- dplyr::summarise(
      summary_tbl,
      value = .aggregate_bin(value, time, agg),
      .groups = "drop"
    )

    out <- dplyr::left_join(grid, summary_tbl, by = "bin")
    out <- dplyr::select(out, id, time, value)
    out
  })

  combined <- dplyr::bind_rows(resampled_list)
  combined <- dplyr::arrange(combined, time, id)
  combined_tbl <- dplyr::as_tibble(combined)

  attributes <- attributes(x)
  attr(combined_tbl, "class") <- attr(x, "class")
  attr(combined_tbl, "lower") <- attributes$lower
  attr(combined_tbl, "upper") <- attributes$upper
  attr(combined_tbl, "cadence") <- cadence_sec
  attr(combined_tbl, "tz") <- tz
  attr(combined_tbl, "dups_dropped") <- attributes$dups_dropped %||% 0L

  combined_tbl
}

.aggregate_bin <- function(values, times, agg) {
  valid <- !is.na(values)
  if (!any(valid)) {
    return(NA_real_)
  }
  values <- values[valid]
  times <- times[valid]
  switch(agg,
    mean = mean(values),
    median = stats::median(values),
    last = {
      idx <- which.max(as.numeric(times))
      values[idx]
    }
  )
}

.parse_duration_sec <- function(x) {
  secs <- suppressWarnings(as.numeric(as.difftime(x, units = "secs")))
  if (is.na(secs)) {
    anchor <- as.POSIXct("1970-01-01", tz = "UTC")
    trial <- try(suppressWarnings(seq(from = anchor, by = x, length.out = 2)), silent = TRUE)
    if (inherits(trial, "try-error")) {
      return(NA_real_)
    }
    secs <- as.numeric(difftime(trial[2], trial[1], units = "secs"))
  }
  secs
}
