#' Detect flatline segments in a wearable time series
#'
#' @param x A `wa_ts` tibble.
#' @param tol Numeric tolerance used when comparing consecutive signal values.
#' @param min_len Minimum duration (character string understood by
#'   [base::as.difftime()]) that a flat segment must span to be reported.
#'
#' @return A tibble with columns `id`, `start`, `end`, `duration`, `type`,
#'   `value`, and `strength`. A zero-row tibble is returned when no flatlines are
#'   detected.
#' @export
#'
#' @examples
#' data <- toy_cgm(n_id = 1, n = 12)
#' ts <- as_wearable_ts(data, id = id, time = time, value = value)
#' ts$value[4:8] <- 125
#' detect_flatlines(ts, tol = 1e-5, min_len = "15 min")
detect_flatlines <- function(x, tol = 1e-6, min_len = "15 min") {
  if (!inherits(x, "wa_ts")) {
    stop("`detect_flatlines()` expects a `wa_ts` object.", call. = FALSE)
  }
  min_secs <- .parse_duration_sec(min_len)
  if (!is.finite(min_secs) || min_secs <= 0) {
    stop("`min_len` must be a positive time specification.", call. = FALSE)
  }
  if (!is.numeric(tol) || length(tol) != 1L || is.na(tol) || tol < 0) {
    stop("`tol` must be a non-negative numeric scalar.", call. = FALSE)
  }

  tz <- attr(x, "tz") %||% "UTC"
  empty <- .empty_segment_tbl(tz)

  if (nrow(x) == 0) {
    return(empty)
  }

  split_idx <- split(seq_len(nrow(x)), x$id)
  segments <- lapply(names(split_idx), function(id_val) {
    idx <- split_idx[[id_val]]
    values <- x$value[idx]
    times <- x$time[idx]
    segs <- .flatline_segments(values, times, tol, min_secs)
    if (nrow(segs) == 0) {
      return(segs)
    }
    segs$id <- rep(as.character(x$id[idx][1]), nrow(segs))
    segs$..start_idx <- NULL
    segs$..end_idx <- NULL
    segs
  })

  out <- dplyr::bind_rows(segments)
  if (nrow(out) == 0) {
    return(empty)
  }
  out$duration <- .secs_to_difftime(out$duration)
  dplyr::arrange(out, id, start)
}

#' Detect saturation segments in a wearable time series
#'
#' @inheritParams detect_flatlines
#' @param lower,upper Numeric bounds defining saturation thresholds. Defaults to
#'   the metadata carried by the `wa_ts` object.
#' @param min_len Minimum duration (character string) that a saturation run must
#'   span to be reported.
#'
#' @return A tibble structured like [detect_flatlines()] with type values
#'   `"saturation_high"` or `"saturation_low"`.
#' @export
#'
#' @examples
#' data <- toy_cgm(n_id = 1, n = 12)
#' ts <- as_wearable_ts(data, id = id, time = time, value = value)
#' ts$value[5:9] <- 405
#' detect_saturation(ts, min_len = "10 min")
detect_saturation <- function(x, lower = attr(x, "lower"), upper = attr(x, "upper"),
                              min_len = "10 min") {
  if (!inherits(x, "wa_ts")) {
    stop("`detect_saturation()` expects a `wa_ts` object.", call. = FALSE)
  }
  min_secs <- .parse_duration_sec(min_len)
  if (!is.finite(min_secs) || min_secs <= 0) {
    stop("`min_len` must be a positive time specification.", call. = FALSE)
  }

  tz <- attr(x, "tz") %||% "UTC"
  empty <- .empty_segment_tbl(tz)

  if (nrow(x) == 0) {
    return(empty)
  }

  lower_bound <- if (is.null(lower) || all(is.na(lower))) -Inf else lower
  upper_bound <- if (is.null(upper) || all(is.na(upper))) Inf else upper

  split_idx <- split(seq_len(nrow(x)), x$id)
  segments <- lapply(names(split_idx), function(id_val) {
    idx <- split_idx[[id_val]]
    values <- x$value[idx]
    times <- x$time[idx]
    results <- list()

    if (is.finite(upper_bound)) {
      mask_high <- !is.na(values) & values >= upper_bound
      results[[length(results) + 1]] <- .mask_to_segments(mask_high, times, values,
                                                          min_secs, "saturation_high")
    }
    if (is.finite(lower_bound)) {
      mask_low <- !is.na(values) & values <= lower_bound
      results[[length(results) + 1]] <- .mask_to_segments(mask_low, times, values,
                                                          min_secs, "saturation_low")
    }
    segs <- dplyr::bind_rows(results)
    if (nrow(segs) == 0) {
      return(segs)
    }
    segs$id <- rep(as.character(x$id[idx][1]), nrow(segs))
    segs$..start_idx <- NULL
    segs$..end_idx <- NULL
    segs
  })

  out <- dplyr::bind_rows(segments)
  if (nrow(out) == 0) {
    return(empty)
  }
  out$duration <- .secs_to_difftime(out$duration)
  dplyr::arrange(out, id, start)
}

#' Detect rate-of-change anomalies in a wearable time series
#'
#' @inheritParams detect_flatlines
#' @param window Character string describing the maximum separation between
#'   points used to compute rate-of-change.
#' @param threshold Numeric threshold applied to the scaled rate-of-change.
#' @param scale Scaling strategy: MAD, standard deviation, or "none".
#'
#' @return A tibble like [detect_flatlines()] with `type` values `"roc_up"` or
#'   `"roc_down"` and a `strength` column containing the maximum scaled
#'   deviation within each segment.
#' @export
#'
#' @examples
#' data <- toy_cgm(n_id = 1, n = 12)
#' ts <- as_wearable_ts(data, id = id, time = time, value = value)
#' ts$value[6] <- ts$value[5] + 60
#' detect_rate_change(ts, window = "15 min", threshold = 3)
detect_rate_change <- function(x, window = "15 min", threshold = 3,
                               scale = c("mad", "sd", "none")) {
  if (!inherits(x, "wa_ts")) {
    stop("`detect_rate_change()` expects a `wa_ts` object.", call. = FALSE)
  }
  scale <- match.arg(scale)
  window_secs <- .parse_duration_sec(window)
  if (!is.finite(window_secs) || window_secs <= 0) {
    stop("`window` must be a positive time specification.", call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1L || !is.finite(threshold) || threshold <= 0) {
    stop("`threshold` must be a positive numeric scalar.", call. = FALSE)
  }

  tz <- attr(x, "tz") %||% "UTC"
  empty <- .empty_segment_tbl(tz)

  if (nrow(x) < 2) {
    return(empty)
  }

  split_idx <- split(seq_len(nrow(x)), x$id)
  segments <- lapply(names(split_idx), function(id_val) {
    idx <- split_idx[[id_val]]
    values <- x$value[idx]
    times <- x$time[idx]

    if (length(values) < 2) {
      return(.empty_segment_tbl(tz))
    }

    dt_sec <- as.numeric(diff(times), units = "secs")
    rates <- rep(NA_real_, length(dt_sec))
    valid_gap <- dt_sec > 0 & dt_sec <= (window_secs + 60)
    rates[valid_gap] <- diff(values)[valid_gap] / (dt_sec[valid_gap] / 60)

    if (all(is.na(rates))) {
      return(.empty_segment_tbl(tz))
    }

    scale_val <- switch(scale,
      mad = stats::mad(rates, na.rm = TRUE, constant = 1),
      sd = stats::sd(rates, na.rm = TRUE),
      none = 1
    )
    if (!is.finite(scale_val) || scale_val == 0) {
      scale_val <- 1
    }

    scores <- rates / scale_val
    up_mask <- !is.na(scores) & scores >= threshold
    down_mask <- !is.na(scores) & scores <= -threshold

    up_segments <- .rate_segments(up_mask, scores, values, times, "roc_up")
    down_segments <- .rate_segments(down_mask, scores, values, times, "roc_down")

    segs <- dplyr::bind_rows(up_segments, down_segments)
    if (nrow(segs) == 0) {
      return(segs)
    }
    segs$id <- rep(as.character(x$id[idx][1]), nrow(segs))
    segs$..start_idx <- NULL
    segs$..end_idx <- NULL
    segs
  })

  out <- dplyr::bind_rows(segments)
  if (nrow(out) == 0) {
    return(empty)
  }
  out$duration <- .secs_to_difftime(out$duration)
  dplyr::arrange(out, id, start)
}

.empty_segment_tbl <- function(tz) {
  dplyr::tibble(
    id = character(),
    start = as.POSIXct(numeric(0), origin = "1970-01-01", tz = tz),
    end = as.POSIXct(numeric(0), origin = "1970-01-01", tz = tz),
    duration = as.difftime(numeric(0), units = "secs"),
    type = character(),
    value = numeric(),
    strength = numeric()
  )
}

.flatline_segments <- function(values, times, tol, min_secs) {
  n <- length(values)
  if (n == 0) {
    return(dplyr::tibble())
  }
  run_id <- integer(n)
  run_id[1] <- 1
  valid <- !is.na(values)
  for (i in seq.int(2, n)) {
    contiguous <- valid[i] && valid[i - 1] && abs(values[i] - values[i - 1]) <= tol
    run_id[i] <- if (contiguous) run_id[i - 1] else run_id[i - 1] + 1
  }

  indices <- split(seq_len(n), run_id)
  out <- lapply(indices, function(idx) {
    if (!all(valid[idx])) {
      return(NULL)
    }
    start_time <- times[idx[1]]
    end_time <- times[idx[length(idx)]]
    duration_sec <- as.numeric(difftime(end_time, start_time, units = "secs"))
    if (duration_sec < min_secs) {
      return(NULL)
    }
    dplyr::tibble(
      ..start_idx = idx[1],
      ..end_idx = idx[length(idx)],
      start = start_time,
      end = end_time,
      duration = duration_sec,
      type = "flatline",
      value = mean(values[idx], na.rm = TRUE),
      strength = length(idx)
    )
  })
  dplyr::bind_rows(out)
}

.mask_to_segments <- function(mask, times, values, min_secs, type) {
  if (!any(mask, na.rm = TRUE)) {
    return(dplyr::tibble())
  }
  mask[is.na(mask)] <- FALSE
  r <- rle(mask)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  keep <- which(r$values)
  if (length(keep) == 0) {
    return(dplyr::tibble())
  }
  segments <- lapply(keep, function(i) {
    idx <- seq.int(starts[i], ends[i])
    start_time <- times[idx[1]]
    end_time <- times[idx[length(idx)]]
    duration_sec <- as.numeric(difftime(end_time, start_time, units = "secs"))
    if (duration_sec < min_secs) {
      return(NULL)
    }
    dplyr::tibble(
      ..start_idx = idx[1],
      ..end_idx = idx[length(idx)],
      start = start_time,
      end = end_time,
      duration = duration_sec,
      type = type,
      value = mean(values[idx], na.rm = TRUE),
      strength = length(idx)
    )
  })
  dplyr::bind_rows(segments)
}

.rate_segments <- function(mask, scores, values, times, type) {
  if (!any(mask, na.rm = TRUE)) {
    return(dplyr::tibble())
  }
  mask[is.na(mask)] <- FALSE
  r <- rle(mask)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  keep <- which(r$values)
  if (length(keep) == 0) {
    return(dplyr::tibble())
  }
  segments <- lapply(keep, function(i) {
    idx <- seq.int(starts[i], ends[i])
    start_idx <- idx[1]
    end_idx <- idx[length(idx)] + 1
    start_time <- times[start_idx]
    end_time <- times[end_idx]
    duration_sec <- as.numeric(difftime(end_time, start_time, units = "secs"))
    dplyr::tibble(
      ..start_idx = start_idx,
      ..end_idx = end_idx,
      start = start_time,
      end = end_time,
      duration = duration_sec,
      type = type,
      value = mean(values[start_idx:end_idx], na.rm = TRUE),
      strength = max(abs(scores[idx]), na.rm = TRUE)
    )
  })
  dplyr::bind_rows(segments)
}

.secs_to_difftime <- function(secs) {
  as.difftime(secs, units = "secs")
}
