#' Evaluate predicted anomaly segments
#'
#' Computes precision, recall, F1, and intersection-over-union (IoU) for
#' predicted vs. reference segments given a time tolerance.
#'
#' @param pred,truth Data frames/tibbles with columns `id`, `start`, and `end`.
#'   `pred` can also be a named list of prediction objects (see Details).
#' @param tolerance Allowed temporal slack interpreted by [base::as.difftime()].
#' @param method Optional label applied when `pred` is a single data frame. When
#'   `pred` is a list, method labels default to list names or entry metadata.
#' @param runtime_sec Optional runtime metadata (in seconds) for the supplied
#'   predictions. When `pred` is a list, runtime metadata can also be provided
#'   per entry (defaults to `NA`).
#'
#' @return A tibble with columns `method`, `runtime_sec`, `mean_n_cps`,
#'   `precision`, `recall`, `f1`, `mae_cp`, and `iou`.
#' @export
evaluate_methods <- function(pred, truth, tolerance = "10 min", method = NULL,
                             runtime_sec = NULL) {
  truth_tbl <- .standardise_eval_tbl(truth, "truth")

  tol_sec <- .parse_duration_sec(tolerance)
  if (!is.finite(tol_sec)) {
    stop("`tolerance` must be convertible to seconds.", call. = FALSE)
  }

  entries <- .as_eval_entries(pred, method, runtime_sec)
  rows <- lapply(entries, function(entry) {
    pred_tbl <- .standardise_eval_tbl(entry$segments, "pred")

    metrics <- if (nrow(pred_tbl) == 0L && nrow(truth_tbl) == 0L) {
      list(
        mean_n_cps = 0,
        precision = 1,
        recall = 1,
        f1 = 1,
        mae_cp = NA_real_,
        iou = 1
      )
    } else {
      matches <- .match_segments(pred_tbl, truth_tbl, tol_sec)
      precision <- if (nrow(pred_tbl) == 0L) if (nrow(truth_tbl) == 0L) 1 else 0 else matches$matched_preds / nrow(pred_tbl)
      recall <- if (nrow(truth_tbl) == 0L) if (nrow(pred_tbl) == 0L) 1 else 0 else matches$matched_truth / nrow(truth_tbl)
      f1 <- if ((precision + recall) == 0) 0 else (2 * precision * recall) / (precision + recall)
      union_secs <- matches$pred_secs + matches$truth_secs - matches$intersection
      iou <- if (union_secs <= 0) 0 else matches$intersection / union_secs
      mean_n_cps <- if (nrow(pred_tbl) == 0L) {
        0
      } else {
        counts <- table(pred_tbl$id)
        mean(as.numeric(counts))
      }
      mae_cp <- if (matches$error_count > 0) {
        matches$error_sum / matches$error_count
      } else {
        NA_real_
      }
      list(
        mean_n_cps = mean_n_cps,
        precision = precision,
        recall = recall,
        f1 = f1,
        mae_cp = mae_cp,
        iou = iou
      )
    }

    dplyr::tibble(
      method = entry$method,
      runtime_sec = entry$runtime_sec,
      mean_n_cps = metrics$mean_n_cps,
      precision = metrics$precision,
      recall = metrics$recall,
      f1 = metrics$f1,
      mae_cp = metrics$mae_cp,
      iou = metrics$iou
    )
  })

  dplyr::bind_rows(rows)
}

.standardise_eval_tbl <- function(x, label) {
  if (is.null(x) || nrow(x) == 0L) {
    return(dplyr::tibble(
      id = character(),
      start = as.POSIXct(numeric(0), origin = "1970-01-01", tz = "UTC"),
      end = as.POSIXct(numeric(0), origin = "1970-01-01", tz = "UTC")
    ))
  }
  if (!all(c("id", "start", "end") %in% names(x))) {
    stop(sprintf("`%s` must contain `id`, `start`, and `end` columns.", label), call. = FALSE)
  }
  tbl <- dplyr::tibble(
    id = as.character(x$id),
    start = as.POSIXct(x$start, origin = "1970-01-01", tz = attr(x$start, "tzone") %||% "UTC"),
    end = as.POSIXct(x$end, origin = "1970-01-01", tz = attr(x$end, "tzone") %||% "UTC")
  )
  tbl$end <- pmax(tbl$end, tbl$start)
  tbl
}

.match_segments <- function(pred, truth, tol_sec) {
  matched_truth_idx <- rep(FALSE, nrow(truth))
  matched_pred_idx <- rep(FALSE, nrow(pred))
  intersection_total <- 0
  abs_error_total <- 0
  abs_error_count <- 0
  for (i in seq_len(nrow(truth))) {
    truth_row <- truth[i, ]
    candidates <- which(
      pred$id == truth_row$id &
        (as.numeric(difftime(pred$start, truth_row$end, units = "secs")) <= tol_sec) &
        (as.numeric(difftime(truth_row$start, pred$end, units = "secs")) <= tol_sec)
    )
    if (length(candidates) == 0L) {
      next
    }
    best_idx <- NULL
    best_overlap <- -Inf
    for (idx in candidates) {
      overlap <- .overlap_with_tolerance(
        pred$start[idx], pred$end[idx],
        truth_row$start, truth_row$end,
        tol_sec
      )
      if (overlap > best_overlap && !matched_pred_idx[idx]) {
        best_overlap <- overlap
        best_idx <- idx
      }
    }
    if (!is.null(best_idx) && best_overlap > 0) {
      matched_truth_idx[i] <- TRUE
      matched_pred_idx[best_idx] <- TRUE
      intersection_total <- intersection_total + best_overlap
      abs_error_total <- abs_error_total + abs(as.numeric(difftime(
        pred$start[best_idx],
        truth_row$start,
        units = "mins"
      )))
      abs_error_count <- abs_error_count + 1
    }
  }
  list(
    matched_preds = sum(matched_pred_idx),
    matched_truth = sum(matched_truth_idx),
    pred_secs = .total_duration(pred),
    truth_secs = .total_duration(truth),
    intersection = intersection_total,
    error_sum = abs_error_total,
    error_count = abs_error_count
  )
}

.total_duration <- function(tbl) {
  if (nrow(tbl) == 0L) {
    return(0)
  }
  sum(as.numeric(difftime(tbl$end, tbl$start, units = "secs")), na.rm = TRUE)
}

.overlap_with_tolerance <- function(a_start, a_end, b_start, b_end, tol_sec) {
  overlap <- as.numeric(difftime(min(a_end, b_end), max(a_start, b_start), units = "secs"))
  if (is.finite(overlap) && overlap > 0) {
    return(overlap)
  }
  # Allow matches separated by gaps smaller than tolerance
  gap <- as.numeric(difftime(max(a_start, b_start), min(a_end, b_end), units = "secs"))
  gap <- abs(gap)
  if (!is.finite(gap) || gap > tol_sec) {
    return(0)
  }
  max(0, tol_sec - gap)
}

.as_eval_entries <- function(pred, method, runtime_sec) {
  build_entry <- function(data, label, runtime) {
    list(
      segments = data,
      method = label %||% "method",
      runtime_sec = .coerce_runtime(runtime)
    )
  }

  if (is.list(pred) && !inherits(pred, "data.frame")) {
    out <- list()
    for (i in seq_along(pred)) {
      entry <- pred[[i]]
      nm <- names(pred)[i]
      if (inherits(entry, "data.frame")) {
        out[[length(out) + 1L]] <- build_entry(
          entry,
          nm %||% method %||% sprintf("method_%02d", i),
          attr(entry, "runtime_sec")
        )
        next
      }
      if (is.list(entry)) {
        segs <- entry$segments %||% entry$pred %||% entry$data
        if (is.null(segs)) {
          next
        }
        out[[length(out) + 1L]] <- build_entry(
          segs,
          entry$method %||% nm %||% method %||% sprintf("method_%02d", i),
          entry$runtime_sec %||% entry$runtime
        )
      }
    }
    if (length(out) == 0L) {
      stop("No valid prediction objects supplied.", call. = FALSE)
    }
    return(out)
  }

  entry <- build_entry(
    pred,
    method %||% attr(pred, "method") %||% "method",
    runtime_sec %||% attr(pred, "runtime_sec")
  )
  list(entry)
}

.coerce_runtime <- function(value) {
  if (is.null(value) || length(value) == 0L) {
    return(NA_real_)
  }
  out <- suppressWarnings(as.numeric(value[1]))
  if (length(out) == 0L || is.na(out) || !is.finite(out)) {
    return(NA_real_)
  }
  out
}
