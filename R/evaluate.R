#' Evaluate predicted anomaly segments
#'
#' Computes precision, recall, F1, and intersection-over-union (IoU) for
#' predicted vs. reference segments given a time tolerance.
#'
#' @param pred,truth Data frames/tibbles with columns `id`, `start`, and `end`.
#' @param tolerance Allowed temporal slack interpreted by [base::as.difftime()].
#'
#' @return A named list with entries `precision`, `recall`, `f1`, and `iou`.
#' @export
evaluate_methods <- function(pred, truth, tolerance = "10 min") {
  pred <- .standardise_eval_tbl(pred, "pred")
  truth <- .standardise_eval_tbl(truth, "truth")

  tol_sec <- .parse_duration_sec(tolerance)
  if (!is.finite(tol_sec)) {
    stop("`tolerance` must be convertible to seconds.", call. = FALSE)
  }

  if (nrow(pred) == 0L && nrow(truth) == 0L) {
    return(list(precision = 1, recall = 1, f1 = 1, iou = 1))
  }

  matches <- .match_segments(pred, truth, tol_sec)
  precision <- if (nrow(pred) == 0L) if (nrow(truth) == 0L) 1 else 0 else matches$matched_preds / nrow(pred)
  recall <- if (nrow(truth) == 0L) if (nrow(pred) == 0L) 1 else 0 else matches$matched_truth / nrow(truth)
  f1 <- if ((precision + recall) == 0) 0 else (2 * precision * recall) / (precision + recall)
  union_secs <- matches$pred_secs + matches$truth_secs - matches$intersection
  iou <- if (union_secs <= 0) 0 else matches$intersection / union_secs
  list(precision = precision, recall = recall, f1 = f1, iou = iou)
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
    }
  }
  list(
    matched_preds = sum(matched_pred_idx),
    matched_truth = sum(matched_truth_idx),
    pred_secs = .total_duration(pred),
    truth_secs = .total_duration(truth),
    intersection = intersection_total
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
