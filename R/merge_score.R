#' Merge heterogeneous anomaly detections
#'
#' Standardises the outputs of the artifact detectors, changepoint detectors,
#' and other anomaly routines into a common schema before merging overlapping
#' segments within each subject. The merged output includes a `components` list
#' column that records the contributing detections for downstream scoring.
#'
#' @param ... Individual detection results as data frames/tibbles or a single
#'   named list. Objects can come directly from [detect_flatlines()],
#'   [detect_saturation()], [detect_rate_change()], [detect_changepoints_pelt()],
#'   or [detect_changepoints_edivisive()].
#' @param gap Maximum gap tolerated between neighbouring detections (per id)
#'   before they are collapsed into a single segment. Can be a character string
#'   understood by [base::as.difftime()] or a numeric number of seconds.
#'
#' @return A tibble with columns `id`, `start`, `end`, `source`, `subtype`,
#'   `score`, `notes`, and `components`. The `components` column stores the
#'   underlying detections contributing to each merged segment.
#' @export
merge_segments <- function(..., gap = "5 min") {
  inputs <- list(...)
  if (length(inputs) == 1L && is.list(inputs[[1]]) && !inherits(inputs[[1]], "wa_ts")) {
    inputs <- inputs[[1]]
  }
  if (length(inputs) == 0L) {
    return(.empty_segments_tbl())
  }

  if (is.null(names(inputs)) || any(names(inputs) == "")) {
    names(inputs) <- paste0("source", seq_along(inputs))
  }

  standardized <- lapply(names(inputs), function(nm) {
    .standardize_segments(inputs[[nm]], nm)
  })
  standardized <- standardized[!vapply(standardized, is.null, logical(1))]
  if (length(standardized) == 0L) {
    return(.empty_segments_tbl())
  }
  stacked <- dplyr::bind_rows(standardized)
  if (nrow(stacked) == 0L) {
    return(.empty_segments_tbl())
  }

  gap_sec <- .parse_duration_sec(gap)
  if (!is.finite(gap_sec)) {
    stop("`gap` must be convertible to seconds.", call. = FALSE)
  }

  stacked <- stacked[order(stacked$id, stacked$start), , drop = FALSE]
  merged <- list()
  cluster <- list()
  current_end <- NULL
  current_id <- NULL

  flush_cluster <- function(cluster_rows) {
    if (length(cluster_rows) == 0L) {
      return(NULL)
    }
    cluster_tbl <- suppressWarnings(dplyr::bind_rows(cluster_rows))
    unique_sources <- unique(cluster_tbl$source)
    unique_types <- unique(cluster_tbl$subtype)
    note_vals <- cluster_tbl$notes
    note_vals <- note_vals[!is.na(note_vals) & nzchar(note_vals)]
    combined <- dplyr::tibble(
      id = cluster_tbl$id[1],
      start = min(cluster_tbl$start),
      end = max(cluster_tbl$end),
      source = paste(unique_sources, collapse = "|"),
      subtype = paste(unique_types, collapse = "|"),
      score = .cluster_score(cluster_tbl$score),
      notes = if (length(note_vals) == 0L) NA_character_ else paste(unique(note_vals), collapse = "; "),
      components = list(cluster_tbl)
    )
    combined
  }

  for (i in seq_len(nrow(stacked))) {
    row <- stacked[i, , drop = FALSE]
    if (length(cluster) == 0L) {
      cluster <- list(row)
      current_end <- row$end
      current_id <- row$id
      next
    }
    within_gap <- identical(row$id, current_id) &&
      (as.numeric(difftime(row$start, current_end, units = "secs")) <= gap_sec)
    if (within_gap) {
      cluster[[length(cluster) + 1L]] <- row
      current_end <- max(current_end, row$end)
    } else {
      merged[[length(merged) + 1L]] <- flush_cluster(cluster)
      cluster <- list(row)
      current_end <- row$end
      current_id <- row$id
    }
  }
  merged[[length(merged) + 1L]] <- flush_cluster(cluster)
  out <- suppressWarnings(dplyr::bind_rows(merged))
  out[order(out$id, out$start), , drop = FALSE]
}

.cluster_score <- function(scores) {
  scores <- as.numeric(scores)
  scores <- scores[is.finite(scores)]
  if (length(scores) == 0L) {
    return(NA_real_)
  }
  max(scores)
}

.standardize_segments <- function(df, source_name) {
  if (is.null(df)) {
    return(NULL)
  }
  if (inherits(df, "wa_ts")) {
    stop("`merge_segments()` expects detector outputs, not `wa_ts` objects.", call. = FALSE)
  }
  df <- dplyr::as_tibble(df)
  if (nrow(df) == 0L) {
    return(NULL)
  }
  if (!("id" %in% names(df))) {
    stop("Every detector output must include an `id` column.", call. = FALSE)
  }

  col_get <- function(col) {
    if (col %in% names(df)) df[[col]] else NULL
  }

  start <- col_get("start") %||% col_get("time") %||% col_get("cp_time")
  end <- col_get("end") %||% col_get("cp_time") %||% col_get("time") %||% start
  if (is.null(start)) {
    stop(sprintf("Could not determine start times for source '%s'.", source_name), call. = FALSE)
  }
  start <- as.POSIXct(start, origin = "1970-01-01", tz = attr(start, "tzone") %||% "UTC")
  end <- as.POSIXct(end, origin = "1970-01-01", tz = attr(start, "tzone") %||% "UTC")

  subtype <- col_get("type") %||% col_get("method") %||% col_get("penalty") %||% source_name
  subtype <- as.character(subtype)

  score <- col_get("score")
  if (is.null(score)) {
    score <- col_get("strength")
  }
  if (is.null(score) && "p_value" %in% names(df)) {
    score <- 1 - suppressWarnings(as.numeric(col_get("p_value")))
  }
  if (is.null(score) && "new_level" %in% names(df)) {
    score <- abs(col_get("new_level"))
  }
  if (is.null(score)) {
    score <- rep(NA_real_, nrow(df))
  }
  score <- as.numeric(score)

  notes <- col_get("details") %||% col_get("notes") %||% col_get("penalty")
  notes <- if (is.null(notes)) rep(NA_character_, nrow(df)) else as.character(notes)

  out <- dplyr::tibble(
    id = as.character(df$id),
    start = start,
    end = end,
    source = source_name,
    subtype = subtype,
    score = score,
    notes = notes
  )
  out <- out[!is.na(out$start) & !is.na(out$id), , drop = FALSE]
  if (nrow(out) == 0L) {
    return(NULL)
  }
  out$end <- pmax(out$end, out$start)
  out
}

.empty_segments_tbl <- function() {
  dplyr::tibble(
    id = character(),
    start = as.POSIXct(numeric(0), origin = "1970-01-01", tz = "UTC"),
    end = as.POSIXct(numeric(0), origin = "1970-01-01", tz = "UTC"),
    source = character(),
    subtype = character(),
    score = numeric(),
    notes = character(),
    components = list()
  )
}

#' Score merged anomaly segments
#'
#' Normalises scores within each detector source and combines them across
#' sources using the requested strategy.
#'
#' @param segments Output from [merge_segments()].
#' @param weights Optional named numeric vector giving source-specific weights.
#'   Unspecified sources default to 1.
#' @param combine Combination strategy: `"max"` (default), `"sum"`, or `"vote"`.
#'
#' @return The input tibble with the `score` column updated to reflect the
#'   combined score. A column `raw_score` is added to retain the pre-combined
#'   values.
#' @export
score_anomalies <- function(segments, weights = NULL, combine = c("max", "sum", "vote")) {
  combine <- match.arg(combine)
  if (nrow(segments) == 0L) {
    return(segments)
  }
  seg <- dplyr::as_tibble(segments)
  seg$raw_score <- seg$score
  seg$.segment_id <- seq_len(nrow(seg))
  comps <- if ("components" %in% names(seg)) seg$components else vector("list", nrow(seg))
  if (all(vapply(comps, is.null, logical(1)))) {
    comps <- lapply(seq_len(nrow(seg)), function(i) {
      dplyr::tibble(
        id = seg$id[i],
        start = seg$start[i],
        end = seg$end[i],
        source = seg$source[i],
        subtype = seg$subtype[i],
        score = seg$raw_score[i],
        notes = seg$notes[i]
      )
    })
  }
  comp_tbl <- lapply(seq_len(nrow(seg)), function(i) {
    comp <- comps[[i]]
    if (is.null(comp) || nrow(comp) == 0L) {
      comp <- dplyr::tibble(
        id = seg$id[i],
        start = seg$start[i],
        end = seg$end[i],
        source = seg$source[i],
        subtype = seg$subtype[i],
        score = seg$raw_score[i],
        notes = seg$notes[i]
      )
    }
    comp$.segment_id <- seg$.segment_id[i]
    comp
  })
  comp_tbl <- dplyr::bind_rows(comp_tbl)
  comp_tbl$score <- as.numeric(comp_tbl$score)
  norm_scores <- .normalise_scores(comp_tbl, weights)

  combined <- switch(
    combine,
    max = {
      tmp <- dplyr::group_by(norm_scores, .segment_id)
      tmp <- dplyr::summarise(tmp, score = suppressWarnings(max(norm_score, na.rm = TRUE)), .groups = "drop")
      tmp
    },
    sum = {
      tmp <- dplyr::group_by(norm_scores, .segment_id)
      tmp <- dplyr::summarise(tmp, score = sum(norm_score, na.rm = TRUE), .groups = "drop")
      tmp
    },
    vote = {
      tmp <- dplyr::group_by(norm_scores, .segment_id)
      tmp <- dplyr::summarise(tmp, score = mean(norm_score > 0, na.rm = TRUE), .groups = "drop")
      tmp
    }
  )

  combined$score[!is.finite(combined$score)] <- 0
  seg <- dplyr::left_join(seg, combined, by = ".segment_id", suffix = c("", ".combined"))
  seg$score <- seg$score.combined
  seg$score.combined <- NULL
  seg$.segment_id <- NULL
  seg
}

.normalise_scores <- function(tbl, weights) {
  tbl$norm_score <- tbl$score
  unique_sources <- unique(tbl$source)
  for (src in unique_sources) {
    mask <- tbl$source == src
    vals <- tbl$score[mask]
    if (all(is.na(vals))) {
      tbl$norm_score[mask] <- 0
      next
    }
    rng <- range(vals, na.rm = TRUE)
    if (!is.finite(rng[1]) || !is.finite(rng[2]) || (rng[2] - rng[1]) < .Machine$double.eps) {
      tbl$norm_score[mask] <- 1
    } else {
      tbl$norm_score[mask] <- (vals - rng[1]) / (rng[2] - rng[1])
    }
  }
  if (!is.null(weights)) {
    weights <- weights[!is.na(names(weights))]
    if (length(weights) > 0L) {
      src_weights <- weights[tbl$source]
      src_weights[is.na(src_weights)] <- 1
      tbl$norm_score <- tbl$norm_score * src_weights
    }
  }
  tbl
}
