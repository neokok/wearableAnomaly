#' High-level anomaly detection pipeline
#'
#' Runs the validation, optional resampling, artifact detectors, changepoint
#' detectors, and scoring stages in a single convenience wrapper.
#'
#' @param x A `wa_ts` object or data frame coerced via [as_wearable_ts()].
#' @param preset Pipeline preset controlling weighting and combination strategy.
#'   `"research"` emphasises sensitivity via score summation whereas `"clinical"`
#'   emphasises specificity via max pooling.
#' @param resample_by Optional cadence passed to [resample_series()]. Set to
#'   `NULL` to skip resampling.
#' @param ... Additional arguments forwarded to [merge_segments()] or
#'   [score_anomalies()] when the argument names match those functions.
#'
#' @return A list containing merged `segments`, raw `changepoints`, raw
#'   `artifacts`, validation `issues`, and (placeholder) `plots`.
#' @export
detect_anomalies <- function(x,
                             preset = c("research", "clinical"),
                             resample_by = "5 min",
                             ...) {
  preset <- match.arg(preset)
  dots <- list(...)

  if (!inherits(x, "wa_ts")) {
    stop("`detect_anomalies()` expects an object produced by `as_wearable_ts()`.", call. = FALSE)
  }

  issues <- validate_ts(x)
  ts <- x
  if (!is.null(resample_by)) {
    ts <- resample_series(ts, by = resample_by)
  }

  artifacts <- list(
    flatlines = detect_flatlines(ts),
    saturation = detect_saturation(ts),
    rate_change = detect_rate_change(ts)
  )
  ts_complete <- .drop_na_ts(ts)
  changepoints_pelt <- detect_changepoints_pelt(ts_complete)
  changepoints_ediv <- detect_changepoints_edivisive(ts_complete)

  merge_formals <- names(formals(merge_segments))
  merge_args <- dots[intersect(names(dots), merge_formals)]
  segments <- do.call(
    merge_segments,
    c(
      list(
        flatlines = artifacts$flatlines,
        saturation = artifacts$saturation,
        rate = artifacts$rate_change,
        pelt = changepoints_pelt,
        edivisive = changepoints_ediv
      ),
      merge_args
    )
  )

  preset_config <- switch(
    preset,
    research = list(
      weights = c(flatlines = 1, saturation = 0.8, rate = 0.8, pelt = 1, edivisive = 1.2),
      combine = "sum"
    ),
    clinical = list(
      weights = c(flatlines = 0.5, saturation = 1.2, rate = 1, pelt = 1.4, edivisive = 1.4),
      combine = "max"
    )
  )

  score_formals <- setdiff(names(formals(score_anomalies)), "segments")
  score_args <- dots[intersect(names(dots), score_formals)]
  combine_arg <- preset_config$combine
  if ("combine" %in% names(score_args)) {
    combine_arg <- score_args$combine
    score_args$combine <- NULL
  }
  weights_arg <- preset_config$weights
  if ("weights" %in% names(score_args)) {
    weights_arg <- score_args$weights
    score_args$weights <- NULL
  }

  segments <- do.call(
    score_anomalies,
    c(
      list(
        segments = segments,
        weights = weights_arg,
        combine = combine_arg
      ),
      score_args
    )
  )

  list(
    segments = segments,
    changepoints = list(pelt = changepoints_pelt, edivisive = changepoints_ediv),
    artifacts = artifacts,
    issues = issues,
    plots = list()
  )
}

.drop_na_ts <- function(ts) {
  mask <- !is.na(ts$value)
  if (all(mask) || sum(mask) < 2L) {
    return(ts)
  }
  subset <- ts[mask, , drop = FALSE]
  class(subset) <- class(ts)
  attrs_to_copy <- c("lower", "upper", "cadence", "tz", "dups_dropped")
  for (nm in attrs_to_copy) {
    attr(subset, nm) <- attr(ts, nm)
  }
  subset
}
