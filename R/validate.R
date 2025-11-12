#' Validate a wearable-anomaly time series object
#'
#' @param x A `wa_ts` object created by [as_wearable_ts()].
#' @param lower,upper Numeric bounds used to flag out-of-range values. Defaults
#'   to the attributes stored on `x`.
#'
#' @return A tibble recording detected issues with columns `issue`, `id`,
#'   `time`, `value`, and `details`. A zero-row tibble is returned when no
#'   issues are found.
#' @export
#'
#' @examples
#' data <- toy_cgm(n_id = 1, n = 6)
#' ts <- as_wearable_ts(data, id = id, time = time, value = value)
#' validate_ts(ts)
validate_ts <- function(x, lower = attr(x, "lower"), upper = attr(x, "upper")) {
	if (!inherits(x, "wa_ts")) {
		stop("`validate_ts()` expects an object created by `as_wearable_ts()`.",
				 call. = FALSE)
	}

	tz <- attr(x, "tz")
	if (is.null(tz)) {
		tz <- "UTC"
	}

	empty_time <- as.POSIXct(numeric(0), origin = "1970-01-01", tz = tz)
	issues <- dplyr::as_tibble(data.frame(
		issue = character(),
		id = character(),
		time = empty_time,
		value = numeric(),
		details = character(),
		stringsAsFactors = FALSE
	))

	required <- c("id", "time", "value")
	missing_cols <- setdiff(required, names(x))
	if (length(missing_cols) > 0) {
		issues <- dplyr::bind_rows(
			issues,
			data.frame(
				issue = rep("missing_column", length(missing_cols)),
				id = rep(NA_character_, length(missing_cols)),
				time = rep(as.POSIXct(NA_real_, origin = "1970-01-01", tz = tz), length(missing_cols)),
				value = rep(NA_real_, length(missing_cols)),
				details = paste0("Missing column: ", missing_cols),
				stringsAsFactors = FALSE
			)
		)
		return(dplyr::as_tibble(issues))
	}

	if (!inherits(x$time, "POSIXct")) {
		issues <- dplyr::bind_rows(
			issues,
			data.frame(
				issue = "invalid_time_class",
				id = NA_character_,
				time = as.POSIXct(NA_real_, origin = "1970-01-01", tz = tz),
				value = NA_real_,
				details = "time column must be POSIXct",
				stringsAsFactors = FALSE
			)
		)
		return(dplyr::as_tibble(issues))
	}

	if (nrow(x) > 1) {
		by_id <- split(seq_len(nrow(x)), x$id)
		non_inc_idx <- unlist(lapply(by_id, function(idx) {
			if (length(idx) < 2) {
				return(integer())
			}
			time_slice <- x$time[idx]
			idx[-1][as.numeric(diff(time_slice)) <= 0]
		}), use.names = FALSE)
		if (length(non_inc_idx) > 0) {
			issues <- dplyr::bind_rows(
				issues,
				data.frame(
					issue = rep("non_increasing_time", length(non_inc_idx)),
					id = as.character(x$id[non_inc_idx]),
					time = x$time[non_inc_idx],
					value = x$value[non_inc_idx],
					details = rep("Timestamps must be strictly increasing within each id.",
												length(non_inc_idx)),
					stringsAsFactors = FALSE
				)
			)
		}
	}

	dup_idx <- which(duplicated(x[, c("id", "time")]))
	if (length(dup_idx) > 0) {
		issues <- dplyr::bind_rows(
			issues,
			data.frame(
				issue = rep("duplicate", length(dup_idx)),
				id = as.character(x$id[dup_idx]),
				time = x$time[dup_idx],
				value = x$value[dup_idx],
				details = rep("Duplicate (id, time) encountered.", length(dup_idx)),
				stringsAsFactors = FALSE
			)
		)
	}

	non_finite_idx <- which(!is.finite(x$value) | is.na(x$value))
	if (length(non_finite_idx) > 0) {
		issues <- dplyr::bind_rows(
			issues,
			data.frame(
				issue = rep("non_finite_value", length(non_finite_idx)),
				id = as.character(x$id[non_finite_idx]),
				time = x$time[non_finite_idx],
				value = x$value[non_finite_idx],
				details = rep("Value must be finite.", length(non_finite_idx)),
				stringsAsFactors = FALSE
			)
		)
	}

		range_idx <- integer(0)
		if (!is.null(lower) && !is.na(lower)) {
			range_idx <- c(range_idx, which(!is.na(x$value) & x$value < lower))
		}
		if (!is.null(upper) && !is.na(upper)) {
			range_idx <- c(range_idx, which(!is.na(x$value) & x$value > upper))
		}
	range_idx <- unique(range_idx)
	if (length(range_idx) > 0) {
		details_vec <- rep("Value outside configured bounds.", length(range_idx))
			if (!is.null(lower) && !is.na(lower)) {
				below_mask <- x$value[range_idx] < lower
				details_vec[below_mask] <- sprintf("Value below lower bound (%s).", lower)
			}
			if (!is.null(upper) && !is.na(upper)) {
				above_mask <- x$value[range_idx] > upper
				details_vec[above_mask] <- sprintf("Value above upper bound (%s).", upper)
			}
		issues <- dplyr::bind_rows(
			issues,
			data.frame(
				issue = rep("out_of_range", length(range_idx)),
				id = as.character(x$id[range_idx]),
				time = x$time[range_idx],
				value = x$value[range_idx],
				details = details_vec,
				stringsAsFactors = FALSE
			)
		)
	}

	dplyr::as_tibble(issues)
}

#' @rdname validate_ts
#' @param issues Output tibble returned by [validate_ts()].
#' @export
has_issues <- function(issues) identical(nrow(issues) > 0, TRUE)
