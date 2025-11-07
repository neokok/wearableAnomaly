#' Coerce data into a wearable-anomaly time series tibble
#'
#' @param data A data frame containing subject identifiers, timestamps, and
#'   signal values.
#' @param id,time,value Tidy-select expressions identifying the identifier,
#'   timestamp, and value columns respectively.
#' @param tz Time zone used when coercing the `time` column to POSIXct. Defaults
#'   to "UTC".
#' @param lower,upper Numeric bounds used when assessing valid signal values.
#' @param cadence Optional numeric cadence (in seconds) describing the expected
#'   sampling interval. Stored as metadata when supplied.
#'
#' @return A tibble with class `wa_ts` and attributes `lower`, `upper`,
#'   `cadence` (stored as `NA` when not supplied), `tz`, and `dups_dropped`.
#' @export
#'
#' @examples
#' data <- toy_cgm(n_id = 2, n = 6)
#' wa <- as_wearable_ts(data, id = id, time = time, value = value)
#' validate_ts(wa)
as_wearable_ts <- function(data, id, time, value, tz = "UTC", lower = 40,
													 upper = 400, cadence = NULL) {
	data_tbl <- dplyr::as_tibble(data)

	pull_column <- function(tbl, expr, arg_name) {
		tryCatch(
			dplyr::pull(tbl, {{ expr }}),
			error = function(e) {
				stop(sprintf("`%s` must refer to an existing column in `data`.", arg_name),
						 call. = FALSE)
			}
		)
	}

	id_vec <- pull_column(data_tbl, {{ id }}, "id")
	time_vec <- pull_column(data_tbl, {{ time }}, "time")
	value_vec <- pull_column(data_tbl, {{ value }}, "value")

	n_rows <- nrow(data_tbl)
	if (length(id_vec) != n_rows || length(time_vec) != n_rows ||
			length(value_vec) != n_rows) {
		stop("`id`, `time`, and `value` must each select exactly one column.",
				 call. = FALSE)
	}

	if (all(is.na(value_vec))) {
		stop("`value` column contains only missing values.", call. = FALSE)
	}

	time_posix <- if (inherits(time_vec, "POSIXct")) {
		attr(time_vec, "tzone") <- tz
		time_vec
	} else if (inherits(time_vec, "Date")) {
		as.POSIXct(time_vec, tz = tz)
	} else {
		suppressWarnings({
			converted <- as.POSIXct(time_vec, tz = tz)
		})
		if (any(!is.na(time_vec)) && all(is.na(converted))) {
			stop("`time` could not be coerced to POSIXct. Provide POSIXct, Date, or convertible inputs.",
					 call. = FALSE)
		}
		attr(converted, "tzone") <- tz
		converted
	}

	if (!inherits(time_posix, "POSIXct")) {
		stop("`time` must be coercible to POSIXct.", call. = FALSE)
	}

	wa <- data.frame(
		id = id_vec,
		time = time_posix,
		value = value_vec,
		stringsAsFactors = FALSE
	)
	wa <- dplyr::as_tibble(wa)

	original_index <- seq_len(nrow(wa))
	ordered_index <- order(wa$id, wa$time)
	if (!identical(ordered_index, original_index)) {
		wa <- wa[ordered_index, , drop = FALSE]
		warning("Reordered input by `id` and `time`.", call. = FALSE)
	}

	dup_mask <- duplicated(wa[, c("id", "time")])
	dups_dropped <- sum(dup_mask)
	if (dups_dropped > 0) {
		wa <- wa[!dup_mask, , drop = FALSE]
		warning(sprintf("Dropped %d duplicated observation(s) using the first occurrence of each (id, time).",
										dups_dropped), call. = FALSE)
	}

	wa_ts <- structure(
		wa,
		class = c("wa_ts", class(wa))
	)

		attr(wa_ts, "lower") <- lower
		attr(wa_ts, "upper") <- upper
		attr(wa_ts, "cadence") <- if (is.null(cadence)) NA_real_ else cadence
	attr(wa_ts, "tz") <- tz
	attr(wa_ts, "dups_dropped") <- as.integer(dups_dropped)

	wa_ts
}
