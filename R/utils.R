`%||%` <- function(lhs, rhs) {
	if (!is.null(lhs)) lhs else rhs
}

#' Generate a small synthetic CGM-like data set
#'
#' @param n_id Number of unique subjects to simulate.
#' @param n Number of observations per subject.
#' @param by Interval passed to [base::seq.POSIXt()] for timestamp spacing.
#' @param seed Optional integer seed for reproducible noise.
#'
#' @return A tibble with columns `id`, `time`, and `value` suitable for
#'   examples and unit tests.
#' @export
#'
#' @examples
#' toy_cgm()
toy_cgm <- function(n_id = 2, n = 12, by = "5 min", seed = 1) {
	if (!is.null(seed)) {
		had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
		if (had_seed) {
			old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
			on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
		} else {
			on.exit(rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
		}
		set.seed(seed)
	}

	n_id <- as.integer(n_id)
	n <- as.integer(n)
	if (n_id < 1L || n < 1L) {
		stop("`n_id` and `n` must be positive integers.", call. = FALSE)
	}

	start_time <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
	subjects <- seq_len(n_id)
	rows <- vector("list", length(subjects))

	for (i in subjects) {
		times_i <- seq(from = start_time + (i - 1) * n * 60, by = by, length.out = n)
		phase <- seq_len(n)
		signal <- 110 + 20 * sin(2 * pi * phase / max(1L, n)) + stats::rnorm(n, sd = 5)
		signal <- signal + (i - 1) * 5
		rows[[i]] <- data.frame(
			id = sprintf("id%02d", i),
			time = times_i,
			value = round(signal),
			stringsAsFactors = FALSE
		)
	}

	out <- do.call(rbind, rows)
	dplyr::as_tibble(out)
}

#' Simulate CGM benchmark series with ground-truth changepoints
#'
#' @param n_series Number of independent subjects to simulate.
#' @param n_points Number of observations per subject (default 288 = 24h at 5
#'   minute cadence).
#' @param min_seg_len Minimum segment length between changepoints (in samples).
#' @param n_cps Nominal number of changepoints to embed per subject.
#' @param seed Optional integer for reproducibility.
#' @param start_time POSIXct timestamp of the first observation.
#' @param dt_minutes Cadence in minutes between readings.
#'
#' @return A list with two tibbles: `data` containing columns `id`, `time`,
#'   `value`; and `truth` containing `id`, `start`, and `end` (changepoint times).
#' @export
#'
#' @examples
#' sim <- simulate_cgm_benchmark(n_series = 2, n_points = 96, n_cps = 2, seed = 42)
#' names(sim)
simulate_cgm_benchmark <- function(n_series = 10L,
                                   n_points = 288L,
                                   min_seg_len = 12L,
                                   n_cps = 3L,
                                   seed = NULL,
                                   start_time = as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
                                   dt_minutes = 5) {
	if (!is.null(seed)) {
		had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
		if (had_seed) {
			old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
			on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
		} else {
			on.exit(rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
		}
		set.seed(seed)
	}

	n_series <- as.integer(n_series)
	n_points <- as.integer(n_points)
	min_seg_len <- as.integer(min_seg_len)
	n_cps <- as.integer(n_cps)
	if (n_series < 1L || n_points < 4L) {
		stop("`n_series` must be >=1 and `n_points` >=4.", call. = FALSE)
	}
	if (min_seg_len < 2L) {
		stop("`min_seg_len` must be >= 2.", call. = FALSE)
	}

	data_list <- vector("list", n_series)
	truth_list <- vector("list", n_series)
	dt_char <- paste(dt_minutes, "min")

	for (i in seq_len(n_series)) {
		id_val <- sprintf("id%03d", i)
		possible <- seq.int(min_seg_len, max(min_seg_len, n_points - min_seg_len))
		if (length(possible) == 0L) {
			cp_idx <- integer()
		} else {
			n_possible <- length(possible)
			k <- min(max(1L, n_cps), n_possible)
			cp_idx <- sort(sample(possible, size = k, replace = FALSE))
		}

		segment_breaks <- c(0L, cp_idx, n_points)
		seg_lengths <- diff(segment_breaks)
		n_segments <- length(seg_lengths)
		seg_means <- pmin(pmax(stats::rnorm(n_segments, mean = 130, sd = 20), 60), 240)
		seg_sds <- runif(n_segments, min = 8, max = 30)

		values <- numeric(0)
		for (j in seq_len(n_segments)) {
			len_j <- seg_lengths[j]
			mu <- seg_means[j]
			sd_j <- seg_sds[j]
			if (len_j <= 0) {
				next
			}
			eps <- stats::arima.sim(
				model = list(ar = 0.6),
				n = len_j,
				sd = sd_j * sqrt(1 - 0.6^2)
			)
			vals <- mu + eps
			values <- c(values, pmin(pmax(vals, 40), 410))
		}
		values <- values[seq_len(n_points)]

		times <- seq(
			from = start_time + (i - 1) * n_points * dt_minutes * 60,
			by = dt_char,
			length.out = n_points
		)

		data_list[[i]] <- dplyr::tibble(
			id = id_val,
			time = times,
			value = values
		)

		if (length(cp_idx) == 0L) {
			truth_list[[i]] <- dplyr::tibble(
				id = character(),
				start = as.POSIXct(numeric(0), origin = "1970-01-01", tz = attr(times, "tzone") %||% "UTC"),
				end = as.POSIXct(numeric(0), origin = "1970-01-01", tz = attr(times, "tzone") %||% "UTC")
			)
		} else {
			cp_times <- times[pmin(cp_idx, length(times))]
			truth_list[[i]] <- dplyr::tibble(
				id = id_val,
				start = cp_times,
				end = cp_times
			)
		}
	}

	list(
		data = dplyr::bind_rows(data_list),
		truth = dplyr::bind_rows(truth_list)
	)
}
