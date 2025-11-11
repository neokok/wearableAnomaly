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
			value = signal,
			stringsAsFactors = FALSE
		)
	}

	out <- do.call(rbind, rows)
	dplyr::as_tibble(out)
}
