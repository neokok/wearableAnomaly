#' Detect changepoints with the E-divisive algorithm
#'
#' Implements a univariate version of the E-divisive procedure based on energy
#' distance to detect distributional changepoints. Candidate changepoints are
#' assessed via a permutation test and accepted when the p-value is below
#' `alpha`. Detected changepoints are then investigated recursively on the left
#' and right segments subject to the same minimum segment length constraint.
#'
#' @inheritParams detect_changepoints_pelt
#' @param R Number of permutations used to compute the reference distribution.
#' @param alpha Significance level required to accept a split (default `0.05`).
#'
#' @return A tibble with columns `id`, `cp_time`, `p_value`, and `method`. The
#'   tibble has zero rows when no changepoints are detected.
#' @export
#'
#' @examples
#' set.seed(123)
#' n <- 180
#' times <- seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
#'   by = "5 min", length.out = n
#' )
#' values <- c(
#'   rnorm(60, 100, 4),
#'   rnorm(60, 120, 6),
#'   rnorm(60, 110, 4)
#' )
#' ts <- as_wearable_ts(
#'   dplyr::tibble(id = "id01", time = times, value = values),
#'   id = id, time = time, value = value
#' )
#' detect_changepoints_edivisive(ts, min_seg_len = 12, R = 99)
detect_changepoints_edivisive <- function(x, min_seg_len = 12L, R = 199L, alpha = 0.05) {
  if (!inherits(x, "wa_ts")) {
    stop("`detect_changepoints_edivisive()` expects a `wa_ts` object.", call. = FALSE)
  }
  if (!is.numeric(min_seg_len) || length(min_seg_len) != 1L || is.na(min_seg_len) || min_seg_len < 2L) {
    stop("`min_seg_len` must be a single integer >= 2.", call. = FALSE)
  }
  min_seg_len <- as.integer(min_seg_len)

  if (!is.numeric(R) || length(R) != 1L || is.na(R) || R < 0) {
    stop("`R` must be a non-negative integer.", call. = FALSE)
  }
  R <- as.integer(R)

  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a probability in (0, 1).", call. = FALSE)
  }

  tz <- attr(x, "tz") %||% "UTC"
  empty <- dplyr::tibble(
    id = character(),
    cp_time = as.POSIXct(numeric(0), origin = "1970-01-01", tz = tz),
    p_value = numeric(),
    method = character()
  )

  tbl <- dplyr::as_tibble(x)
  tbl <- dplyr::arrange(tbl, id, time)
  splits <- split(seq_len(nrow(tbl)), tbl$id)

  results <- lapply(names(splits), function(id_val) {
    idx <- splits[[id_val]]
    data_id <- tbl[idx, , drop = FALSE]
    data_id <- data_id[order(data_id$time), , drop = FALSE]
    values <- data_id$value
    times <- data_id$time

    if (!is.numeric(values)) {
      stop("`value` column must be numeric for E-divisive.", call. = FALSE)
    }

    non_finite <- !is.finite(values)
    if (any(non_finite)) {
      warning(
        sprintf("Dropped %d rows with missing values for id %s.", sum(non_finite), id_val),
        call. = FALSE
      )
      data_id <- data_id[!non_finite, , drop = FALSE]
      values <- data_id$value
      times <- data_id$time
    }

    n <- length(values)
    if (n == 0) {
      return(NULL)
    }
    if (n < 2L * min_seg_len) {
      stop(
        sprintf(
          "id %s has only %d observations; need at least %d for min_seg_len = %d.",
          id_val, n, 2L * min_seg_len, min_seg_len
        ),
        call. = FALSE
      )
    }

    segs <- .edivisive_recurse(values, times, min_seg_len, R, alpha)
    if (nrow(segs) == 0) {
      return(NULL)
    }
    segs$id <- id_val
    segs
  })

  out <- dplyr::bind_rows(results)
  if (nrow(out) == 0) {
    return(empty)
  }
  out$method <- "edivisive"
  out <- dplyr::select(out, id, cp_time, p_value, method)
  dplyr::arrange(out, id, cp_time)
}

.edivisive_recurse <- function(values, times, min_seg_len, R, alpha) {
  n <- length(values)
  if (n < 2L * min_seg_len) {
    return(.edivisive_empty(times))
  }

  backend <- .ed_choose_backend(n)
  best <- .edivisive_best_split(values, min_seg_len, backend = backend)
  if (is.null(best) || !is.finite(best$stat) || best$stat <= 0) {
    return(.edivisive_empty(times))
  }

  p_value <- .edivisive_permutation_p(values, min_seg_len, best$stat, R, backend)
  if (p_value > alpha) {
    return(.edivisive_empty(times))
  }

  cp_idx <- best$cp_index
  current <- dplyr::tibble(cp_time = times[cp_idx], p_value = p_value)

  left <- .edivisive_recurse(
    values = values[seq_len(cp_idx)],
    times = times[seq_len(cp_idx)],
    min_seg_len = min_seg_len,
    R = R,
    alpha = alpha
  )
  right <- .edivisive_recurse(
    values = values[seq.int(cp_idx + 1L, n)],
    times = times[seq.int(cp_idx + 1L, n)],
    min_seg_len = min_seg_len,
    R = R,
    alpha = alpha
  )

  dplyr::bind_rows(left, current, right)
}

.edivisive_empty <- function(times) {
  empty_time <- times[FALSE]
  dplyr::tibble(cp_time = empty_time, p_value = numeric())
}

.edivisive_best_split <- function(values, min_seg_len, backend = c("auto", "r", "rcpp")) {
  backend <- match.arg(backend)
  if (backend == "auto") {
    backend <- .ed_choose_backend(length(values))
  }
  if (identical(backend, "rcpp") && .ed_cpp_available()) {
    return(.edivisive_best_split_rcpp(values, min_seg_len))
  }
  .edivisive_best_split_r(values, min_seg_len)
}

.edivisive_best_split_r <- function(values, min_seg_len) {
  n <- length(values)
  candidates <- seq.int(min_seg_len, n - min_seg_len)
  if (length(candidates) == 0) {
    return(NULL)
  }

  dist_mat <- abs(outer(values, values, "-"))
  split <- candidates[1]
  left_idx <- seq_len(split)
  right_idx <- seq.int(split + 1L, n)
  left_sum <- sum(dist_mat[left_idx, left_idx, drop = FALSE])
  cross_sum <- sum(dist_mat[left_idx, right_idx, drop = FALSE])
  right_sum <- sum(dist_mat[right_idx, right_idx, drop = FALSE])

  best_stat <- -Inf
  best_split <- NA_integer_
  current_split <- split

  for (split in candidates) {
    if (split > current_split) {
      for (move in seq.int(current_split + 1L, split)) {
        left_contrib <- if (move > 1L) sum(dist_mat[seq_len(move - 1L), move, drop = FALSE]) else 0
        right_contrib <- if (move + 1L <= n) {
          sum(dist_mat[move, seq.int(move + 1L, n), drop = FALSE])
        } else {
          0
        }
        left_sum <- left_sum + 2 * left_contrib
        cross_sum <- cross_sum - left_contrib + right_contrib
        right_sum <- right_sum - 2 * right_contrib
      }
      current_split <- split
    }

    n1 <- split
    n2 <- n - split
    cross_mean <- cross_sum / (n1 * n2)
    left_denom <- n1 * (n1 - 1L)
    right_denom <- n2 * (n2 - 1L)
    left_mean <- if (left_denom > 0) left_sum / left_denom else 0
    right_mean <- if (right_denom > 0) right_sum / right_denom else 0
    scale_factor <- (n1 * n2) / n
    stat <- scale_factor * (2 * cross_mean - left_mean - right_mean)

    if (stat > best_stat) {
      best_stat <- stat
      best_split <- split
    }
  }

  list(cp_index = best_split, stat = best_stat)
}

.edivisive_best_split_rcpp <- function(values, min_seg_len) {
  n <- length(values)
  candidates <- seq.int(min_seg_len, n - min_seg_len)
  if (length(candidates) == 0) {
    return(NULL)
  }
  best_stat <- -Inf
  best_split <- NA_integer_
  for (split in candidates) {
    stat <- .ed_energy_stat_rcpp(values, split)
    if (!is.finite(stat)) {
      next
    }
    if (stat > best_stat) {
      best_stat <- stat
      best_split <- split
    }
  }
  if (!is.finite(best_stat) || !is.finite(best_split)) {
    return(NULL)
  }
  list(cp_index = best_split, stat = best_stat)
}

.edivisive_permutation_p <- function(values, min_seg_len, observed_stat, R, backend) {
  if (R <= 0 || !is.finite(observed_stat)) {
    return(0)
  }
  perm_stats <- numeric(R)
  for (i in seq_len(R)) {
    perm_values <- sample(values, length(values), replace = FALSE)
    best <- .edivisive_best_split(perm_values, min_seg_len, backend = backend)
    perm_stats[i] <- if (is.null(best) || !is.finite(best$stat)) 0 else best$stat
  }
  (1 + sum(perm_stats >= observed_stat)) / (R + 1)
}
