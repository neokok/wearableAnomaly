#' Detect changepoints in wearable signals using the PELT algorithm
#'
#' @param x A `wa_ts` tibble produced by [as_wearable_ts()].
#' @param cost Cost function to optimise. Choose from `"meanvar"` (default),
#'   `"mean"`, or `"poisson"`.
#' @param penalty Penalty strategy. Options are `"MBIC"`, `"BIC"`, and
#'   `"AIC"`.
#' @param min_seg_len Minimum number of observations permitted in any segment.
#'
#' @return A tibble with the detected changepoints containing columns `id`,
#'   `cp_time`, `cp_index`, `new_level`, `new_var`, `method`, and `penalty`.
#'   Returns a zero-row tibble with these columns when no changepoints are
#'   detected.
#' @export
#'
#' @examples
#' set.seed(123)
#' n <- 240
#' times <- seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
#'                     by = "5 min", length.out = n)
#' values <- c(rnorm(120, mean = 100, sd = 5), rnorm(120, mean = 130, sd = 5))
#' data <- dplyr::tibble(id = "id01", time = times, value = values)
#' ts <- as_wearable_ts(data, id = id, time = time, value = value)
#' detect_changepoints_pelt(ts, cost = "meanvar", penalty = "MBIC")
detect_changepoints_pelt <- function(x,
                                     cost = c("meanvar", "mean", "poisson"),
                                     penalty = c("MBIC", "BIC", "AIC"),
                                     min_seg_len = 12L) {
  if (!inherits(x, "wa_ts")) {
    stop("`detect_changepoints_pelt()` expects a `wa_ts` object.", call. = FALSE)
  }
  cost <- match.arg(cost)
  penalty <- match.arg(penalty)
  if (!is.numeric(min_seg_len) || length(min_seg_len) != 1L || is.na(min_seg_len) ||
      min_seg_len < 2L) {
    stop("`min_seg_len` must be a single integer >= 2.", call. = FALSE)
  }
  min_seg_len <- as.integer(min_seg_len)

  tz <- attr(x, "tz") %||% "UTC"
  empty <- dplyr::tibble(
    id = character(),
    cp_time = as.POSIXct(numeric(0), origin = "1970-01-01", tz = tz),
    cp_index = integer(),
    new_level = numeric(),
    new_var = numeric(),
    method = character(),
    penalty = character()
  )

  x <- dplyr::arrange(dplyr::as_tibble(x), id, time)
  split_idx <- split(seq_len(nrow(x)), x$id)

  results <- lapply(names(split_idx), function(id_val) {
    idx <- split_idx[[id_val]]
    data_id <- x[idx, , drop = FALSE]
    na_mask <- is.na(data_id$value)
    if (any(na_mask)) {
      warning(sprintf("Dropped %d rows with missing values for id %s.",
                      sum(na_mask), id_val), call. = FALSE)
      data_id <- data_id[!na_mask, , drop = FALSE]
    }

    n <- nrow(data_id)
    if (n == 0) {
      return(empty)
    }
    if (n < 2L * min_seg_len) {
      stop(sprintf("id %s has only %d observations; need at least %d for min_seg_len = %d.",
                   id_val, n, 2L * min_seg_len, min_seg_len), call. = FALSE)
    }

    y <- data_id$value
    times <- data_id$time

    if (cost == "poisson" && any(y < 0, na.rm = TRUE)) {
      stop("Poisson cost requires non-negative values.", call. = FALSE)
    }

    cost_obj <- .make_cost_functions(y, cost)
    penalty_value <- .penalty_value(penalty, cost, length(y))
    cp_indices <- .pelt_core_dispatch(
      values = y,
      segment_cost = cost_obj$segment_cost,
      min_seg_len = min_seg_len,
      penalty_value = penalty_value,
      cost = cost
    )

    if (length(cp_indices) == 0) {
      return(empty)
    }

    post <- pelt_postprocess(
      values = y,
      times = times,
      cp_indices = cp_indices,
      method = "pelt",
      penalty = penalty
    )
    if (nrow(post) == 0) {
      return(empty)
    }
    post$id <- id_val
    post[, c("id", "cp_time", "cp_index", "new_level", "new_var", "method", "penalty")]
  })

  out <- dplyr::bind_rows(results)
  if (nrow(out) == 0) {
    return(empty)
  }
  out <- out[order(out$id, out$cp_time), , drop = FALSE]
  dplyr::as_tibble(out)
}

.pelt_core_r <- function(values, segment_cost, min_seg_len, penalty_value) {
  n <- length(values)
  if (n < 2L * min_seg_len) {
    stop("Series is shorter than required for the supplied `min_seg_len`.",
         call. = FALSE)
  }

  F <- rep(Inf, n + 1L)
  F[1] <- -penalty_value
  cp_store <- vector("list", n + 1L)
  candidate_set <- 0L

  for (t in seq.int(min_seg_len, n)) {
    eligible <- candidate_set[candidate_set <= t - min_seg_len]
    if (length(eligible) == 0L) {
      next
    }
    seg_costs <- segment_cost(eligible, t)
    total_costs <- F[eligible + 1L] + seg_costs + penalty_value
    best_idx <- which.min(total_costs)
    best_candidate <- eligible[best_idx]
    F[t + 1L] <- total_costs[best_idx]
    cp_store[[t + 1L]] <- c(cp_store[[best_candidate + 1L]], best_candidate)

    # Prune candidates that cannot be optimal going forward
    keep_mask <- rep(TRUE, length(candidate_set))
    if (length(eligible) > 0L) {
      drop_candidates <- eligible[total_costs > (F[t + 1L] + 1e-8)]
      if (length(drop_candidates) > 0L) {
        keep_mask <- !candidate_set %in% drop_candidates
      }
    }
    candidate_set <- unique(c(candidate_set[keep_mask], t))
  }

  cps <- cp_store[[n + 1L]]
  cps <- cps[cps > 0L]
  as.integer(sort(unique(cps)))
}

.make_cost_functions <- function(values, cost) {
  prefix_sum <- c(0, cumsum(values))
  prefix_sumsq <- c(0, cumsum(values ^ 2))
  segment_cost <- switch(cost,
    meanvar = function(s_vec, t) {
      len <- t - s_vec
      seg_sum <- prefix_sum[t + 1L] - prefix_sum[s_vec + 1L]
      seg_sumsq <- prefix_sumsq[t + 1L] - prefix_sumsq[s_vec + 1L]
      sse <- seg_sumsq - (seg_sum ^ 2) / len
      var_hat <- pmax(sse / len, .Machine$double.eps)
      len * log(var_hat)
    },
    mean = function(s_vec, t) {
      len <- t - s_vec
      seg_sum <- prefix_sum[t + 1L] - prefix_sum[s_vec + 1L]
      seg_sumsq <- prefix_sumsq[t + 1L] - prefix_sumsq[s_vec + 1L]
      seg_sumsq - (seg_sum ^ 2) / len
    },
    poisson = function(s_vec, t) {
      len <- t - s_vec
      seg_sum <- prefix_sum[t + 1L] - prefix_sum[s_vec + 1L]
      lambda_hat <- pmax(seg_sum / len, .Machine$double.eps)
      2 * (len * lambda_hat - seg_sum * log(lambda_hat))
    }
  )

  list(segment_cost = segment_cost)
}

.penalty_value <- function(penalty, cost, n) {
  k <- switch(cost,
    meanvar = 2,
    mean = 1,
    poisson = 2
  )
  base <- switch(penalty,
    MBIC = k * log(n) + 2 * log(log(n)),
    BIC = k * log(n),
    AIC = 2 * k
  )
  as.numeric(base)
}

#' @keywords internal
pelt_postprocess <- function(values, times, cp_indices, method, penalty) {
  make_empty <- function() {
    tz <- attr(times, "tzone") %||% "UTC"
    dplyr::tibble(
      id = character(),
      cp_time = as.POSIXct(numeric(0), origin = "1970-01-01", tz = tz),
      cp_index = integer(),
      new_level = numeric(),
      new_var = numeric(),
      method = character(),
      penalty = character()
    )
  }

  if (length(cp_indices) == 0L) {
    return(make_empty())
  }

  n <- length(values)
  cp_indices <- sort(unique(as.integer(cp_indices)))
  cp_indices <- cp_indices[cp_indices >= 1L & cp_indices < n]
  if (length(cp_indices) == 0L) {
    return(make_empty())
  }

  boundaries <- c(0L, cp_indices, n)
  segments <- vector("list", length(cp_indices))
  for (i in seq_along(cp_indices)) {
    right_start <- cp_indices[i] + 1L
    right_end <- boundaries[i + 2L]
    seg_values <- values[right_start:right_end]
    seg_mean <- mean(seg_values)
    seg_var <- if (length(seg_values) > 1L) stats::var(seg_values) else 0
    segments[[i]] <- list(
      cp_index = cp_indices[i],
      cp_time = times[cp_indices[i]],
      new_level = seg_mean,
      new_var = seg_var
    )
  }

  seg_df <- dplyr::bind_rows(lapply(segments, as.data.frame))
  seg_df$cp_index <- as.integer(seg_df$cp_index)
  seg_df$method <- method
  seg_df$penalty <- penalty
  dplyr::as_tibble(seg_df)
}
