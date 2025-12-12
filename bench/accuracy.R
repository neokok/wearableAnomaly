## Packages
library(tibble)
library(wearableAnomaly)   # where your detect_* live
library(ecp)
library(changepoint)

set.seed(123)


simulate_cgm_truth = function(
    id          = "id01",
    n_points    = 24 * 12,        # 1 day at 5 min resolution
    min_seg_len = 12,             # 1 hour
    n_cps       = 4,              # number of true changepoints
    start_time  = as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
    dt_minutes  = 5,
    phi         = 0.7             # AR(1) coefficient
) {
  n = n_points

  ## choose internal changepoints with spacing >= 2 * min_seg_len
  possible = (2 * min_seg_len):(n - 2 * min_seg_len)
  cp_truth = sort(sample(possible, size = n_cps, replace = FALSE))
  cp_truth = as.integer(cp_truth)

  cuts = c(0, cp_truth, n)
  seg_lengths = diff(cuts)

  ## generate segment means and sds
  seg_means = pmin(pmax(rnorm(length(seg_lengths), mean = 130, sd = 25), 70), 250)
  seg_sds   = runif(length(seg_lengths), min = 10, max = 35)

  values = numeric(0)

  for (k in seq_along(seg_lengths)) {
    len_k  = seg_lengths[k]
    mu_k   = seg_means[k]
    sd_k   = seg_sds[k]

    ## AR(1) errors around mu_k
    eps = as.numeric(arima.sim(
      model = list(ar = phi),
      n     = len_k,
      sd    = sd_k * sqrt(1 - phi^2)
    ))
    seg_vals = mu_k + eps
    seg_vals = pmin(pmax(seg_vals, 40), 400)   # clamp to CGM range

    values = c(values, seg_vals)
  }

  times = seq.POSIXt(from = start_time,
                     by   = paste(dt_minutes, "min"),
                     length.out = n)

  raw_df = tibble(
    id    = id,
    time  = times,
    value = values
  )

  ts = as_wearable_ts(raw_df, id = id, time = time, value = value)

  list(
    ts       = ts,
    cp_truth = cp_truth,
    cp_time  = times[cp_truth]
  )
}

match_cps = function(true_cps, est_cps, w = 3) {
  true_cps = sort(unique(true_cps))
  est_cps  = sort(unique(est_cps))

  if (length(true_cps) == 0 && length(est_cps) == 0) {
    return(list(TP = 0, FP = 0, FN = 0,
                precision = NA_real_, recall = NA_real_, F1 = NA_real_,
                abs_err = numeric(0)))
  }

  if (length(true_cps) == 0) {
    FP = length(est_cps)
    return(list(TP = 0, FP = FP, FN = 0,
                precision = 0, recall = NA_real_, F1 = NA_real_,
                abs_err = numeric(0)))
  }

  if (length(est_cps) == 0) {
    FN = length(true_cps)
    return(list(TP = 0, FP = 0, FN = FN,
                precision = NA_real_, recall = 0, F1 = NA_real_,
                abs_err = numeric(0)))
  }

  true_used = rep(FALSE, length(true_cps))
  est_used  = rep(FALSE, length(est_cps))
  abs_err   = numeric(0)

  for (j in seq_along(est_cps)) {
    diffs = abs(true_cps - est_cps[j])
    i_min = which.min(diffs)
    if (!true_used[i_min] && diffs[i_min] <= w) {
      true_used[i_min] = TRUE
      est_used[j]      = TRUE
      abs_err          = c(abs_err, diffs[i_min])
    }
  }

  TP = sum(est_used)
  FP = sum(!est_used)
  FN = sum(!true_used)

  precision = TP / (TP + FP + 1e-8)
  recall    = TP / (TP + FN + 1e-8)
  F1        = 2 * precision * recall / (precision + recall + 1e-8)

  list(TP = TP, FP = FP, FN = FN,
       precision = precision,
       recall    = recall,
       F1        = F1,
       abs_err   = abs_err)
}


compare_edivisive_one = function(sim,
                                 min_seg_len = 12,
                                 R           = 199,
                                 alpha       = 0.05,
                                 tol_window  = 5) {
  ts = sim$ts
  x  = ts$value
  n  = length(x)
  true_cps = sim$cp_truth

  ## your implementation
  t_mine = system.time({
    res_mine = detect_changepoints_edivisive(
      ts,
      min_seg_len = min_seg_len,
      R           = R,
      alpha       = alpha
    )
  })[["elapsed"]]

  if (nrow(res_mine) == 0) {
    cps_mine = integer(0)
  } else {
    cps_mine = match(res_mine$cp_time, ts$time)
    cps_mine = cps_mine[!is.na(cps_mine)]
  }

  m_mine = match_cps(true_cps, cps_mine, w = tol_window)

  row_mine = data.frame(
    method     = "wearableAnomaly_edivisive",
    n          = n,
    elapsed    = t_mine,
    n_cps_hat  = length(cps_mine),
    precision  = m_mine$precision,
    recall     = m_mine$recall,
    F1         = m_mine$F1,
    mean_abs_err = ifelse(length(m_mine$abs_err) == 0,
                          NA_real_,
                          mean(m_mine$abs_err))
  )

  ## ecp implementation
  t_pkg = system.time({
    res_pkg = ecp::e.divisive(
      X        = matrix(x, ncol = 1),
      R        = R,
      sig.lvl  = alpha,
      min.size = min_seg_len,
      alpha    = 1          # energy exponent, default
    )
  })[["elapsed"]]

  est_pkg = res_pkg$estimates
  if (length(est_pkg) <= 2) {
    cps_pkg = integer(0)
  } else {
    cps_pkg = est_pkg[-c(1, length(est_pkg))]
  }

  m_pkg = match_cps(true_cps, cps_pkg, w = tol_window)

  row_pkg = data.frame(
    method     = "ecp_e.divisive",
    n          = n,
    elapsed    = t_pkg,
    n_cps_hat  = length(cps_pkg),
    precision  = m_pkg$precision,
    recall     = m_pkg$recall,
    F1         = m_pkg$F1,
    mean_abs_err = ifelse(length(m_pkg$abs_err) == 0,
                          NA_real_,
                          mean(m_pkg$abs_err))
  )

  rbind(row_mine, row_pkg)
}


benchmark_edivisive = function(
    n_series    = 50,
    n_points    = 24 * 12,
    min_seg_len = 12,
    n_cps       = 4,
    R           = 199,
    alpha       = 0.05,
    tol_window  = 5
) {
  res_list = vector("list", length = n_series)

  for (b in 1:n_series) {
    sim_b = simulate_cgm_truth(
      id          = paste0("id", sprintf("%03d", b)),
      n_points    = n_points,
      min_seg_len = min_seg_len,
      n_cps       = n_cps
    )
    res_list[[b]] = compare_edivisive_one(
      sim_b,
      min_seg_len = min_seg_len,
      R           = R,
      alpha       = alpha,
      tol_window  = tol_window
    )
  }

  res = do.call(rbind, res_list)

  summary = aggregate(
    cbind(elapsed, n_cps_hat, precision, recall, F1, mean_abs_err) ~ method,
    data = res,
    FUN  = function(z) c(mean = mean(z, na.rm = TRUE),
                         sd   = sd(z,   na.rm = TRUE))
  )

  list(raw = res, summary = summary)
}


compare_pelt_one = function(sim,
                            cost        = "meanvar",
                            penalty     = "MBIC",
                            min_seg_len = 12,
                            tol_window  = 5) {
  ts = sim$ts
  x  = ts$value
  n  = length(x)
  true_cps = sim$cp_truth

  ## your PELT implementation
  t_mine = system.time({
    res_mine = detect_changepoints_pelt(
      ts,
      cost        = cost,
      penalty     = penalty,
      min_seg_len = min_seg_len
    )
  })[["elapsed"]]

  if (nrow(res_mine) == 0) {
    cps_mine = integer(0)
  } else {
    cps_mine = sort(unique(res_mine$cp_index))
  }

  m_mine = match_cps(true_cps, cps_mine, w = tol_window)

  row_mine = data.frame(
    method       = "wearableAnomaly_PELT",
    n            = n,
    elapsed      = t_mine,
    n_cps_hat    = length(cps_mine),
    precision    = m_mine$precision,
    recall       = m_mine$recall,
    F1           = m_mine$F1,
    mean_abs_err = ifelse(length(m_mine$abs_err) == 0,
                          NA_real_,
                          mean(m_mine$abs_err))
  )

  ## changepoint PELT with matching settings
  cp_fun = switch(
    cost,
    meanvar = changepoint::cpt.meanvar,
    mean    = changepoint::cpt.mean,
    poisson = changepoint::cpt.poisson,
    stop("Unsupported cost")
  )

  t_pkg = system.time({
    res_pkg = cp_fun(
      x,
      method   = "PELT",
      penalty  = penalty,
      minseglen = min_seg_len
    )
  })[["elapsed"]]

  cps_pkg = changepoint::cpts(res_pkg)

  m_pkg = match_cps(true_cps, cps_pkg, w = tol_window)

  row_pkg = data.frame(
    method       = "changepoint_PELT",
    n            = n,
    elapsed      = t_pkg,
    n_cps_hat    = length(cps_pkg),
    precision    = m_pkg$precision,
    recall       = m_pkg$recall,
    F1           = m_pkg$F1,
    mean_abs_err = ifelse(length(m_pkg$abs_err) == 0,
                          NA_real_,
                          mean(m_pkg$abs_err))
  )

  rbind(row_mine, row_pkg)
}


benchmark_pelt = function(
    n_series    = 50,
    n_points    = 24 * 12,
    min_seg_len = 12,
    n_cps       = 4,
    cost        = "meanvar",
    penalty     = "MBIC",
    tol_window  = 5
) {
  res_list = vector("list", length = n_series)

  for (b in 1:n_series) {
    sim_b = simulate_cgm_truth(
      id          = paste0("id", sprintf("%03d", b)),
      n_points    = n_points,
      min_seg_len = min_seg_len,
      n_cps       = n_cps
    )
    res_list[[b]] = compare_pelt_one(
      sim_b,
      cost        = cost,
      penalty     = penalty,
      min_seg_len = min_seg_len,
      tol_window  = tol_window
    )
  }

  res = do.call(rbind, res_list)

  summary = aggregate(
    cbind(elapsed, n_cps_hat, precision, recall, F1, mean_abs_err) ~ method,
    data = res,
    FUN  = function(z) c(mean = mean(z, na.rm = TRUE),
                         sd   = sd(z,   na.rm = TRUE))
  )

  list(raw = res, summary = summary)
}

## E divisive
## E-divisive (improved settings)
ediv_results = benchmark_edivisive(
  n_series    = 50,
  n_points    = 24 * 12,
  min_seg_len = 18,    # bump from 12 → 18 (reduces false positives)
  n_cps       = 4,
  R           = 99,    # faster and still stable
  alpha       = 0.01,  # stricter significance threshold
  tol_window  = 5
)

ediv_results$summary



pelt_results = benchmark_pelt(
  n_series    = 50,
  n_points    = 24 * 12,
  min_seg_len = 30,      # ← NEW recommended update
  n_cps       = 4,
  cost        = "meanvar",
  penalty     = "MBIC",
  tol_window  = 5
)

pelt_results$summary


pelt_results$summary
