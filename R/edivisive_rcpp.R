#' @keywords internal
.ed_cpp_available <- function() {
  ns <- asNamespace("wearableAnomaly")
  if (!exists("energy_stat_segment", envir = ns, inherits = FALSE)) {
    return(FALSE)
  }
  info <- tryCatch(
    getNativeSymbolInfo(
      "_wearableAnomaly_energy_stat_segment",
      PACKAGE = "wearableAnomaly"
    ),
    error = function(e) NULL
  )
  if (is.null(info)) {
    return(FALSE)
  }
  TRUE
}

#' @keywords internal
.ed_should_use_rcpp <- function(n) {
  if (!.ed_cpp_available() || length(n) == 0L || n <= 0L) {
    return(FALSE)
  }
  threshold <- getOption("wearableAnomaly.rcpp_threshold", 500L)
  use_opt <- getOption("wearableAnomaly.use_rcpp", TRUE)
  isTRUE(use_opt) || n >= threshold
}

#' @keywords internal
.ed_choose_backend <- function(n) {
  if (.ed_should_use_rcpp(n)) {
    return("rcpp")
  }
  "r"
}

#' @keywords internal
.ed_energy_stat_rcpp <- function(values, split) {
  energy_stat_segment(values, 1L, split, split + 1L, length(values))
}
