#' @keywords internal
.pelt_cpp_available <- function() {
	exists("pelt_core_meanvar", envir = asNamespace("wearableAnomaly"), inherits = FALSE)
}

#' @keywords internal
.pelt_should_use_rcpp <- function(n) {
	if (n <= 0L) {
		return(FALSE)
	}
	if (!.pelt_cpp_available()) {
		return(FALSE)
	}
	threshold <- getOption("wearableAnomaly.rcpp_threshold", 5000L)
	use_opt <- getOption("wearableAnomaly.use_rcpp", TRUE)
	isTRUE(use_opt) || n >= threshold
}

#' @keywords internal
.pelt_core_dispatch <- function(values, segment_cost, min_seg_len, penalty_value, cost) {
	if (identical(cost, "meanvar") && .pelt_should_use_rcpp(length(values))) {
		res <- try(pelt_core_meanvar(values, min_seg_len, penalty_value), silent = TRUE)
		if (!inherits(res, "try-error")) {
			return(res)
		}
		warning("Falling back to R implementation of PELT after Rcpp failure.", call. = FALSE)
	}
	.pelt_core_r(values, segment_cost, min_seg_len, penalty_value)
}
