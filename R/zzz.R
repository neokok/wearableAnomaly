#' wearableAnomaly: Tools for Wearable Device Anomaly Detection
#'
#' The package provides helpers to ingest, validate, resample, and analyse
#' wearable sensor signals ahead of anomaly detection workflows.
#'
#' @docType package
#' @name wearableAnomaly-package
#' @useDynLib wearableAnomaly, .registration = TRUE
#' @importFrom Rcpp sourceCpp
"_PACKAGE"

utils::globalVariables(c(
	"bin", "value", "time", "id", "start", "end", "duration", "type", "strength",
	"..start_idx", "..end_idx", "cp_time", "cp_index", "new_level", "new_var", "method",
	"penalty"
))
