# =============================================================================
# INTERNAL UTILITY FUNCTIONS (not exported)
# =============================================================================

.validate_numeric <- function(x, name) {
  if (!is.numeric(x)) {
    stop(sprintf("'%s' must be numeric.", name), call. = FALSE)
  }
}

.validate_positive <- function(x, name) {
  .validate_numeric(x, name)
  if (any(x <= 0, na.rm = TRUE)) {
    stop(sprintf("'%s' must contain only positive values.", name), call. = FALSE)
  }
}

.validate_non_negative <- function(x, name) {
  .validate_numeric(x, name)
  if (any(x < 0, na.rm = TRUE)) {
    stop(sprintf("'%s' must not contain negative values.", name), call. = FALSE)
  }
}

.validate_same_length <- function(x, y) {
  if (length(x) != length(y) && length(x) != 1 && length(y) != 1) {
    stop("Input vectors must have the same length.", call. = FALSE)
  }
}

.validate_proportion <- function(x, name) {
  .validate_numeric(x, name)
  if (any(x < 0 | x > 1, na.rm = TRUE)) {
    warning(sprintf("'%s' contains values outside [0, 1].", name), call. = FALSE)
  }
}

.validate_weights <- function(w, n) {
  if (length(w) != n) {
    stop(sprintf("'weights' must have exactly %d elements.", n), call. = FALSE)
  }
  if (abs(sum(w) - 1) > 1e-6) {
    stop("'weights' must sum to 1.", call. = FALSE)
  }
}
