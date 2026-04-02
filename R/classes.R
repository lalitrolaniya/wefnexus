# =============================================================================
# S3 CLASSES AND METHODS
# Print, summary, and plot methods for wefnexus result objects
# =============================================================================

# --- nexus_result class (from nexus_summary) ---

#' Print method for nexus_result
#'
#' @param x An object of class \code{nexus_result}.
#' @param ... Additional arguments passed to \code{print.data.frame}.
#'
#' @return Invisibly returns the input object \code{x}. Called for the
#'   side effect of printing a formatted nexus analysis summary to the
#'   console.
#'
#' @export
print.nexus_result <- function(x, ...) {
  cat("WEFNC Nexus Analysis Results\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  cat("Treatments:", nrow(x), "\n")
  cat("Nexus Index range:", round(min(x$nexus_index), 3), "-",
      round(max(x$nexus_index), 3), "\n")
  cat("Best performer:", x$treatment[which.max(x$nexus_index)], "\n\n")
  print.data.frame(x, row.names = FALSE, ...)
  invisible(x)
}

#' Summary method for nexus_result
#'
#' @param object An object of class \code{nexus_result}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object \code{object}. Called for
#'   the side effect of printing a detailed dimension-wise summary to
#'   the console.
#'
#' @export
summary.nexus_result <- function(object, ...) {
  cat("WEFNC Nexus Summary\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  cat("Treatments:", paste(object$treatment, collapse = ", "), "\n\n")
  dims <- c("W_score", "E_score", "F_score", "N_score", "C_score")
  dim_names <- c("Water", "Energy", "Food", "Nutrient", "Carbon")
  cat("Dimension Scores (0-1, higher = better):\n")
  for (i in seq_along(dims)) {
    vals <- object[[dims[i]]]
    cat(sprintf("  %-10s: min=%.3f  mean=%.3f  max=%.3f\n",
                dim_names[i], min(vals), mean(vals), max(vals)))
  }
  cat("\nComposite Nexus Index:\n")
  for (j in seq_len(nrow(object))) {
    cat(sprintf("  %-15s: %.4f\n", object$treatment[j],
                object$nexus_index[j]))
  }
  cat("\nBest:", object$treatment[which.max(object$nexus_index)], "\n")
  invisible(object)
}

#' Plot method for nexus_result
#'
#' @param x An object of class \code{nexus_result}.
#' @param type Character. Plot type: \code{"radar"} (default) or
#'   \code{"heatmap"}.
#' @param ... Additional arguments passed to \code{\link{nexus_radar}}
#'   or \code{\link{nexus_heatmap}}.
#'
#' @return Invisibly returns the scores matrix. Called for the side
#'   effect of generating a radar or heatmap plot.
#'
#' @export
plot.nexus_result <- function(x, type = c("radar", "heatmap"), ...) {
  type <- match.arg(type)
  dims <- c("W_score", "E_score", "F_score", "N_score", "C_score")
  scores_mat <- as.matrix(x[, dims])
  if (type == "radar") {
    nexus_radar(scores_mat, treatment_names = x$treatment, ...)
  } else {
    nexus_heatmap(scores_mat, row_labels = x$treatment,
                  col_labels = c("Water", "Energy", "Food",
                                 "Nutrient", "Carbon"), ...)
  }
}

# --- cf_result class (from carbon_footprint, scalar) ---

#' Print method for cf_result
#'
#' @param x An object of class \code{cf_result}.
#' @param ... Additional arguments passed to \code{print}.
#'
#' @return Invisibly returns the input object \code{x}. Called for the
#'   side effect of printing a formatted carbon footprint summary to
#'   the console.
#'
#' @export
print.cf_result <- function(x, ...) {
  cat("Carbon Footprint Analysis\n")
  cat(paste(rep("-", 35), collapse = ""), "\n")
  cat("Total CF:", x$total_cf, "kg CO2-eq/ha\n")
  if (!is.null(x$cf_intensity)) {
    cat("CF intensity:", x$cf_intensity, "kg CO2-eq/kg grain\n")
  }
  cat("\nSource-wise breakdown:\n")
  print(x$breakdown, row.names = FALSE, ...)
  invisible(x)
}

#' Plot method for cf_result
#'
#' @param x An object of class \code{cf_result}.
#' @param ... Additional arguments passed to \code{\link[graphics]{barplot}}.
#'
#' @return No return value, called for the side effect of generating a
#'   horizontal bar plot showing carbon footprint by emission source.
#'
#' @export
plot.cf_result <- function(x, ...) {
  bd <- x$breakdown
  bd <- bd[bd$emission_kg_CO2eq > 0, ]
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))
  graphics::par(mar = c(5, 8, 4, 2))
  cols <- c("#F44336", "#FF9800", "#4CAF50", "#2196F3", "#9C27B0",
            "#795548", "#607D8B", "#E91E63", "#00BCD4")
  graphics::barplot(bd$emission_kg_CO2eq, names.arg = bd$source,
          horiz = TRUE, las = 1, col = cols[seq_len(nrow(bd))],
          xlab = "kg CO2-eq/ha",
          main = paste("Carbon Footprint:", x$total_cf, "kg CO2-eq/ha"),
          ...)
}

# --- sustainability_result class ---

#' Print method for sustainability_result
#'
#' @param x An object of class \code{sustainability_result}.
#' @param ... Additional arguments passed to \code{print.data.frame}.
#'
#' @return Invisibly returns the input object \code{x}. Called for the
#'   side effect of printing sustainability scores and categories to
#'   the console.
#'
#' @export
print.sustainability_result <- function(x, ...) {
  cat("WEFNC Sustainability Assessment\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  for (i in seq_len(nrow(x))) {
    cat(sprintf("  Score: %.4f -> %s\n", x$nexus_score[i], x$category[i]))
  }
  cat("\n")
  print.data.frame(x, row.names = FALSE, ...)
  invisible(x)
}
