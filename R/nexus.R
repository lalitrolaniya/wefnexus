# =============================================================================
# NEXUS MODULE
# Integration, Trade-off Analysis, Visualization & Sustainability Scoring
# =============================================================================

#' Normalize Data (Min-Max)
#'
#' Min-max normalization to scale values between 0 and 1.
#'
#'
#' @details Formula: \deqn{X_{norm} = \frac{X - X_{min}}{X_{max} - X_{min}}}{X_norm = (X - X_min) / (X_max - X_min)}
#'
#' @param x Numeric vector.
#' @param inverse Logical. If \code{TRUE}, higher original values receive
#'   lower normalized scores. Use for metrics where lower is better,
#'   such as carbon footprint or energy intensity. Default \code{FALSE}.
#'
#' @return Numeric vector normalized to the 0 to 1 range. Returns 0.5
#'   for all elements if the input has zero range.
#'
#' @examples
#' normalize_minmax(c(10, 20, 30, 40, 50))
#' normalize_minmax(c(10, 20, 30, 40, 50), inverse = TRUE)
#'
#' @export
normalize_minmax <- function(x, inverse = FALSE) {
  .validate_numeric(x, "x")
  x_range <- range(x, na.rm = TRUE)
  if (x_range[1] == x_range[2]) return(rep(0.5, length(x)))
  norm <- (x - x_range[1]) / (x_range[2] - x_range[1])
  if (inverse) norm <- 1 - norm
  return(round(norm, 4))
}

#' Normalize Data (Z-score)
#'
#' Standardization to z-scores (mean = 0, standard deviation = 1).
#'
#'
#' @details Formula: \deqn{Z = \frac{X - \mu}{\sigma}}{Z = (X - mean) / sd}
#'
#' @param x Numeric vector.
#'
#' @return Numeric vector of z-scores. Returns all zeros if standard
#'   deviation is zero.
#'
#' @examples
#' normalize_zscore(c(10, 20, 30, 40, 50))
#'
#' @export
normalize_zscore <- function(x) {
  .validate_numeric(x, "x")
  s <- stats::sd(x, na.rm = TRUE)
  if (s == 0) return(rep(0, length(x)))
  return(round((x - mean(x, na.rm = TRUE)) / s, 4))
}

#' WEFNC Nexus Index
#'
#' Computes a weighted composite Water-Energy-Food-Nutrient-Carbon
#' nexus sustainability index from normalized dimension scores.
#'
#' @param water_score Numeric vector. Water performance score (0 to 1).
#' @param energy_score Numeric vector. Energy performance score (0 to 1).
#' @param food_score Numeric vector. Food productivity score (0 to 1).
#' @param nutrient_score Numeric vector. Nutrient use efficiency score
#'   (0 to 1).
#' @param carbon_score Numeric vector. Carbon performance score (0 to 1),
#'   where higher values indicate lower carbon footprint.
#' @param weights Numeric vector of length 5. Weights for Water, Energy,
#'   Food, Nutrient, and Carbon dimensions. Must sum to 1. Default is
#'   equal weighting (0.2 each).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @return Numeric vector. Composite nexus index on a 0 to 1 scale,
#'   where higher values indicate better overall sustainability.
#'
#' @details
#' \deqn{NI = w_W S_W + w_E S_E + w_F S_F + w_N S_N + w_C S_C}
#'
#' @examples
#' \donttest{
#' nexus_index(water_score = c(0.85, 0.72), energy_score = c(0.70, 0.80),
#'             food_score = c(0.90, 0.85), nutrient_score = c(0.65, 0.70),
#'             carbon_score = c(0.80, 0.60))
#'
#' }
#' @export
nexus_index <- function(water_score, energy_score, food_score,
                        nutrient_score, carbon_score,
                        weights = rep(0.2, 5), verbose = getOption("wefnexus.verbose", TRUE)) {
  .validate_weights(weights, 5)
  sc <- list(water_score = water_score, energy_score = energy_score,
             food_score = food_score, nutrient_score = nutrient_score,
             carbon_score = carbon_score)
  for (nm in names(sc)) .validate_proportion(sc[[nm]], nm)
  lens <- lengths(sc)
  if (length(unique(lens[lens != 1])) > 1) {
    stop("All score vectors must have the same length.", call. = FALSE)
  }
  ni <- weights[1] * water_score + weights[2] * energy_score +
    weights[3] * food_score + weights[4] * nutrient_score +
    weights[5] * carbon_score
  if (verbose) message("Nexus index computed: ", length(ni), " values.")
  return(round(ni, 4))
}

#' Nexus Trade-off Matrix
#'
#' Computes pairwise correlation matrix among nexus dimensions to
#' identify synergies (positive) and trade-offs (negative).
#'
#' @param data A data frame with numeric columns for each nexus
#'   dimension (e.g., WUE, EUE, yield, NUE, carbon efficiency).
#' @param method Character. Correlation method: \code{"pearson"}
#'   (default), \code{"spearman"}, or \code{"kendall"}.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @return A numeric correlation matrix with pairwise correlations.
#'   When all indicators are higher-is-better, positive values suggest
#'   synergies and negative values trade-offs; for a lower-is-better
#'   indicator (such as carbon footprint) the reading reverses. The p-values of the pairwise
#'   correlation tests (\code{\link[stats]{cor.test}}) and the number of
#'   complete pairs are attached as \code{attr(, "p_values")} and
#'   \code{attr(, "n")}. The result has class \code{tradeoff_matrix} and
#'   behaves as an ordinary matrix; \code{plot()} or
#'   \code{\link{plot_tradeoff}} draws it.
#'
#' @examples
#' \donttest{
#' df <- data.frame(WUE = c(8.5, 7.2, 6.5, 9.0, 7.8),
#'                  EUE = c(11.2, 12.5, 9.8, 10.5, 11.0),
#'                  Yield = c(1500, 1380, 1250, 1650, 1580),
#'                  NUE = c(25, 23, 20.8, 27.5, 26.3),
#'                  CF = c(2500, 3000, 1800, 2800, 2200))
#' nexus_tradeoff(df)
#'
#' }
#' @export
nexus_tradeoff <- function(data, method = "pearson", verbose = getOption("wefnexus.verbose", TRUE)) {
  if (!is.data.frame(data)) stop("'data' must be a data frame.", call. = FALSE)
  method <- match.arg(method, c("pearson", "spearman", "kendall"))
  num_cols <- sapply(data, is.numeric)
  if (sum(num_cols) < 2) {
    stop("Need at least 2 numeric columns.", call. = FALSE)
  }
  x <- data[, num_cols, drop = FALSE]
  cor_mat <- stats::cor(x, use = "pairwise.complete.obs", method = method)
  k <- ncol(x)
  p_mat <- matrix(NA_real_, k, k, dimnames = dimnames(cor_mat))
  n_mat <- matrix(NA_integer_, k, k, dimnames = dimnames(cor_mat))
  for (i in seq_len(k)) {
    for (j in seq_len(k)) {
      ok <- stats::complete.cases(x[[i]], x[[j]])
      n_mat[i, j] <- sum(ok)
      if (i != j && sum(ok) > 2 &&
          stats::sd(x[[i]][ok]) > 0 && stats::sd(x[[j]][ok]) > 0) {
        p_mat[i, j] <- suppressWarnings(
          stats::cor.test(x[[i]][ok], x[[j]][ok], method = method)$p.value)
      }
    }
  }
  if (verbose) message("Trade-off matrix computed (", method, " method).")
  out <- round(cor_mat, 3)
  attr(out, "p_values") <- signif(p_mat, 4)
  attr(out, "n") <- n_mat
  class(out) <- c("tradeoff_matrix", class(cor_mat))
  return(out)
}

#' Nexus Radar Plot
#'
#' Creates a radar (spider/web) chart showing the WEFNC nexus profile
#' of one or more treatments.
#'
#' @param scores A matrix or data frame where rows represent treatments
#'   and columns represent nexus dimensions. Values should be on a
#'   0 to 1 scale.
#' @param labels Character vector. Names for nexus dimensions.
#'   Default: Water, Energy, Food, Nutrient, Carbon.
#' @param treatment_names Character vector. Labels for each treatment.
#' @param colors Character vector. Colors for each treatment. Default is
#'   a colour-blind-checked palette (up to 8 treatments).
#' @param title Character. Plot title. Default "WEFNC Nexus Profile".
#' @param fill Logical. If \code{TRUE}, fills the radar polygon area.
#'   Default \code{TRUE}.
#' @param alpha Numeric. Fill transparency (0 to 1). Default 0.2.
#'
#' @return Invisibly returns the input \code{scores} matrix. Called
#'   primarily for the side effect of generating a radar plot.
#'
#' @examples
#' \donttest{
#' scores <- matrix(c(0.85, 0.70, 0.90, 0.65, 0.80,
#'                     0.72, 0.80, 0.85, 0.70, 0.60),
#'                  nrow = 2, byrow = TRUE)
#' nexus_radar(scores,
#'   treatment_names = c("CA+SSDI", "CT+Flood"))
#'
#' }
#' @export
nexus_radar <- function(scores,
                        labels = c("Water", "Energy", "Food",
                                   "Nutrient", "Carbon"),
                        treatment_names = NULL, colors = NULL,
                        title = "WEFNC Nexus Profile",
                        fill = TRUE, alpha = 0.2) {
  if (is.data.frame(scores)) scores <- as.matrix(scores)
  if (ncol(scores) != length(labels)) {
    stop("Number of columns must match length of 'labels'.", call. = FALSE)
  }
  n_treat <- nrow(scores)
  n_dims <- ncol(scores)
  if (is.null(treatment_names)) {
    treatment_names <- paste("T", seq_len(n_treat))
  }
  if (is.null(colors)) colors <- .group_cols(n_treat)
  # First dimension at the top, then clockwise
  angles <- pi / 2 - seq(0, 2 * pi, length.out = n_dims + 1)[-(n_dims + 1)]
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))
  graphics::par(mar = c(2, 2, 3, 2))
  graphics::plot(0, 0, type = "n", xlim = c(-1.4, 1.4),
                 ylim = c(-1.3, 1.3), asp = 1, axes = FALSE,
                 xlab = "", ylab = "")
  graphics::title(main = title, cex.main = 1.2)
  for (r in seq(0.2, 1.0, by = 0.2)) {
    theta <- seq(0, 2 * pi, length.out = 100)
    graphics::lines(r * cos(theta), r * sin(theta),
                    col = .wef_ink[["grid"]], lwd = 0.8)
    graphics::text(0, r + 0.03, labels = r, cex = 0.6,
                   col = .wef_ink[["muted"]])
  }
  for (i in seq_len(n_dims)) {
    graphics::lines(c(0, cos(angles[i])), c(0, sin(angles[i])),
                    col = .wef_ink[["grid"]], lwd = 0.8)
    lx <- 1.2 * cos(angles[i])
    ly <- 1.2 * sin(angles[i])
    graphics::text(lx, ly, labels[i], cex = 0.9, font = 2,
                   col = .wef_ink[["secondary"]])
  }
  for (j in seq_len(n_treat)) {
    vals <- scores[j, ]
    x <- c(vals * cos(angles), vals[1] * cos(angles[1]))
    y <- c(vals * sin(angles), vals[1] * sin(angles[1]))
    if (fill) {
      col_rgb <- grDevices::col2rgb(colors[j]) / 255
      graphics::polygon(x, y,
                        col = grDevices::rgb(col_rgb[1], col_rgb[2],
                                             col_rgb[3], alpha),
                        border = NA)
    }
    graphics::lines(x, y, col = colors[j], lwd = 2)
    graphics::points(x[-length(x)], y[-length(y)],
                     col = colors[j], pch = 19, cex = 1)
  }
  graphics::legend("bottomright", legend = treatment_names,
                   col = colors, lwd = 2, pch = 19, cex = 0.8, bty = "n")
  invisible(scores)
}

#' Nexus Heatmap
#'
#' Creates a heatmap showing nexus dimension scores across treatments.
#'
#' @param scores A matrix or data frame where rows represent treatments
#'   and columns represent nexus dimensions.
#' @param row_labels Character vector. Treatment names.
#' @param col_labels Character vector. Dimension names.
#' @param title Character. Plot title. Default "WEFNC Nexus Heatmap".
#' @param color_palette Character vector of colors for gradient.
#'   Default is a single-hue blue scale that stays readable with colour
#'   vision deficiency (version 1.0.0 used red-yellow-green).
#'
#' @return Invisibly returns the input \code{scores} matrix. Called
#'   primarily for the side effect of generating a heatmap plot.
#'
#' @examples
#' \donttest{
#' scores <- matrix(c(0.85, 0.70, 0.90, 0.65, 0.80,
#'                     0.72, 0.80, 0.85, 0.70, 0.60),
#'                  nrow = 2, byrow = TRUE)
#' nexus_heatmap(scores,
#'   row_labels = c("Conservation", "Conventional"),
#'   col_labels = c("Water", "Energy", "Food", "Nutrient", "Carbon"))
#'
#' }
#' @export
nexus_heatmap <- function(scores, row_labels = NULL, col_labels = NULL,
                          title = "WEFNC Nexus Heatmap",
                          color_palette = NULL) {
  if (is.data.frame(scores)) scores <- as.matrix(scores)
  n_row <- nrow(scores)
  n_col <- ncol(scores)
  if (is.null(row_labels)) row_labels <- paste("T", seq_len(n_row))
  if (is.null(col_labels)) col_labels <- paste("D", seq_len(n_col))
  if (length(row_labels) != n_row || length(col_labels) != n_col) {
    stop("'row_labels' and 'col_labels' must match the rows and columns ",
         "of 'scores'.", call. = FALSE)
  }
  if (is.null(color_palette)) {
    color_palette <- grDevices::colorRampPalette(.wef_seq)(100)
  }
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))
  graphics::par(mar = c(5, 8, 4, 6))
  graphics::plot(NA, xlim = c(0.5, n_col + 0.5),
                 ylim = c(0.5, n_row + 0.5),
                 xlab = "", ylab = "", axes = FALSE, main = title)
  for (i in seq_len(n_row)) {
    for (j in seq_len(n_col)) {
      val <- scores[i, j]
      col_idx <- if (is.na(val)) NA else max(1, min(100, round(val * 100)))
      graphics::rect(j - 0.5, n_row - i + 0.5,
                     j + 0.5, n_row - i + 1.5,
                     col = color_palette[col_idx],
                     border = "white", lwd = 2)
      graphics::text(j, n_row - i + 1, round(val, 2), cex = 0.9,
                     col = if (!is.na(val) && val > 0.6) "white" else
                       .wef_ink[["primary"]])
    }
  }
  graphics::axis(1, at = seq_len(n_col), labels = col_labels,
                 tick = FALSE, las = 2, cex.axis = 0.9)
  graphics::axis(2, at = seq_len(n_row), labels = rev(row_labels),
                 tick = FALSE, las = 1, cex.axis = 0.9)
  invisible(scores)
}

#' Nexus Sustainability Score
#'
#' Comprehensive sustainability score with categorization based on
#' user-defined thresholds.
#'
#'
#' @details Formula: \deqn{NSS = \sum_{i=1}^{5} w_i S_i}{NSS = sum(w_i * S_i)}
#'   where \eqn{S_i} are the dimension scores and \eqn{w_i} the weights
#'   (equal weights give the mean).
#'
#' @param water_score Numeric vector. Water dimension score (0 to 1).
#' @param energy_score Numeric vector. Energy dimension score (0 to 1).
#' @param food_score Numeric vector. Food dimension score (0 to 1).
#' @param nutrient_score Numeric vector. Nutrient dimension score (0 to 1).
#' @param carbon_score Numeric vector. Carbon dimension score (0 to 1).
#' @param weights Numeric vector of length 5. Default equal weights.
#' @param thresholds Numeric vector of length 3. Breakpoints for
#'   sustainability categories: c(low, medium, high). Default
#'   c(0.4, 0.6, 0.8).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{nexus_score}{Numeric. Composite sustainability score (0 to 1)}
#'   \item{category}{Character. Sustainability category: Unsustainable,
#'     Low Sustainability, Moderately Sustainable, or Highly Sustainable}
#'   \item{water}{Numeric. Water dimension input score}
#'   \item{energy}{Numeric. Energy dimension input score}
#'   \item{food}{Numeric. Food dimension input score}
#'   \item{nutrient}{Numeric. Nutrient dimension input score}
#'   \item{carbon}{Numeric. Carbon dimension input score}
#' }
#'
#' @examples
#' \donttest{
#' nexus_sustainability_score(
#'   water_score = c(0.85, 0.60, 0.45),
#'   energy_score = c(0.70, 0.55, 0.35),
#'   food_score = c(0.90, 0.80, 0.50),
#'   nutrient_score = c(0.65, 0.70, 0.40),
#'   carbon_score = c(0.80, 0.50, 0.30)
#' )
#'
#' }
#' @export
nexus_sustainability_score <- function(water_score, energy_score,
                                       food_score, nutrient_score,
                                       carbon_score,
                                       weights = rep(0.2, 5),
                                       thresholds = c(0.4, 0.6, 0.8),
                                       verbose = getOption("wefnexus.verbose", TRUE)) {
  ni <- nexus_index(water_score, energy_score, food_score,
                    nutrient_score, carbon_score, weights,
                    verbose = FALSE)
  category <- ifelse(ni >= thresholds[3], "Highly Sustainable",
                     ifelse(ni >= thresholds[2], "Moderately Sustainable",
                            ifelse(ni >= thresholds[1], "Low Sustainability",
                                   "Unsustainable")))
  result <- data.frame(
    nexus_score = ni, category = category,
    water = water_score, energy = energy_score,
    food = food_score, nutrient = nutrient_score,
    carbon = carbon_score, stringsAsFactors = FALSE
  )
  if (verbose) message("Sustainability scores with categories computed.")
  class(result) <- c("sustainability_result", "data.frame")
  return(result)
}

#' Nexus Summary
#'
#' Generates a comprehensive one-call nexus analysis from raw
#' agronomic field data, computing all dimension metrics, normalizing,
#' and producing the composite index.
#'
#' @param yield Numeric vector. Crop yield (kg/ha).
#' @param water_consumed Numeric vector. Total water consumed (mm).
#' @param energy_input Numeric vector. Total energy input (MJ/ha).
#' @param energy_output Numeric vector. Total energy output (MJ/ha).
#' @param n_applied Numeric vector. Nitrogen applied (kg/ha). Needed when
#'   \code{nutrient_metric = "pfp"}. May be \code{NULL} when
#'   \code{nutrient_metric = "iue"}.
#' @param n_uptake Numeric vector. Total plant nitrogen uptake (kg/ha).
#'   Needed when \code{nutrient_metric = "iue"}; when supplied, internal
#'   utilization efficiency is also reported.
#' @param carbon_emission Numeric vector. Total GHG emission
#'   (kg CO2-eq/ha).
#' @param treatment_names Character vector. Treatment labels.
#'   Default \code{NULL}.
#' @param weights Numeric vector of length 5 (Water, Energy, Food,
#'   Nutrient, Carbon) summing to 1, or one of \code{"equal"},
#'   \code{"entropy"}, \code{"critic"} or \code{"pca"} to derive
#'   weights with \code{\link{nexus_weights}}. Default equal weights.
#' @param nutrient_metric Character. Indicator for the nutrient dimension:
#'   \code{"pfp"} (partial factor productivity of applied N, default) or
#'   \code{"iue"} (internal utilization efficiency, yield per kg N
#'   uptake). Use \code{"iue"} for legumes grown without fertilizer N,
#'   where PFP cannot be computed.
#' @param carbon_basis Character. Indicator for the carbon dimension:
#'   \code{"emission"} (total emission per ha, default, as in version
#'   1.0.0) or \code{"intensity"} (emission per kg yield). Intensity
#'   avoids penalising high-yielding treatments for larger absolute
#'   emissions.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @details Each dimension indicator is min-max normalized across the
#'   treatments supplied (carbon is inverted, so lower emission scores
#'   higher). Scores are therefore relative to the set of treatments
#'   being compared; with only two treatments every score is 0 or 1.
#'   Min-max scaling stretches any spread to the full 0 to 1 range, so a
#'   small absolute difference (for example 2 percent in IUE) can look as
#'   large as a big one; check the raw indicators before interpreting.
#'   When every treatment receives the same N, PFP is proportional to
#'   yield, so the function warns and \code{nutrient_metric = "iue"} is
#'   the better choice.
#'
#' @return A data frame of class \code{nexus_result} with one row per
#'   treatment and columns:
#' \describe{
#'   \item{treatment}{Character. Treatment name}
#'   \item{yield_kg_ha}{Numeric. Crop yield}
#'   \item{WUE_kg_mm}{Numeric. Water use efficiency}
#'   \item{EUE_ratio}{Numeric. Energy use efficiency ratio}
#'   \item{EROI}{Numeric. Energy return on investment}
#'   \item{net_energy_MJ}{Numeric. Net energy balance}
#'   \item{PFP_kg_kgN}{Numeric. Partial factor productivity of N
#'     (\code{NA} when N applied is missing or zero)}
#'   \item{carbon_eff}{Numeric. Carbon efficiency}
#'   \item{carbon_intensity}{Numeric. Carbon intensity}
#'   \item{W_score}{Numeric. Normalized water score}
#'   \item{E_score}{Numeric. Normalized energy score}
#'   \item{F_score}{Numeric. Normalized food score}
#'   \item{N_score}{Numeric. Normalized nutrient score}
#'   \item{C_score}{Numeric. Normalized carbon score}
#'   \item{nexus_index}{Numeric. Composite nexus index}
#'   \item{IUE_kg_kgN}{Numeric. Internal utilization efficiency of N
#'     (\code{NA} when \code{n_uptake} is not supplied)}
#'   \item{rank}{Integer. Rank by nexus index (1 = best)}
#' }
#' The weights used are stored in \code{attr(result, "weights")}.
#'
#' @examples
#' \donttest{
#' nexus_summary(
#'   yield = c(4500, 4200, 3800),
#'   water_consumed = c(450, 400, 350),
#'   energy_input = c(12000, 11000, 9500),
#'   energy_output = c(135000, 125000, 112000),
#'   n_applied = c(150, 120, 90),
#'   n_uptake = c(100, 90, 80),
#'   carbon_emission = c(2500, 2200, 1800),
#'   treatment_names = c("CA+Drip", "CT+Sprinkler", "ZT+Rainfed")
#' )
#'
#' # Legume without fertilizer N, carbon scored per kg grain
#' nexus_summary(
#'   yield = c(950, 870, 780),
#'   water_consumed = c(260, 240, 220),
#'   energy_input = c(5200, 4800, 4100),
#'   energy_output = c(36000, 33000, 29500),
#'   n_applied = c(0, 0, 0),
#'   n_uptake = c(62, 57, 50),
#'   carbon_emission = c(610, 560, 470),
#'   nutrient_metric = "iue", carbon_basis = "intensity"
#' )
#' }
#' @export
nexus_summary <- function(yield, water_consumed, energy_input,
                          energy_output, n_applied = NULL, n_uptake = NULL,
                          carbon_emission, treatment_names = NULL,
                          weights = rep(0.2, 5),
                          nutrient_metric = c("pfp", "iue"),
                          carbon_basis = c("emission", "intensity"),
                          verbose = getOption("wefnexus.verbose", TRUE)) {
  nutrient_metric <- match.arg(nutrient_metric)
  carbon_basis <- match.arg(carbon_basis)
  .validate_positive(yield, "yield")
  n <- length(yield)
  if (is.null(treatment_names)) {
    treatment_names <- paste("Treatment", seq_len(n))
  }
  if (length(treatment_names) != n) {
    stop("'treatment_names' must have the same length as 'yield'.",
         call. = FALSE)
  }
  wue <- water_use_efficiency(yield, water_consumed, verbose = FALSE)
  eue <- energy_use_efficiency(energy_output, energy_input, verbose = FALSE)
  eroi_val <- eroi(energy_output, energy_input, verbose = FALSE)
  ne <- net_energy(energy_output, energy_input, verbose = FALSE)

  pfp <- rep(NA_real_, n)
  if (!is.null(n_applied)) {
    .validate_non_negative(n_applied, "n_applied")
    n_applied <- rep_len(n_applied, n)
    ok <- !is.na(n_applied) & n_applied > 0
    pfp[ok] <- round(yield[ok] / n_applied[ok], 2)
  }
  iue <- rep(NA_real_, n)
  if (!is.null(n_uptake)) {
    iue <- internal_utilization_efficiency(yield, n_uptake, verbose = FALSE)
  }
  if (nutrient_metric == "pfp") {
    if (anyNA(pfp)) {
      stop("PFP needs 'n_applied' > 0 for every treatment. For legumes ",
           "without fertilizer N, use nutrient_metric = \"iue\" with ",
           "'n_uptake'.", call. = FALSE)
    }
    nut <- pfp
    if (n >= 2 && length(unique(n_applied)) == 1) {
      warning("N applied is the same for every treatment, so PFP is ",
              "proportional to yield and the nutrient score repeats the ",
              "food score (yield then carries double weight). Consider ",
              "nutrient_metric = \"iue\".", call. = FALSE)
    }
  } else {
    if (is.null(n_uptake)) {
      stop("nutrient_metric = \"iue\" requires 'n_uptake'.", call. = FALSE)
    }
    nut <- iue
  }

  ce <- carbon_efficiency(yield, carbon_emission, verbose = FALSE)
  ci <- round(carbon_emission / yield, 4)
  w_norm <- normalize_minmax(wue)
  e_norm <- normalize_minmax(eue)
  f_norm <- normalize_minmax(yield)
  n_norm <- normalize_minmax(nut)
  c_norm <- normalize_minmax(if (carbon_basis == "emission")
    carbon_emission else ci, inverse = TRUE)

  if (is.character(weights)) {
    weights <- nexus_weights(
      data.frame(Water = w_norm, Energy = e_norm, Food = f_norm,
                 Nutrient = n_norm, Carbon = c_norm),
      method = weights, verbose = FALSE)
  }
  ni <- nexus_index(w_norm, e_norm, f_norm, n_norm, c_norm,
                    weights, verbose = FALSE)
  result <- data.frame(
    treatment = treatment_names, yield_kg_ha = yield,
    WUE_kg_mm = wue, EUE_ratio = eue, EROI = eroi_val,
    net_energy_MJ = ne, PFP_kg_kgN = pfp,
    carbon_eff = ce, carbon_intensity = ci,
    W_score = w_norm, E_score = e_norm, F_score = f_norm,
    N_score = n_norm, C_score = c_norm, nexus_index = ni,
    IUE_kg_kgN = iue,
    rank = as.integer(rank(-ni, ties.method = "min")),
    stringsAsFactors = FALSE
  )
  attr(result, "weights") <- stats::setNames(
    as.numeric(weights), c("Water", "Energy", "Food", "Nutrient", "Carbon"))
  if (verbose) message("Nexus summary computed for ", n, " treatments.")
  class(result) <- c("nexus_result", "data.frame")
  return(result)
}

#' Print method for tradeoff_matrix
#'
#' @param x An object of class \code{tradeoff_matrix} from
#'   \code{\link{nexus_tradeoff}}.
#' @param ... Additional arguments passed to \code{print}.
#'
#' @return Invisibly returns \code{x}. Called for the side effect of
#'   printing the correlation matrix.
#'
#' @export
print.tradeoff_matrix <- function(x, ...) {
  m <- matrix(as.numeric(x), nrow(x), ncol(x), dimnames = dimnames(x))
  print(m, ...)
  cat("p-values: attr(x, \"p_values\"); draw with plot(x) or plot_tradeoff(x)\n")
  invisible(x)
}

#' Plot method for tradeoff_matrix
#'
#' @param x An object of class \code{tradeoff_matrix}.
#' @param ... Passed to \code{\link{plot_tradeoff}}, for example
#'   \code{benefit}.
#'
#' @return A ggplot2 object.
#'
#' @export
plot.tradeoff_matrix <- function(x, ...) {
  plot_tradeoff(x, ...)
}
