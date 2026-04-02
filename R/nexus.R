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
#'   Default \code{TRUE}.
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
                        weights = rep(0.2, 5), verbose = TRUE) {
  .validate_weights(weights, 5)
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
#'   Default \code{TRUE}.
#'
#' @return A numeric correlation matrix with pairwise correlations.
#'   Positive values suggest synergies; negative values suggest
#'   trade-offs between dimensions.
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
nexus_tradeoff <- function(data, method = "pearson", verbose = TRUE) {
  if (!is.data.frame(data)) stop("'data' must be a data frame.", call. = FALSE)
  method <- match.arg(method, c("pearson", "spearman", "kendall"))
  num_cols <- sapply(data, is.numeric)
  if (sum(num_cols) < 2) {
    stop("Need at least 2 numeric columns.", call. = FALSE)
  }
  cor_mat <- stats::cor(data[, num_cols], use = "pairwise.complete.obs",
                        method = method)
  if (verbose) message("Trade-off matrix computed (", method, " method).")
  return(round(cor_mat, 3))
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
#' @param colors Character vector. Colors for each treatment.
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
  if (is.null(colors)) {
    colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#A65628", "#F781BF", "#999999")[seq_len(n_treat)]
  }
  angles <- seq(0, 2 * pi, length.out = n_dims + 1)[-(n_dims + 1)]
  angles <- angles - pi / 2
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
                    col = "grey80", lty = 2)
    graphics::text(0, r + 0.03, labels = r, cex = 0.6, col = "grey50")
  }
  for (i in seq_len(n_dims)) {
    graphics::lines(c(0, cos(angles[i])), c(0, sin(angles[i])),
                    col = "grey60")
    lx <- 1.2 * cos(angles[i])
    ly <- 1.2 * sin(angles[i])
    graphics::text(lx, ly, labels[i], cex = 0.9, font = 2)
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
#'   Default is a red-yellow-green palette.
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
  if (is.null(color_palette)) {
    color_palette <- grDevices::colorRampPalette(
      c("#D73027", "#FDAE61", "#FEE08B", "#A6D96A", "#1A9850"))(100)
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
      col_idx <- max(1, min(100, round(val * 100)))
      graphics::rect(j - 0.5, n_row - i + 0.5,
                     j + 0.5, n_row - i + 1.5,
                     col = color_palette[col_idx],
                     border = "white", lwd = 2)
      graphics::text(j, n_row - i + 1, round(val, 2),
                     cex = 0.9, font = 2)
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
#' @details Formula: \deqn{NSS = \frac{1}{n} \sum_{i=1}^{n} S_i}{NSS = mean(S_i)} where S_i are normalized sub-dimension scores.
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
#'   Default \code{TRUE}.
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
                                       verbose = TRUE) {
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
#' @param n_applied Numeric vector. Nitrogen applied (kg/ha).
#' @param n_uptake Numeric vector. Nitrogen total uptake (kg/ha).
#' @param carbon_emission Numeric vector. Total GHG emission
#'   (kg CO2-eq/ha).
#' @param treatment_names Character vector. Treatment labels.
#'   Default \code{NULL}.
#' @param weights Numeric vector of length 5. Dimension weights.
#'   Default equal weights.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return A data frame with one row per treatment and columns:
#' \describe{
#'   \item{treatment}{Character. Treatment name}
#'   \item{yield_kg_ha}{Numeric. Crop yield}
#'   \item{WUE_kg_mm}{Numeric. Water use efficiency}
#'   \item{EUE_ratio}{Numeric. Energy use efficiency ratio}
#'   \item{EROI}{Numeric. Energy return on investment}
#'   \item{net_energy_MJ}{Numeric. Net energy balance}
#'   \item{PFP_kg_kgN}{Numeric. Partial factor productivity of N}
#'   \item{carbon_eff}{Numeric. Carbon efficiency}
#'   \item{carbon_intensity}{Numeric. Carbon intensity}
#'   \item{W_score}{Numeric. Normalized water score}
#'   \item{E_score}{Numeric. Normalized energy score}
#'   \item{F_score}{Numeric. Normalized food score}
#'   \item{N_score}{Numeric. Normalized nutrient score}
#'   \item{C_score}{Numeric. Normalized carbon score}
#'   \item{nexus_index}{Numeric. Composite nexus index}
#' }
#'
#' @examples
#' \donttest{
#' nexus_summary(
#'   yield = c(4500, 4200, 3800),
#'   water_consumed = c(450, 400, 350),
#'   energy_input = c(12000, 11000, 9500),
#'   energy_output = c(135000, 125000, 112000),
#'   n_applied = c(120, 120, 120),
#'   n_uptake = c(100, 90, 80),
#'   carbon_emission = c(2500, 2200, 1800),
#'   treatment_names = c("CA+Drip", "CT+Sprinkler", "ZT+Rainfed")
#' )
#'
#' }
#' @export
nexus_summary <- function(yield, water_consumed, energy_input,
                          energy_output, n_applied, n_uptake,
                          carbon_emission, treatment_names = NULL,
                          weights = rep(0.2, 5), verbose = TRUE) {
  n <- length(yield)
  if (is.null(treatment_names)) {
    treatment_names <- paste("Treatment", seq_len(n))
  }
  wue <- water_use_efficiency(yield, water_consumed, verbose = FALSE)
  eue <- energy_use_efficiency(energy_output, energy_input, verbose = FALSE)
  eroi_val <- eroi(energy_output, energy_input, verbose = FALSE)
  ne <- net_energy(energy_output, energy_input, verbose = FALSE)
  pfp <- partial_factor_productivity(yield, n_applied, verbose = FALSE)
  ce <- carbon_efficiency(yield, carbon_emission, verbose = FALSE)
  ci <- round(carbon_emission / yield, 4)
  w_norm <- normalize_minmax(wue)
  e_norm <- normalize_minmax(eue)
  f_norm <- normalize_minmax(yield)
  n_norm <- normalize_minmax(pfp)
  c_norm <- normalize_minmax(carbon_emission, inverse = TRUE)
  ni <- nexus_index(w_norm, e_norm, f_norm, n_norm, c_norm,
                    weights, verbose = FALSE)
  result <- data.frame(
    treatment = treatment_names, yield_kg_ha = yield,
    WUE_kg_mm = wue, EUE_ratio = eue, EROI = eroi_val,
    net_energy_MJ = ne, PFP_kg_kgN = pfp,
    carbon_eff = ce, carbon_intensity = ci,
    W_score = w_norm, E_score = e_norm, F_score = f_norm,
    N_score = n_norm, C_score = c_norm, nexus_index = ni,
    stringsAsFactors = FALSE
  )
  if (verbose) message("Nexus summary computed for ", n, " treatments.")
  class(result) <- c("nexus_result", "data.frame")
  return(result)
}
