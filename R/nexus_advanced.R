# =============================================================================
# ADVANCED NEXUS FUNCTIONS
# ggplot2-based plots and weight sensitivity analysis
# =============================================================================

#' @importFrom rlang .data
NULL

# Suppress R CMD check NOTE for .data pronoun
utils::globalVariables(c(".data"))

#' Nexus Radar Plot (ggplot2)
#'
#' Publication-quality radar chart using ggplot2 for WEFNC nexus profiles.
#'
#' @param scores A matrix or data frame (treatments as rows, dimensions
#'   as columns). Values should be on a 0 to 1 scale.
#' @param treatment_names Character vector. Labels for treatments.
#' @param dim_labels Character vector. Labels for nexus dimensions.
#'   Default: Water, Energy, Food, Nutrient, Carbon.
#' @param title Character. Plot title.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   scores <- matrix(c(0.85, 0.70, 0.90, 0.65, 0.80,
#'                       0.72, 0.80, 0.85, 0.70, 0.60),
#'                    nrow = 2, byrow = TRUE)
#'   nexus_radar_gg(scores, treatment_names = c("CA", "CT"))
#' }
#' }
#'
#' @export
nexus_radar_gg <- function(scores,
                           treatment_names = NULL,
                           dim_labels = c("Water", "Energy", "Food",
                                          "Nutrient", "Carbon"),
                           title = "WEFNC Nexus Profile") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Install with install.packages('ggplot2').",
         call. = FALSE)
  }
  if (is.data.frame(scores)) scores <- as.matrix(scores)
  n_treat <- nrow(scores)
  n_dims <- ncol(scores)
  if (is.null(treatment_names)) {
    treatment_names <- paste("T", seq_len(n_treat))
  }
  angles <- seq(0, 2 * pi, length.out = n_dims + 1)[seq_len(n_dims)]
  df_list <- list()
  for (i in seq_len(n_treat)) {
    vals <- c(scores[i, ], scores[i, 1])
    ang <- c(angles, angles[1])
    df_list[[i]] <- data.frame(
      treatment = treatment_names[i],
      angle = ang,
      value = vals,
      x = vals * cos(ang - pi / 2),
      y = vals * sin(ang - pi / 2),
      stringsAsFactors = FALSE
    )
  }
  plot_df <- do.call(rbind, df_list)
  label_df <- data.frame(
    label = dim_labels,
    x = 1.15 * cos(angles - pi / 2),
    y = 1.15 * sin(angles - pi / 2),
    stringsAsFactors = FALSE
  )
  grid_list <- list()
  for (r in seq(0.2, 1.0, by = 0.2)) {
    theta <- seq(0, 2 * pi, length.out = 100)
    grid_list[[length(grid_list) + 1]] <- data.frame(
      x = r * cos(theta), y = r * sin(theta), r = r)
  }
  grid_df <- do.call(rbind, grid_list)
  p <- ggplot2::ggplot() +
    ggplot2::geom_path(data = grid_df,
                       ggplot2::aes(x = .data$x, y = .data$y,
                                    group = .data$r),
                       colour = "grey80", linewidth = 0.3) +
    ggplot2::geom_polygon(data = plot_df,
                          ggplot2::aes(x = .data$x, y = .data$y,
                                       fill = .data$treatment,
                                       colour = .data$treatment,
                                       group = .data$treatment),
                          alpha = 0.15, linewidth = 1) +
    ggplot2::geom_point(data = plot_df,
                        ggplot2::aes(x = .data$x, y = .data$y,
                                     colour = .data$treatment),
                        size = 2) +
    ggplot2::geom_text(data = label_df,
                       ggplot2::aes(x = .data$x, y = .data$y,
                                    label = .data$label),
                       fontface = "bold", size = 3.5) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = title, fill = "Treatment",
                  colour = "Treatment") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold",
                                          size = 14),
      legend.position = "bottom"
    )
  return(p)
}

#' Nexus Heatmap (ggplot2)
#'
#' Publication-quality heatmap using ggplot2 for WEFNC nexus scores.
#'
#' @param scores A matrix or data frame (treatments as rows, dimensions
#'   as columns).
#' @param treatment_names Character vector. Row labels.
#' @param dim_labels Character vector. Column labels.
#'   Default: Water, Energy, Food, Nutrient, Carbon.
#' @param title Character. Plot title.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   scores <- matrix(c(0.85, 0.70, 0.90, 0.65, 0.80,
#'                       0.72, 0.80, 0.85, 0.70, 0.60),
#'                    nrow = 2, byrow = TRUE)
#'   nexus_heatmap_gg(scores, treatment_names = c("CA", "CT"))
#' }
#' }
#'
#' @export
nexus_heatmap_gg <- function(scores,
                              treatment_names = NULL,
                              dim_labels = c("Water", "Energy", "Food",
                                             "Nutrient", "Carbon"),
                              title = "WEFNC Nexus Heatmap") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Install with install.packages('ggplot2').",
         call. = FALSE)
  }
  if (is.data.frame(scores)) scores <- as.matrix(scores)
  if (is.null(treatment_names)) {
    treatment_names <- paste("T", seq_len(nrow(scores)))
  }
  df <- expand.grid(
    Dimension = factor(dim_labels, levels = dim_labels),
    Treatment = factor(treatment_names, levels = rev(treatment_names))
  )
  df$Value <- as.vector(t(scores[rev(seq_len(nrow(scores))), ]))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Dimension,
                                          y = .data$Treatment,
                                          fill = .data$Value)) +
    ggplot2::geom_tile(colour = "white", linewidth = 1.5) +
    ggplot2::geom_text(ggplot2::aes(label = round(.data$Value, 2)),
                       size = 4, fontface = "bold") +
    ggplot2::scale_fill_gradientn(
      colours = c("#D73027", "#FDAE61", "#FEE08B", "#A6D96A", "#1A9850"),
      limits = c(0, 1), name = "Score") +
    ggplot2::labs(title = title, x = "", y = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold",
                                          size = 14),
      axis.text = ggplot2::element_text(size = 11),
      panel.grid = ggplot2::element_blank()
    )
  return(p)
}

#' Nexus Weight Sensitivity Analysis
#'
#' Evaluates how the composite nexus index changes as the weight of one
#' dimension varies from 0 to its maximum feasible value, with remaining
#' weight distributed equally among the other four dimensions.
#'
#' @param water_score Numeric vector. Water scores (0 to 1).
#' @param energy_score Numeric vector. Energy scores (0 to 1).
#' @param food_score Numeric vector. Food scores (0 to 1).
#' @param nutrient_score Numeric vector. Nutrient scores (0 to 1).
#' @param carbon_score Numeric vector. Carbon scores (0 to 1).
#' @param treatment_names Character vector. Treatment labels.
#' @param steps Integer. Number of weight steps to evaluate.
#'   Default 20.
#' @param verbose Logical. If \code{TRUE}, prints a message.
#'   Default \code{TRUE}.
#'
#' @return A data frame with columns: dimension, weight, treatment,
#'   and nexus_index showing how the index changes as each dimension's
#'   weight varies.
#'
#' @examples
#' \donttest{
#' sa <- nexus_sensitivity(
#'   water_score = c(0.9, 0.5), energy_score = c(0.6, 0.8),
#'   food_score = c(0.8, 0.7), nutrient_score = c(0.7, 0.6),
#'   carbon_score = c(0.5, 0.9),
#'   treatment_names = c("CA", "CT"), steps = 10
#' )
#' head(sa)
#' }
#'
#' @export
nexus_sensitivity <- function(water_score, energy_score, food_score,
                              nutrient_score, carbon_score,
                              treatment_names = NULL,
                              steps = 20, verbose = TRUE) {
  n <- length(water_score)
  if (is.null(treatment_names)) {
    treatment_names <- paste("T", seq_len(n))
  }
  dim_names <- c("Water", "Energy", "Food", "Nutrient", "Carbon")
  all_scores <- cbind(water_score, energy_score, food_score,
                      nutrient_score, carbon_score)
  weight_seq <- seq(0, 1, length.out = steps + 1)
  results <- list()
  for (d in seq_along(dim_names)) {
    for (w in weight_seq) {
      remaining <- (1 - w) / 4
      wts <- rep(remaining, 5)
      wts[d] <- w
      ni <- as.numeric(all_scores %*% wts)
      results[[length(results) + 1]] <- data.frame(
        dimension = dim_names[d],
        weight = round(w, 3),
        treatment = treatment_names,
        nexus_index = round(ni, 4),
        stringsAsFactors = FALSE
      )
    }
  }
  result <- do.call(rbind, results)
  if (verbose) {
    message("Sensitivity analysis: ", steps + 1,
            " weight steps x 5 dimensions x ", n, " treatments.")
  }
  return(result)
}

#' Plot Nexus Sensitivity Analysis (ggplot2)
#'
#' Visualises the output of \code{\link{nexus_sensitivity}} as faceted
#' line plots showing how the nexus index changes with dimension weights.
#'
#' @param sa Data frame returned by \code{\link{nexus_sensitivity}}.
#' @param title Character. Plot title.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   sa <- nexus_sensitivity(
#'     water_score = c(0.9, 0.5), energy_score = c(0.6, 0.8),
#'     food_score = c(0.8, 0.7), nutrient_score = c(0.7, 0.6),
#'     carbon_score = c(0.5, 0.9),
#'     treatment_names = c("CA", "CT"), steps = 10
#'   )
#'   plot_sensitivity(sa)
#' }
#' }
#'
#' @export
plot_sensitivity <- function(sa, title = "Nexus Weight Sensitivity") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.", call. = FALSE)
  }
  p <- ggplot2::ggplot(sa, ggplot2::aes(x = .data$weight,
                                          y = .data$nexus_index,
                                          colour = .data$treatment)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 1) +
    ggplot2::facet_wrap(~ dimension, nrow = 1) +
    ggplot2::labs(title = title, x = "Dimension Weight",
                  y = "Nexus Index", colour = "Treatment") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      strip.text = ggplot2::element_text(face = "bold")
    )
  return(p)
}
