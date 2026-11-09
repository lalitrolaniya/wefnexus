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
#' @param facet Logical. If \code{TRUE}, draws one panel per treatment
#'   (small multiples), which stays readable with many treatments.
#'   Default \code{FALSE} (all treatments overlaid).
#' @param reference Logical. With \code{facet = TRUE}, draws the mean
#'   profile of all treatments as a grey outline in every panel for
#'   comparison. Default \code{TRUE}.
#' @param colours Character vector of colours, one per treatment. Default
#'   is a colour-blind-checked palette (up to 8 treatments).
#' @param ncol Integer. Number of panel columns when \code{facet = TRUE}.
#'
#' @return A ggplot2 object.
#'
#' @details Overlaid radars become hard to read beyond three or four
#'   treatments; use \code{facet = TRUE} for more.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   scores <- matrix(c(0.85, 0.70, 0.90, 0.65, 0.80,
#'                       0.72, 0.80, 0.85, 0.70, 0.60),
#'                    nrow = 2, byrow = TRUE)
#'   nexus_radar_gg(scores, treatment_names = c("CA", "CT"))
#'   nexus_radar_gg(scores, treatment_names = c("CA", "CT"), facet = TRUE)
#' }
#' }
#'
#' @export
nexus_radar_gg <- function(scores,
                           treatment_names = NULL,
                           dim_labels = c("Water", "Energy", "Food",
                                          "Nutrient", "Carbon"),
                           title = "WEFNC Nexus Profile",
                           facet = FALSE, reference = TRUE,
                           colours = NULL, ncol = NULL) {
  .need_ggplot()
  if (is.data.frame(scores)) scores <- as.matrix(scores)
  n_treat <- nrow(scores)
  n_dims <- ncol(scores)
  if (length(dim_labels) != n_dims) {
    stop("Length of 'dim_labels' must equal the number of columns.",
         call. = FALSE)
  }
  if (is.null(treatment_names)) {
    treatment_names <- paste("T", seq_len(n_treat))
  }
  if (length(treatment_names) != n_treat) {
    stop("'treatment_names' must have one label per row of 'scores'.",
         call. = FALSE)
  }
  if (is.null(colours)) colours <- .group_cols(n_treat)
  colours <- stats::setNames(rep_len(colours, n_treat), treatment_names)
  angles <- seq(0, 2 * pi, length.out = n_dims + 1)[seq_len(n_dims)]
  poly <- function(vals, label) {
    ang <- c(angles, angles[1])
    v <- c(vals, vals[1])
    data.frame(treatment = label, x = v * sin(ang), y = v * cos(ang),
               stringsAsFactors = FALSE)
  }
  plot_df <- do.call(rbind, lapply(seq_len(n_treat), function(i)
    poly(scores[i, ], treatment_names[i])))
  plot_df$treatment <- factor(plot_df$treatment, levels = treatment_names)
  pts <- plot_df[!duplicated(paste(plot_df$treatment, round(plot_df$x, 8),
                                   round(plot_df$y, 8))), ]
  theta <- seq(0, 2 * pi, length.out = 120)
  grid_df <- do.call(rbind, lapply(c(0.25, 0.5, 0.75, 1), function(r)
    data.frame(x = r * sin(theta), y = r * cos(theta), r = r)))
  spoke_df <- data.frame(x = 0, y = 0, xend = sin(angles), yend = cos(angles))
  label_df <- data.frame(label = dim_labels, x = 1.2 * sin(angles),
                         y = 1.2 * cos(angles),
                         hjust = ifelse(abs(sin(angles)) < 0.1, 0.5,
                                        ifelse(sin(angles) > 0, 0, 1)),
                         stringsAsFactors = FALSE)
  ring_lab <- data.frame(x = 0, y = c(-0.5, -1), lab = c("0.5", "1.0"))

  p <- ggplot2::ggplot() +
    ggplot2::geom_path(data = grid_df,
                       ggplot2::aes(x = .data$x, y = .data$y,
                                    group = .data$r),
                       colour = .wef_ink[["grid"]], linewidth = 0.35) +
    ggplot2::geom_segment(data = spoke_df,
                          ggplot2::aes(x = .data$x, y = .data$y,
                                       xend = .data$xend, yend = .data$yend),
                          colour = .wef_ink[["grid"]], linewidth = 0.35) +
    ggplot2::geom_text(data = ring_lab,
                       ggplot2::aes(x = .data$x, y = .data$y,
                                    label = .data$lab),
                       hjust = 0.5, vjust = 1.3, size = 2.6,
                       colour = .wef_ink[["muted"]])
  if (facet && reference && n_treat > 1) {
    ref <- poly(colMeans(scores, na.rm = TRUE), "Mean")
    p <- p + ggplot2::geom_path(data = ref[, c("x", "y")],
                                ggplot2::aes(x = .data$x, y = .data$y),
                                colour = .wef_ink[["muted"]],
                                linewidth = 0.5, linetype = "22")
  }
  p <- p +
    ggplot2::geom_polygon(data = plot_df,
                          ggplot2::aes(x = .data$x, y = .data$y,
                                       fill = .data$treatment,
                                       group = .data$treatment),
                          alpha = 0.12, colour = NA) +
    ggplot2::geom_path(data = plot_df,
                       ggplot2::aes(x = .data$x, y = .data$y,
                                    colour = .data$treatment,
                                    group = .data$treatment),
                       linewidth = 0.9, linejoin = "round") +
    ggplot2::geom_point(data = pts,
                        ggplot2::aes(x = .data$x, y = .data$y,
                                     fill = .data$treatment),
                        shape = 21, size = 2.3, stroke = 0.8,
                        colour = "white") +
    ggplot2::geom_text(data = label_df,
                       ggplot2::aes(x = .data$x, y = .data$y,
                                    label = .data$label,
                                    hjust = .data$hjust),
                       size = 3.2, colour = .wef_ink[["secondary"]]) +
    ggplot2::scale_colour_manual(values = colours, name = NULL) +
    ggplot2::scale_fill_manual(values = colours, name = NULL) +
    ggplot2::coord_equal(xlim = c(-1.55, 1.55), ylim = c(-1.3, 1.3),
                         clip = "off") +
    ggplot2::labs(title = title) +
    .theme_wef() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank())
  if (facet) {
    p <- p + ggplot2::facet_wrap(~ treatment, ncol = ncol) +
      ggplot2::theme(legend.position = "none",
                     strip.text = ggplot2::element_text(hjust = 0.5))
    if (reference && n_treat > 1) {
      p <- p + ggplot2::labs(caption = "Dashed grey: mean of all treatments.")
    }
  }
  p
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
#' @param colours Character vector of colours for the 0 to 1 gradient.
#'   Default is a single-hue blue scale that is readable with colour
#'   vision deficiency. The red-yellow-green scale of version 1.0.0 is
#'   \code{c("#D73027", "#FDAE61", "#FEE08B", "#A6D96A", "#1A9850")}.
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
                              title = "WEFNC Nexus Heatmap",
                              colours = NULL) {
  .need_ggplot()
  if (is.data.frame(scores)) scores <- as.matrix(scores)
  if (is.null(treatment_names)) {
    treatment_names <- paste("T", seq_len(nrow(scores)))
  }
  if (length(treatment_names) != nrow(scores)) {
    stop("'treatment_names' must have one label per row of 'scores'.",
         call. = FALSE)
  }
  if (length(dim_labels) != ncol(scores)) {
    stop("'dim_labels' must have one label per column of 'scores'.",
         call. = FALSE)
  }
  if (is.null(colours)) colours <- .wef_seq
  # One row per cell, built explicitly so each value stays with its own
  # treatment (1.0.0 reversed the rows relative to their labels).
  df <- data.frame(
    Dimension = factor(rep(dim_labels, times = nrow(scores)),
                       levels = dim_labels),
    Treatment = factor(rep(treatment_names, each = ncol(scores)),
                       levels = rev(treatment_names)),
    Value = as.vector(t(scores))
  )
  df$txt <- .text_on(.ramp_hex(df$Value, colours, c(0, 1)))
  ggplot2::ggplot(df, ggplot2::aes(x = .data$Dimension,
                                   y = .data$Treatment,
                                   fill = .data$Value)) +
    ggplot2::geom_tile(colour = "white", linewidth = 1.2) +
    ggplot2::geom_text(ggplot2::aes(label = formatC(.data$Value, format = "f",
                                                    digits = 2),
                                    colour = .data$txt),
                       size = 3.4) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_gradientn(colours = colours, limits = c(0, 1),
                                  name = "Score") +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    .theme_wef() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   legend.position = "right",
                   legend.key.height = ggplot2::unit(1, "cm"))
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
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
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
                              steps = 20, verbose = getOption("wefnexus.verbose", TRUE)) {
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
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_colour_manual(
      values = stats::setNames(.group_cols(length(unique(sa$treatment))),
                               unique(sa$treatment))) +
    ggplot2::facet_wrap(~ dimension, nrow = 1) +
    ggplot2::labs(title = title, x = "Dimension Weight",
                  y = "Nexus Index", colour = "Treatment") +
    .theme_wef()
  return(p)
}
