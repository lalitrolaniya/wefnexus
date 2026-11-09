# =============================================================================
# PUBLICATION-READY FIGURES (ggplot2)
# Shared palette and theme, plot methods for result objects, trade-off heatmap
# =============================================================================

# Colour-blind-checked categorical palette (fixed order, never cycled),
# text inks and a single-hue sequential ramp.
.wef_cols <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
               "#e87ba4", "#008300", "#6250d6", "#e34948")
.wef_ink <- c(primary = "#0b0b0b", secondary = "#52514e",
              muted = "#898781", grid = "#e1e0d9", axis = "#c3c2b7")
.wef_seq <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf",
              "#184f95", "#0d366b")
.wef_div <- c(low = "#e34948", mid = "#f0efec", high = "#2a78d6")

# Text colour (near-black or white) with the higher contrast on each fill.
.text_on <- function(fill) {
  rgb <- grDevices::col2rgb(fill) / 255
  lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  L <- 0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ]
  ifelse((1.05 / (L + 0.05)) > ((L + 0.05) / 0.05), "white",
         .wef_ink[["primary"]])
}

# Fill colour of values v on a gradient (Lab interpolation, as ggplot2).
.ramp_hex <- function(v, colours, limits) {
  t <- (pmin(pmax(v, limits[1]), limits[2]) - limits[1]) /
    (limits[2] - limits[1])
  t[is.na(t)] <- 0
  m <- grDevices::colorRamp(colours, space = "Lab")(t)
  grDevices::rgb(m[, 1], m[, 2], m[, 3], maxColorValue = 255)
}

.need_ggplot <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this plot. Install it with ",
         "install.packages(\"ggplot2\").", call. = FALSE)
  }
}

# Colours for n groups. Up to 8 use the fixed palette; beyond that every
# group gets the first colour and identity is carried by labels or facets.
.group_cols <- function(n) {
  if (n <= length(.wef_cols)) .wef_cols[seq_len(n)] else rep(.wef_cols[1], n)
}

.theme_wef <- function(base_size = 11) {
  ink <- .wef_ink
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = ink[["primary"]]),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0,
                                         size = ggplot2::rel(1.15)),
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(colour = ink[["secondary"]],
                                            hjust = 0),
      plot.caption = ggplot2::element_text(colour = ink[["muted"]],
                                           hjust = 0,
                                           size = ggplot2::rel(0.8)),
      plot.caption.position = "plot",
      axis.text = ggplot2::element_text(colour = ink[["secondary"]]),
      axis.title = ggplot2::element_text(colour = ink[["secondary"]]),
      panel.grid.major = ggplot2::element_line(colour = ink[["grid"]],
                                               linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0,
                                         colour = ink[["primary"]]),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(colour = ink[["secondary"]]),
      legend.text = ggplot2::element_text(colour = ink[["primary"]]),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(10, 14, 10, 10)
    )
}

#' Plot method for nexus_anova
#'
#' Draws treatment means as horizontal bars with error bars and the
#' letter groups from \code{\link{nexus_anova}}, one panel per response.
#'
#' @param x An object of class \code{nexus_anova}.
#' @param response Character vector of responses to plot. Default all.
#' @param error Character. Error bars: \code{"se"} (standard error of
#'   each treatment mean, default), \code{"sem"} (pooled SEm from the
#'   ANOVA) or \code{"none"}.
#' @param labels Optional named character vector to relabel panels, for
#'   example \code{c(grain_yield = "Grain yield (kg/ha)")}.
#' @param ncol Integer. Number of panel columns. Default chosen
#'   automatically.
#' @param title Character. Plot title. Default \code{NULL} (none).
#' @param ... Unused.
#'
#' @return A ggplot2 object.
#'
#' @details Panels for responses whose treatment F test is not
#'   significant are marked "(ns)" and show no letters.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   data(arid_pulse_trial)
#'   res <- nexus_anova(arid_pulse_trial,
#'                      response = c("grain_yield", "n_uptake"),
#'                      treatment = "treatment", block = "replication",
#'                      verbose = FALSE)
#'   plot(res, labels = c(grain_yield = "Grain yield (kg/ha)",
#'                        n_uptake = "N uptake (kg/ha)"))
#' }
#'
#' @export
plot.nexus_anova <- function(x, response = NULL,
                             error = c("se", "sem", "none"),
                             labels = NULL, ncol = NULL, title = NULL,
                             ...) {
  .need_ggplot()
  error <- match.arg(error)
  resp <- if (is.null(response)) x$stats$response else response
  bad <- setdiff(resp, x$stats$response)
  if (length(bad) > 0) {
    stop("Unknown response(s): ", paste(bad, collapse = ", "), call. = FALSE)
  }
  trt <- x$means$treatment
  rows <- lapply(resp, function(r) {
    st <- x$stats[x$stats$response == r, ]
    err <- switch(error,
                  se = if (!is.null(x$se)) x$se[[r]] else rep(st$SEm,
                                                             length(trt)),
                  sem = rep(st$SEm, length(trt)),
                  none = rep(0, length(trt)))
    err[is.na(err)] <- 0
    lab <- if (!is.null(labels) && r %in% names(labels)) labels[[r]] else r
    if (!st$significant) lab <- paste(lab, "(ns)")
    data.frame(panel = lab, treatment = trt, mean = x$means[[r]],
               err = err,
               letter = if (st$significant) x$letters[[r]] else "",
               stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, rows)
  df$panel <- factor(df$panel, levels = unique(df$panel))
  df$treatment <- factor(df$treatment, levels = rev(trt))
  df$tip <- df$mean + df$err
  pad <- tapply(df$tip, df$panel, function(v) 0.03 * max(abs(v), na.rm = TRUE))
  df$label_x <- df$tip + pad[as.character(df$panel)]
  df$blank_x <- df$tip + 6 * pad[as.character(df$panel)]

  pm <- if (isTRUE(l10n_info()[["UTF-8"]])) "\u00b1" else "+/-"
  sub <- switch(error,
                se = paste("Bars: treatment means", pm, "standard error."),
                sem = paste("Bars: treatment means", pm, "pooled SEm."),
                none = "Bars: treatment means.")
  alpha <- if (!is.null(x$alpha)) x$alpha else 0.05
  cap <- sprintf(paste("Means sharing a letter do not differ",
                       "(Fisher's LSD, p = %s)."), alpha)

  p <- ggplot2::ggplot(df, ggplot2::aes(y = .data$treatment)) +
    ggplot2::geom_col(ggplot2::aes(x = .data$mean), fill = .wef_cols[1],
                      width = 0.6) +
    ggplot2::geom_blank(ggplot2::aes(x = .data$blank_x)) +
    ggplot2::labs(title = title, subtitle = sub, caption = cap,
                  x = NULL, y = NULL) +
    ggplot2::facet_wrap(~ panel, scales = "free_x", ncol = ncol) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.02))) +
    .theme_wef() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_line(
                     colour = .wef_ink[["axis"]], linewidth = 0.4))
  if (error != "none") {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data$mean - .data$err,
                   xmax = .data$mean + .data$err),
      width = 0.18, linewidth = 0.4, colour = .wef_ink[["primary"]])
  }
  p + ggplot2::geom_text(ggplot2::aes(x = .data$label_x,
                                      label = .data$letter),
                         hjust = 0, size = 3.4,
                         colour = .wef_ink[["primary"]])
}

#' Plot method for energy_budget
#'
#' Stacked horizontal bars of input energy for each treatment, split by
#' input item, by direct and indirect energy, or by renewable and
#' non-renewable energy.
#'
#' @param x An object of class \code{energy_budget}.
#' @param type Character. \code{"items"} (default), \code{"form"}
#'   (direct/indirect) or \code{"source"} (renewable/non-renewable).
#' @param percent Logical. If \code{TRUE}, bars show percentage shares
#'   instead of MJ/ha. Default \code{FALSE}.
#' @param top Integer. For \code{type = "items"}, the number of largest
#'   items shown individually; the rest are combined as "Other". Default
#'   7 (maximum 7).
#' @param title Character. Plot title. Default \code{NULL}.
#' @param ... Unused.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   inputs <- data.frame(
#'     treatment = c("CT", "ZT"),
#'     human_labour_man = c(320, 240), diesel = c(65, 38),
#'     electricity = c(180, 75), nitrogen = c(20, 20),
#'     phosphorus = c(40, 40), seed = c(20, 20), herbicide = c(0, 0.75)
#'   )
#'   eb <- energy_budget(inputs, verbose = FALSE)
#'   plot(eb)
#'   plot(eb, type = "source", percent = TRUE)
#' }
#'
#' @export
plot.energy_budget <- function(x, type = c("items", "form", "source"),
                               percent = FALSE, top = 7, title = NULL,
                               ...) {
  .need_ggplot()
  type <- match.arg(type)
  top <- max(1, min(as.integer(top), 7))
  d <- x$detail[x$detail$type == "input", , drop = FALSE]
  if (nrow(d) == 0) stop("No input items to plot.", call. = FALSE)
  rows <- unique(x$detail$row)
  if (type == "items") {
    tot <- tapply(d$energy_MJ, d$item, sum)
    tot <- tot[tot > 0]
    ord <- names(sort(tot, decreasing = TRUE))
    keep <- ord[seq_len(min(top, length(ord)))]
    d$group <- ifelse(d$item %in% keep, d$item, "Other")
    lev <- c(keep, if (any(d$group == "Other")) "Other")
    lab <- .pretty_item(lev)
  } else {
    d$group <- d[[type]]
    lev <- if (type == "form") c("direct", "indirect") else
      c("renewable", "non-renewable")
    lab <- if (type == "form") c("Direct", "Indirect") else
      c("Renewable", "Non-renewable")
  }
  agg <- stats::aggregate(energy_MJ ~ row + group, data = d, FUN = sum)
  agg <- agg[agg$energy_MJ > 0, , drop = FALSE]
  agg$group <- factor(agg$group, levels = rev(lev), labels = rev(lab))
  agg$row <- factor(agg$row, levels = rev(rows))
  cols <- stats::setNames(.group_cols(length(lev)), lab)
  if (type == "items" && "Other" %in% lev) {
    cols[["Other"]] <- .wef_ink[["axis"]]
  }
  p <- ggplot2::ggplot(agg, ggplot2::aes(x = .data$energy_MJ,
                                         y = .data$row,
                                         fill = .data$group)) +
    ggplot2::geom_col(width = 0.6, colour = "white", linewidth = 0.6,
                      position = if (percent) "fill" else "stack") +
    ggplot2::scale_fill_manual(values = cols, breaks = lab, name = NULL) +
    ggplot2::labs(title = title, y = NULL,
                  x = if (percent) "Share of input energy (%)" else
                    "Input energy (MJ/ha)") +
    .theme_wef() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = if (length(lev) > 4)
      2 else 1))
  if (percent) {
    p <- p + ggplot2::scale_x_continuous(
      labels = function(v) paste0(round(100 * v), "%"),
      expand = ggplot2::expansion(mult = c(0, 0.02)))
  } else {
    p <- p + ggplot2::scale_x_continuous(
      labels = function(v) format(v, big.mark = ",", scientific = FALSE,
                                  trim = TRUE),
      expand = ggplot2::expansion(mult = c(0, 0.04)))
  }
  p
}

#' Plot method for topsis_result
#'
#' Ranked horizontal bars of the TOPSIS closeness coefficient, with the
#' best treatment highlighted.
#'
#' @param x An object of class \code{topsis_result} from
#'   \code{\link{nexus_topsis}}.
#' @param highlight Integer. Number of top-ranked treatments to
#'   highlight. Default 1.
#' @param title Character. Plot title. Default \code{NULL}.
#' @param ... Unused.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   data(arid_pulse_nexus)
#'   d <- arid_pulse_nexus
#'   ind <- data.frame(WUE = d$grain_yield / d$total_water,
#'                     Yield = d$grain_yield, CF = d$ghg_emission)
#'   ts <- nexus_topsis(ind, benefit = c(TRUE, TRUE, FALSE),
#'                      treatment_names = d$treatment, verbose = FALSE)
#'   plot(ts)
#' }
#'
#' @export
plot.topsis_result <- function(x, highlight = 1, title = NULL, ...) {
  .need_ggplot()
  df <- as.data.frame(x)
  df <- df[order(df$closeness), , drop = FALSE]
  df$treatment <- factor(df$treatment, levels = df$treatment)
  df$top <- df$rank <= highlight
  df$lab <- formatC(df$closeness, format = "f", digits = 3)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$closeness,
                                   y = .data$treatment)) +
    ggplot2::geom_col(ggplot2::aes(fill = .data$top), width = 0.6,
                      show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = .data$lab), hjust = -0.15,
                       size = 3.3, colour = .wef_ink[["secondary"]]) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = .wef_cols[1],
                                          `FALSE` = "#9ec5f4")) +
    ggplot2::scale_x_continuous(limits = c(0, 1.08),
                                breaks = seq(0, 1, 0.25),
                                expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::labs(title = title, y = NULL,
                  x = "TOPSIS closeness to the ideal (0 to 1, higher is better)") +
    .theme_wef() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Plot a Nexus Trade-off Matrix
#'
#' Draws the correlation matrix from \code{\link{nexus_tradeoff}} as a
#' heatmap with a diverging scale (blue positive, red negative, grey near
#' zero). Cells show the coefficient and significance stars. Supply
#' \code{benefit} to orient the indicators so that blue always means a
#' synergy and red a trade-off.
#'
#' @param cor_mat A correlation matrix, normally from
#'   \code{\link{nexus_tradeoff}}.
#' @param p_values Matrix of p-values. Default taken from
#'   \code{attr(cor_mat, "p_values")}; if absent, no stars are shown.
#' @param benefit Optional logical vector, one value per indicator:
#'   \code{TRUE} if higher is better, \code{FALSE} if lower is better
#'   (for example GHG emission). When given, each coefficient is
#'   multiplied by the product of the two directions, so a positive value
#'   always means both indicators improve together (synergy). When
#'   \code{NULL} (default), the raw correlations are shown.
#' @param triangle Character. \code{"lower"} (default) shows each pair
#'   once; \code{"full"} shows the whole matrix.
#' @param digits Integer. Decimal places for the coefficients. Default 2.
#' @param title Character. Plot title. Default \code{NULL}.
#'
#' @return A ggplot2 object.
#'
#' @details Stars: * p < 0.05, ** p < 0.01, *** p < 0.001 (orienting
#'   changes only the sign, not the p-value). Without \code{benefit}, a
#'   negative correlation is not necessarily a trade-off: lower GHG
#'   emission with higher yield is a synergy. With few treatments,
#'   correlations are imprecise; interpret them with care.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   data(arid_pulse_trial)
#'   d <- arid_pulse_trial
#'   ind <- data.frame(Yield = d$grain_yield,
#'                     WUE = d$grain_yield / d$total_water,
#'                     EUE = d$energy_output / d$energy_input,
#'                     IUE = d$grain_yield / d$n_uptake,
#'                     GHG = d$ghg_emission)
#'   tm <- nexus_tradeoff(ind, verbose = FALSE)
#'   plot_tradeoff(tm)
#'   # GHG is lower-is-better: orient so blue = synergy, red = trade-off
#'   plot_tradeoff(tm, benefit = c(TRUE, TRUE, TRUE, TRUE, FALSE))
#' }
#'
#' @export
plot_tradeoff <- function(cor_mat, p_values = attr(cor_mat, "p_values"),
                          benefit = NULL, triangle = c("lower", "full"),
                          digits = 2, title = NULL) {
  .need_ggplot()
  triangle <- match.arg(triangle)
  if (!is.matrix(cor_mat) || nrow(cor_mat) != ncol(cor_mat)) {
    stop("'cor_mat' must be a square correlation matrix.", call. = FALSE)
  }
  nm <- colnames(cor_mat)
  if (is.null(nm)) nm <- paste0("V", seq_len(ncol(cor_mat)))
  k <- length(nm)
  sgn <- rep(1, k)
  if (!is.null(benefit)) {
    if (!is.logical(benefit) || length(benefit) != k) {
      stop(sprintf("'benefit' must be a logical vector of length %d.", k),
           call. = FALSE)
    }
    sgn <- ifelse(benefit, 1, -1)
  }
  df <- expand.grid(i = seq_len(k), j = seq_len(k))
  df$r <- cor_mat[cbind(df$i, df$j)] * sgn[df$i] * sgn[df$j]
  df$p <- if (!is.null(p_values)) p_values[cbind(df$i, df$j)] else NA_real_
  if (triangle == "lower") df <- df[df$i > df$j, , drop = FALSE]
  if (triangle == "full") df <- df[df$i != df$j, , drop = FALSE]
  df$row <- factor(nm[df$i], levels = rev(nm))
  df$col <- factor(nm[df$j], levels = nm)
  stars <- ifelse(is.na(df$p), "",
                  ifelse(df$p < 0.001, "***",
                         ifelse(df$p < 0.01, "**",
                                ifelse(df$p < 0.05, "*", ""))))
  df$lab <- paste0(formatC(df$r, format = "f", digits = digits), stars)
  df$txt <- .text_on(.ramp_hex(df$r, unname(.wef_div), c(-1, 1)))
  ggplot2::ggplot(df, ggplot2::aes(x = .data$col, y = .data$row,
                                   fill = .data$r)) +
    ggplot2::geom_tile(colour = "white", linewidth = 1.2) +
    ggplot2::geom_text(ggplot2::aes(label = .data$lab, colour = .data$txt),
                       size = 3.4) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_gradient2(
      low = .wef_div[["low"]], mid = .wef_div[["mid"]],
      high = .wef_div[["high"]], midpoint = 0, limits = c(-1, 1),
      name = if (is.null(benefit)) "r" else "Oriented r",
      breaks = c(-1, -0.5, 0, 0.5, 1)) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = title, x = NULL, y = NULL,
                  caption = paste(
                    if (is.null(benefit))
                      "Blue: positive correlation. Red: negative correlation."
                    else "Blue: synergy. Red: trade-off (indicators oriented so higher is better).",
                    "* p < 0.05, ** p < 0.01, *** p < 0.001")) +
    .theme_wef() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 30,
                                                       hjust = 1),
                   legend.position = "right",
                   legend.key.height = ggplot2::unit(1.2, "cm"))
}

#' Plot method for nexus_analysis
#'
#' Quick access to the main figures of a \code{\link{nexus_analysis}}.
#'
#' @param x An object of class \code{nexus_analysis}.
#' @param which Character. \code{"radar"} (default, one panel per
#'   treatment), \code{"anova"} (means, error bars and letters),
#'   \code{"topsis"} (ranking) or \code{"heatmap"} (dimension scores).
#' @param ... Passed to the underlying plot function.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   data(arid_pulse_trial)
#'   res <- nexus_analysis(
#'     arid_pulse_trial, treatment = "treatment", block = "replication",
#'     yield = "grain_yield", water = "total_water",
#'     energy_in = "energy_input", energy_out = "energy_output",
#'     emission = "ghg_emission", n_uptake = "n_uptake", verbose = FALSE)
#'   plot(res)
#'   plot(res, which = "anova")
#' }
#'
#' @export
plot.nexus_analysis <- function(x, which = c("radar", "anova", "topsis",
                                             "heatmap"), ...) {
  .need_ggplot()
  which <- match.arg(which)
  dims <- c("W_score", "E_score", "F_score", "N_score", "C_score")
  sc <- as.matrix(as.data.frame(x$nexus)[, dims])
  switch(which,
         radar = nexus_radar_gg(sc, treatment_names = x$nexus$treatment,
                                facet = TRUE, title = NULL, ...),
         anova = {
           if (is.null(x$anova)) {
             stop("No ANOVA: treatments were not replicated.", call. = FALSE)
           }
           plot(x$anova, ...)
         },
         topsis = plot(x$topsis, ...),
         heatmap = nexus_heatmap_gg(sc, treatment_names = x$nexus$treatment,
                                    title = NULL, ...))
}

.pretty_item <- function(v) {
  known <- c(human_labour_man = "Human labour (men)",
             human_labour_woman = "Human labour (women)",
             bullock_pair = "Bullock pairs",
             irrigation_water = "Irrigation water",
             zinc_sulphate = "Zinc sulphate", fym = "FYM",
             nitrogen = "Nitrogen (N)", phosphorus = "Phosphorus (P2O5)",
             potassium = "Potassium (K2O)")
  out <- ifelse(v %in% names(known), known[v], gsub("_", " ", v))
  out <- paste0(toupper(substring(out, 1, 1)), substring(out, 2))
  unname(out)
}
