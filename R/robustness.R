# =============================================================================
# RANKING ROBUSTNESS
# Compare treatment rankings across weighting methods and aggregation rules
# =============================================================================

#' Compare Treatment Rankings Across Weighting Methods
#'
#' Ranks treatments eight ways (composite index and TOPSIS, each with
#' equal, entropy, CRITIC and PCA weights) and measures how far the
#' rankings agree. A ranking that holds across methods is a stronger
#' basis for a recommendation than one that depends on a single choice
#' of weights.
#'
#' @param data A data frame or matrix with treatments in rows and raw
#'   indicators in columns. Only numeric columns are used.
#' @param benefit Logical vector, one value per numeric column:
#'   \code{TRUE} if higher is better, \code{FALSE} if lower is better.
#'   Default all \code{TRUE}.
#' @param treatment_names Character vector of treatment labels.
#' @param methods Character vector of weighting methods to compare.
#'   Default \code{c("equal", "entropy", "critic", "pca")}.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @details For each weighting method, the composite index is the
#'   weighted sum of min-max normalized indicators (inverted where
#'   \code{benefit = FALSE}), and TOPSIS is computed with
#'   \code{\link{nexus_topsis}}. Agreement among the \eqn{m} rankings of
#'   \eqn{n} treatments is summarised by Kendall's coefficient of
#'   concordance with correction for ties,
#'   \deqn{W = \frac{12 S}{m^2 (n^3 - n) - m \sum T}}{W = 12 S / (m^2 (n^3 - n) - m sum(T))}
#'   where \eqn{S} is the sum of squared deviations of the treatment rank
#'   sums from their mean and \eqn{T = \sum (t^3 - t)} over tied groups.
#'   \eqn{W} runs from 0 (no agreement) to 1 (identical rankings). The
#'   chi-square statistic \eqn{\chi^2 = m (n - 1) W} with \eqn{n - 1}
#'   degrees of freedom is reported for reference only: the eight
#'   rankings come from the same data and are not independent judges, so
#'   its p-value is not a formal test of robustness. Read W itself, and
#'   look at which treatments change rank and under which method.
#'
#' @return An object of class \code{rank_compare}: a list with
#' \describe{
#'   \item{ranks}{Data frame: treatment, one rank column per method,
#'     \code{mean_rank} and \code{consensus} (rank of the mean rank).}
#'   \item{scores}{Data frame of index values and TOPSIS closeness.}
#'   \item{weights}{Matrix of weights (methods x indicators).}
#'   \item{kendall_w}{Kendall's W.}
#'   \item{chisq, df, p_value}{Chi-square test of W.}
#'   \item{spearman}{Matrix of Spearman correlations between rankings.}
#' }
#'
#' @examples
#' data(arid_pulse_nexus)
#' d <- arid_pulse_nexus
#' ind <- data.frame(
#'   WUE = d$grain_yield / d$total_water,
#'   EUE = (d$energy_output_grain + d$energy_output_straw) / d$energy_input,
#'   Yield = d$grain_yield,
#'   IUE = d$grain_yield / d$n_uptake,
#'   CF = d$ghg_emission
#' )
#' rc <- nexus_rank_compare(ind, benefit = c(TRUE, TRUE, TRUE, TRUE, FALSE),
#'                          treatment_names = d$treatment)
#' rc
#' if (requireNamespace("ggplot2", quietly = TRUE)) plot(rc)
#'
#' @references
#' Kendall, M.G. & Babington Smith, B. (1939). The problem of m rankings.
#' \emph{The Annals of Mathematical Statistics}, 10(3), 275-287.
#' \doi{10.1214/aoms/1177732186}
#'
#' @seealso \code{\link{nexus_weights}}, \code{\link{nexus_topsis}},
#'   \code{\link{nexus_sensitivity}}
#' @export
nexus_rank_compare <- function(data, benefit = NULL, treatment_names = NULL,
                               methods = c("equal", "entropy", "critic",
                                           "pca"),
                               verbose = getOption("wefnexus.verbose",
                                                   TRUE)) {
  x <- .indicator_matrix(data)
  benefit <- .check_benefit(benefit, ncol(x))
  methods <- unique(match.arg(methods, c("equal", "entropy", "critic", "pca"),
                              several.ok = TRUE))
  n <- nrow(x)
  if (n < 3) stop("Need at least 3 treatments.", call. = FALSE)
  if (anyNA(x)) stop("'data' must not contain missing values.", call. = FALSE)
  if (is.null(treatment_names)) treatment_names <- paste("T", seq_len(n))
  if (length(treatment_names) != n) {
    stop("'treatment_names' must have one label per row.", call. = FALSE)
  }
  z <- x
  for (j in seq_len(ncol(x))) z[, j] <- .minmax_raw(x[, j], !benefit[j])

  wmat <- t(vapply(methods, function(m)
    as.numeric(nexus_weights(x, method = m, benefit = benefit,
                             verbose = FALSE)), numeric(ncol(x))))
  if (length(methods) == 1) wmat <- matrix(wmat, nrow = 1)
  dimnames(wmat) <- list(methods, colnames(x))

  sc <- data.frame(treatment = treatment_names, stringsAsFactors = FALSE)
  rk <- sc
  for (m in methods) {
    idx <- as.numeric(z %*% wmat[m, ])
    ts <- nexus_topsis(x, weights = wmat[m, ], benefit = benefit,
                       treatment_names = treatment_names, verbose = FALSE)
    sc[[paste0("index_", m)]] <- round(idx, 4)
    sc[[paste0("topsis_", m)]] <- ts$closeness
    rk[[paste0("index_", m)]] <- rank(-idx, ties.method = "average")
    rk[[paste0("topsis_", m)]] <- rank(-ts$closeness, ties.method = "average")
  }
  rcols <- setdiff(names(rk), "treatment")
  R <- as.matrix(rk[, rcols])
  rk$mean_rank <- round(rowMeans(R), 3)
  rk$consensus <- as.integer(rank(rk$mean_rank, ties.method = "min"))

  m <- ncol(R)
  rs <- rowSums(R)
  S <- sum((rs - mean(rs))^2)
  Tj <- sum(apply(R, 2, function(r) {
    t <- table(r)
    sum(t^3 - t)
  }))
  denom <- m^2 * (n^3 - n) - m * Tj
  W <- if (denom > 0) 12 * S / denom else NA_real_
  chisq <- m * (n - 1) * W
  pval <- stats::pchisq(chisq, df = n - 1, lower.tail = FALSE)
  sp <- suppressWarnings(stats::cor(R, method = "spearman"))

  if (verbose) {
    message("Kendall's W = ", round(W, 3), " across ", m,
            " rankings; consensus best: ",
            rk$treatment[which.min(rk$mean_rank)], ".")
  }
  structure(list(ranks = rk, scores = sc, weights = wmat,
                 kendall_w = round(W, 4), chisq = round(chisq, 3),
                 df = n - 1, p_value = signif(pval, 4),
                 spearman = round(sp, 3)),
            class = "rank_compare")
}

#' Print method for rank_compare
#'
#' @param x An object of class \code{rank_compare}.
#' @param ... Additional arguments passed to \code{print.data.frame}.
#'
#' @return Invisibly returns \code{x}. Called for the side effect of
#'   printing the ranks and the concordance statistics.
#'
#' @export
print.rank_compare <- function(x, ...) {
  cat("Ranking robustness across weighting methods\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")
  out <- x$ranks[order(x$ranks$mean_rank), , drop = FALSE]
  print.data.frame(out, row.names = FALSE, ...)
  agree <- if (is.na(x$kendall_w)) "undefined" else
    if (x$kendall_w >= 0.9) "very strong" else
      if (x$kendall_w >= 0.7) "strong" else
        if (x$kendall_w >= 0.5) "moderate" else "weak"
  cat(sprintf("\nKendall's W = %.3f (%s agreement across %d rankings).\n",
              x$kendall_w, agree, length(setdiff(names(x$ranks),
                                                 c("treatment", "mean_rank",
                                                   "consensus")))))
  cat("The rankings share the same data, so read W descriptively.\n")
  invisible(x)
}

#' Plot method for rank_compare
#'
#' Rank chart: each line follows one treatment across the eight
#' rankings, so stable and unstable treatments are easy to see.
#'
#' @param x An object of class \code{rank_compare}.
#' @param title Character. Plot title. Default \code{NULL}.
#' @param ... Unused.
#'
#' @return A ggplot2 object.
#'
#' @export
plot.rank_compare <- function(x, title = NULL, ...) {
  .need_ggplot()
  rk <- x$ranks
  rcols <- setdiff(names(rk), c("treatment", "mean_rank", "consensus"))
  trt <- rk$treatment[order(rk$mean_rank)]
  df <- data.frame(
    treatment = rep(rk$treatment, times = length(rcols)),
    method = rep(rcols, each = nrow(rk)),
    rank = as.vector(as.matrix(rk[, rcols])),
    stringsAsFactors = FALSE)
  pretty <- function(v) {
    v <- sub("^index_", "Index\n", v)
    v <- sub("^topsis_", "TOPSIS\n", v)
    v <- sub("critic", "CRITIC", v)
    v <- sub("pca", "PCA", v)
    sub("(\n)([a-z])", "\\1\\U\\2", v, perl = TRUE)
  }
  df$method <- factor(pretty(df$method), levels = pretty(rcols))
  df$treatment <- factor(df$treatment, levels = trt)
  cols <- stats::setNames(.group_cols(nrow(rk)), rk$treatment)
  last <- df[df$method == levels(df$method)[nlevels(df$method)], ]
  n <- nrow(rk)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$method, y = .data$rank,
                                   colour = .data$treatment,
                                   group = .data$treatment)) +
    ggplot2::geom_line(linewidth = 0.9, lineend = "round") +
    ggplot2::geom_point(ggplot2::aes(fill = .data$treatment), shape = 21,
                        size = 2.6, stroke = 0.9, colour = "white") +
    ggplot2::geom_text(data = last,
                       ggplot2::aes(label = .data$treatment),
                       hjust = 0, nudge_x = 0.18, size = 3.1,
                       colour = .wef_ink[["primary"]]) +
    ggplot2::scale_y_reverse(breaks = seq_len(n),
                             expand = ggplot2::expansion(add = 0.4)) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0.3, 2.2))) +
    ggplot2::scale_colour_manual(values = cols, name = NULL) +
    ggplot2::scale_fill_manual(values = cols, name = NULL) +
    ggplot2::labs(title = title, x = NULL, y = "Rank (1 = best)",
                  caption = sprintf("Kendall's W = %.2f across %d rankings.",
                                    x$kendall_w, length(rcols))) +
    .theme_wef() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   legend.position = "bottom")
}
