# =============================================================================
# OBJECTIVE WEIGHTING AND MULTI-CRITERIA RANKING
# Entropy, CRITIC and PCA weights; TOPSIS ranking
# =============================================================================

#' Objective Weights for Nexus Indicators
#'
#' Derives dimension weights from the data instead of assigning them by
#' judgement. Supports equal weights, the Shannon entropy weight method,
#' the CRITIC method and PCA-based weights. The result can be passed
#' directly to the \code{weights} argument of \code{\link{nexus_index}},
#' \code{\link{nexus_sustainability_score}}, \code{\link{nexus_summary}}
#' or \code{\link{nexus_topsis}}.
#'
#' @param data A data frame or matrix with treatments in rows and
#'   indicators (for example the five WEFNC dimension scores) in columns.
#'   Only numeric columns are used.
#' @param method Character. One of \code{"entropy"} (default),
#'   \code{"critic"}, \code{"pca"} or \code{"equal"}.
#' @param benefit Logical vector, one value per numeric column.
#'   \code{TRUE} means higher values are better, \code{FALSE} means lower
#'   values are better (for example carbon footprint). Default all
#'   \code{TRUE}. Direction changes entropy and CRITIC weights, because it
#'   changes the normalized values and the signs of correlations; PCA
#'   weights do not depend on it.
#' @param verbose Logical. If \code{TRUE}, prints the weights.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @details
#' Indicators are first min-max normalized to 0 to 1 (inverted where
#' \code{benefit = FALSE}). With \eqn{m} treatments and \eqn{n}
#' indicators:
#'
#' \strong{Entropy.} \eqn{p_{ij} = x_{ij} / \sum_i x_{ij}},
#' \eqn{e_j = -\frac{1}{\ln m} \sum_i p_{ij} \ln p_{ij}} (with
#' \eqn{0 \ln 0 = 0}), and \eqn{w_j = (1 - e_j) / \sum_j (1 - e_j)}.
#' Indicators that vary more between treatments get more weight.
#'
#' \strong{CRITIC.} \eqn{C_j = \sigma_j \sum_k (1 - r_{jk})} and
#' \eqn{w_j = C_j / \sum_j C_j}, where \eqn{\sigma_j} is the standard
#' deviation of the normalized indicator and \eqn{r_{jk}} the Pearson
#' correlation between indicators. Indicators that vary more and overlap
#' less with others get more weight.
#'
#' \strong{PCA.} Principal components of the standardized indicators
#' are computed and components with eigenvalue of at least 1 are kept
#' (at least one). The weight of indicator \eqn{j} is proportional to
#' \eqn{\sum_k \lambda_k |v_{jk}|}, where \eqn{\lambda_k} is the
#' eigenvalue and \eqn{v_{jk}} the loading.
#'
#' Constant indicators receive zero weight under entropy, CRITIC and PCA.
#' With very few treatments (fewer than about five) data-driven weights
#' are unstable and should be reported with a sensitivity analysis
#' (\code{\link{nexus_sensitivity}}).
#'
#' @return A named numeric vector of weights summing to 1, with the
#'   method stored in \code{attr(, "method")}.
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
#' nexus_weights(ind, method = "entropy",
#'               benefit = c(TRUE, TRUE, TRUE, TRUE, FALSE))
#' nexus_weights(ind, method = "critic",
#'               benefit = c(TRUE, TRUE, TRUE, TRUE, FALSE))
#'
#' @references
#' Hwang, C.L. & Yoon, K. (1981). \emph{Multiple Attribute Decision
#' Making: Methods and Applications}. Springer, Berlin.
#'
#' Diakoulaki, D., Mavrotas, G. & Papayannakis, L. (1995). Determining
#' objective weights in multiple criteria problems: The CRITIC method.
#' \emph{Computers & Operations Research}, 22(7), 763-770.
#' \doi{10.1016/0305-0548(94)00059-H}
#'
#' @seealso \code{\link{nexus_topsis}}, \code{\link{nexus_sensitivity}}
#' @export
nexus_weights <- function(data, method = c("entropy", "critic", "pca",
                                           "equal"),
                          benefit = NULL,
                          verbose = getOption("wefnexus.verbose", TRUE)) {
  method <- match.arg(method)
  x <- .indicator_matrix(data)
  n_ind <- ncol(x)
  benefit <- .check_benefit(benefit, n_ind)
  if (nrow(x) < 2) stop("Need at least 2 rows (treatments).", call. = FALSE)

  z <- x
  for (j in seq_len(n_ind)) {
    z[, j] <- .minmax_raw(x[, j], inverse = !benefit[j])
  }
  varying <- apply(x, 2, function(v) {
    r <- range(v, na.rm = TRUE)
    is.finite(r[1]) && r[2] > r[1]
  })

  w <- switch(
    method,
    equal = rep(1 / n_ind, n_ind),
    entropy = {
      m <- nrow(z)
      e <- apply(z, 2, function(v) {
        v <- v[!is.na(v)]
        s <- sum(v)
        if (s == 0) return(1)
        p <- v / s
        p <- p[p > 0]
        -sum(p * log(p)) / log(m)
      })
      d <- 1 - e
      d[!varying] <- 0
      d
    },
    critic = {
      cj <- rep(0, n_ind)
      if (sum(varying) == 1) {
        cj[varying] <- 1
      } else if (sum(varying) > 1) {
        zv <- z[, varying, drop = FALSE]
        s <- apply(zv, 2, stats::sd, na.rm = TRUE)
        r <- suppressWarnings(stats::cor(zv, use = "pairwise.complete.obs"))
        r[is.na(r)] <- 0
        # Perfectly correlated indicators can give r = 0.9999999999999998
        # on some platforms; treat that rounding noise as exactly 1.
        r <- pmin(pmax(r, -1), 1)
        r[abs(1 - r) < 1e-10] <- 1
        cj[varying] <- s * colSums(1 - r)
      }
      cj
    },
    pca = {
      w_p <- rep(0, n_ind)
      keep <- which(varying)
      if (length(keep) == 1) {
        w_p[keep] <- 1
      } else if (length(keep) > 1) {
        zz <- x[stats::complete.cases(x[, keep]), keep, drop = FALSE]
        pc <- stats::prcomp(zz, center = TRUE, scale. = TRUE)
        ev <- pc$sdev^2
        k <- max(1, sum(ev >= 1))
        w_p[keep] <- as.numeric(abs(pc$rotation[, seq_len(k), drop = FALSE]) %*%
                                  ev[seq_len(k)])
      }
      w_p
    }
  )
  if (sum(w) <= 0) {
    if (any(varying)) {
      warning("The indicators are perfectly correlated, so ",
              switch(method, critic = "CRITIC", pca = "PCA", method),
              " weights are undefined; using equal weights.", call. = FALSE)
    } else {
      warning("No indicator varies across treatments; using equal weights.",
              call. = FALSE)
    }
    w <- rep(1 / n_ind, n_ind)
  }
  w <- w / sum(w)
  names(w) <- colnames(x)
  attr(w, "method") <- method
  if (verbose) {
    message("Weights (", method, "): ",
            paste0(names(w), " = ", round(w, 3), collapse = ", "))
  }
  w
}

#' TOPSIS Ranking of Treatments
#'
#' Ranks treatments by the Technique for Order of Preference by
#' Similarity to Ideal Solution (TOPSIS) using several nexus indicators
#' at once. Unlike \code{\link{nexus_index}}, TOPSIS works on raw
#' indicators in their own units and handles "lower is better"
#' indicators directly.
#'
#' @param data A data frame or matrix with treatments in rows and
#'   indicators in columns. Only numeric columns are used.
#' @param weights Numeric vector of weights, one per numeric column, or
#'   a method name passed to \code{\link{nexus_weights}}
#'   (\code{"equal"}, \code{"entropy"}, \code{"critic"}, \code{"pca"}).
#'   Default \code{"equal"}.
#' @param benefit Logical vector, one value per numeric column.
#'   \code{TRUE} for higher-is-better indicators, \code{FALSE} for
#'   lower-is-better indicators such as carbon footprint or energy
#'   intensity. Default all \code{TRUE}.
#' @param treatment_names Character vector of treatment labels. Default
#'   uses row names or "T 1", "T 2", and so on.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @details
#' Each column is vector-normalized,
#' \eqn{r_{ij} = x_{ij} / \sqrt{\sum_i x_{ij}^2}}, and multiplied by its
#' weight. The ideal solution takes the best value of each column and the
#' anti-ideal the worst. The closeness coefficient is
#' \eqn{C_i = D_i^- / (D_i^+ + D_i^-)}, where \eqn{D^+} and \eqn{D^-} are
#' Euclidean distances to the ideal and anti-ideal. \eqn{C_i} lies
#' between 0 and 1; higher is better.
#'
#' @return A data frame of class \code{topsis_result} with columns
#'   \code{treatment}, \code{d_ideal}, \code{d_anti_ideal},
#'   \code{closeness} and \code{rank} (1 = best). The weights used are
#'   stored in \code{attr(, "weights")}. Use \code{plot()} for a ranked
#'   bar chart.
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
#' nexus_topsis(ind, weights = "entropy",
#'              benefit = c(TRUE, TRUE, TRUE, TRUE, FALSE),
#'              treatment_names = d$treatment)
#'
#' @references
#' Hwang, C.L. & Yoon, K. (1981). \emph{Multiple Attribute Decision
#' Making: Methods and Applications}. Springer, Berlin.
#'
#' @seealso \code{\link{nexus_weights}}
#' @export
nexus_topsis <- function(data, weights = "equal", benefit = NULL,
                         treatment_names = NULL,
                         verbose = getOption("wefnexus.verbose", TRUE)) {
  x <- .indicator_matrix(data)
  n_ind <- ncol(x)
  benefit <- .check_benefit(benefit, n_ind)
  if (anyNA(x)) stop("'data' must not contain missing values.", call. = FALSE)
  if (any(x < 0)) {
    warning("Negative values found; vector normalization assumes ",
            "non-negative indicators.", call. = FALSE)
  }
  if (is.character(weights)) {
    weights <- nexus_weights(x, method = weights, benefit = benefit,
                             verbose = FALSE)
  }
  if (length(weights) != n_ind) {
    stop(sprintf("'weights' must have %d elements.", n_ind), call. = FALSE)
  }
  if (any(weights < 0)) stop("'weights' must be non-negative.", call. = FALSE)
  if (sum(weights) <= 0) stop("'weights' must not all be zero.", call. = FALSE)
  weights <- weights / sum(weights)
  if (is.null(treatment_names)) {
    treatment_names <- if (!is.null(rownames(data)) &&
                           !identical(rownames(data),
                                      as.character(seq_len(nrow(x))))) {
      rownames(data)
    } else {
      paste("T", seq_len(nrow(x)))
    }
  }
  norms <- sqrt(colSums(x^2))
  norms[norms == 0] <- 1
  v <- sweep(sweep(x, 2, norms, "/"), 2, as.numeric(weights), "*")
  best <- ifelse(benefit, apply(v, 2, max), apply(v, 2, min))
  worst <- ifelse(benefit, apply(v, 2, min), apply(v, 2, max))
  d_pos <- sqrt(rowSums(sweep(v, 2, best)^2))
  d_neg <- sqrt(rowSums(sweep(v, 2, worst)^2))
  cc <- ifelse(d_pos + d_neg == 0, 0.5, d_neg / (d_pos + d_neg))
  result <- data.frame(
    treatment = treatment_names,
    d_ideal = round(d_pos, 4),
    d_anti_ideal = round(d_neg, 4),
    closeness = round(cc, 4),
    rank = as.integer(rank(-cc, ties.method = "min")),
    stringsAsFactors = FALSE
  )
  attr(result, "weights") <- stats::setNames(as.numeric(weights),
                                             colnames(x))
  class(result) <- c("topsis_result", "data.frame")
  if (verbose) {
    message("TOPSIS: best treatment is ",
            result$treatment[which.max(cc)], ".")
  }
  result
}

# --- internal helpers --------------------------------------------------------

.indicator_matrix <- function(data) {
  if (is.matrix(data)) data <- as.data.frame(data)
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame or matrix.", call. = FALSE)
  }
  num <- vapply(data, is.numeric, logical(1))
  if (sum(num) < 2) stop("Need at least 2 numeric columns.", call. = FALSE)
  x <- as.matrix(data[, num, drop = FALSE])
  if (is.null(colnames(x))) colnames(x) <- paste0("V", seq_len(ncol(x)))
  x
}

.check_benefit <- function(benefit, n) {
  if (is.null(benefit)) return(rep(TRUE, n))
  if (!is.logical(benefit) || length(benefit) != n) {
    stop(sprintf("'benefit' must be a logical vector of length %d.", n),
         call. = FALSE)
  }
  benefit
}

.minmax_raw <- function(x, inverse = FALSE) {
  r <- range(x, na.rm = TRUE)
  if (!all(is.finite(r)) || r[1] == r[2]) return(rep(0.5, length(x)))
  z <- (x - r[1]) / (r[2] - r[1])
  if (inverse) 1 - z else z
}
