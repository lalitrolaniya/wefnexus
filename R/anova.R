# =============================================================================
# TREATMENT COMPARISON
# ANOVA with SEm, CD (LSD), CV and letter groups for field experiments
# =============================================================================

#' ANOVA Table for Nexus Indicators (CRD or RCBD)
#'
#' Runs a one-way analysis of variance for each indicator, in a
#' completely randomized design (CRD) or randomized complete block design
#' (RCBD), and returns the statistics usually reported in agronomy
#' papers: treatment means, standard error of mean (SEm), critical
#' difference (CD, the least significant difference), coefficient of
#' variation (CV) and letter groups.
#'
#' @param data A data frame with replication-level (plot-level) data.
#' @param response Character vector of numeric column names to analyse.
#' @param treatment Character. Name of the treatment column.
#' @param block Character. Name of the block (replication) column for an
#'   RCBD. \code{NULL} (default) fits a CRD.
#' @param alpha Numeric. Significance level for the F test, CD and
#'   letters. Default 0.05.
#' @param digits Integer. Decimal places in the formatted table.
#'   Default 2.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @details For each response the model \code{y ~ block + treatment}
#'   (RCBD) or \code{y ~ treatment} (CRD) is fitted with
#'   \code{\link[stats]{aov}}; in an RCBD the treatment F test is adjusted
#'   for blocks. With error mean square \eqn{MSE}, \eqn{r} replications
#'   and error degrees of freedom \eqn{df_e}:
#'   \deqn{SEm = \sqrt{MSE / r}, \quad SEd = \sqrt{2 MSE / r}}
#'   \deqn{CD = t_{1 - \alpha/2,\, df_e} \times SEd, \quad
#'   CV = 100 \sqrt{MSE} / \bar{y}}
#'
#'   Treatment means are least-squares means: in an RCBD with missing
#'   plots they are adjusted for blocks, and with complete data they equal
#'   the raw means. Letters follow Fisher's LSD: every pair of treatments
#'   is compared with its own standard error of the difference, and the
#'   compact letter display is built with the insert-and-absorb algorithm
#'   of Piepho (2004), so treatments sharing a letter do not differ at
#'   level \code{alpha} and treatments that differ share no letter. With
#'   equal replication this is the familiar single CD. With unequal
#'   replication or missing plots the single SEm, SEd and CD in the table
#'   use the harmonic mean of replications and are approximate, while the
#'   letters remain exact. Beyond 26 groups the letters continue with
#'   capitals and then numbered letters.
#'
#'   When the treatment F test is not significant, all treatments get the
#'   letter "a" and the CD is shown as "NS" in the formatted table.
#'   Treatments with no observations for a response are shown as missing.
#'
#' @return An object of class \code{nexus_anova}: a list with
#' \describe{
#'   \item{means}{Data frame of treatment means (treatments in rows,
#'     responses in columns).}
#'   \item{letters}{Data frame of letter groups in the same layout.}
#'   \item{se}{Data frame of the standard error of each treatment mean
#'     (standard deviation / sqrt(n)), in the same layout.}
#'   \item{stats}{Data frame, one row per response: \code{F_value},
#'     \code{p_value}, \code{significant}, \code{MSE}, \code{df_error},
#'     \code{SEm}, \code{SEd}, \code{CD}, \code{CV_pct},
#'     \code{grand_mean}, \code{balanced}.}
#'   \item{table}{Character data frame ready for a manuscript: means with
#'     letters, followed by SEm, CD and CV rows.}
#'   \item{models}{List of fitted \code{aov} objects.}
#'   \item{alpha}{The significance level used.}
#' }
#' Use \code{plot()} on the result for a bar chart with error bars and
#' letters (\code{\link{plot.nexus_anova}}).
#'
#' @examples
#' data(arid_pulse_trial)
#' res <- nexus_anova(arid_pulse_trial,
#'                    response = c("grain_yield", "n_uptake"),
#'                    treatment = "treatment", block = "replication")
#' res
#' res$stats
#'
#' @references
#' Gomez, K.A. & Gomez, A.A. (1984). \emph{Statistical Procedures for
#' Agricultural Research}, 2nd ed. John Wiley & Sons, New York.
#'
#' Piepho, H.-P. (2004). An algorithm for a letter-based representation
#' of all-pairwise comparisons. \emph{Journal of Computational and
#' Graphical Statistics}, 13(2), 456-466.
#' \doi{10.1198/1061860043515}
#'
#' @export
nexus_anova <- function(data, response, treatment, block = NULL,
                        alpha = 0.05, digits = 2,
                        verbose = getOption("wefnexus.verbose", TRUE)) {
  if (!is.data.frame(data)) stop("'data' must be a data frame.", call. = FALSE)
  cols <- c(response, treatment, block)
  miss <- setdiff(cols, names(data))
  if (length(miss) > 0) {
    stop("Column(s) not found in 'data': ", paste(miss, collapse = ", "),
         call. = FALSE)
  }
  for (r in response) .validate_numeric(data[[r]], r)
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("'alpha' must be between 0 and 1.", call. = FALSE)
  }
  trt <- factor(data[[treatment]], levels = unique(data[[treatment]]))
  if (nlevels(trt) < 2) stop("Need at least 2 treatments.", call. = FALSE)
  reps <- table(trt)
  if (all(reps < 2)) {
    stop("Each treatment has a single observation; ANOVA needs ",
         "replicated data.", call. = FALSE)
  }
  blk <- if (!is.null(block)) factor(data[[block]]) else NULL
  all_trt <- levels(trt)

  means <- data.frame(treatment = all_trt, stringsAsFactors = FALSE)
  lets <- means
  ses <- means
  stats_rows <- list()
  models <- list()
  for (r in response) {
    df <- data.frame(y = data[[r]], trt = trt)
    if (!is.null(blk)) df$blk <- blk
    df <- df[!is.na(df$y), , drop = FALSE]
    present <- all_trt[all_trt %in% as.character(df$trt)]
    if (length(present) < 2) {
      stop("'", r, "' has data for fewer than 2 treatments.", call. = FALSE)
    }
    df$trt <- factor(as.character(df$trt), levels = present)
    if (!is.null(blk)) df$blk <- droplevels(df$blk)
    fml <- if (is.null(blk)) y ~ trt else y ~ blk + trt
    fit <- stats::aov(fml, data = df)
    tab <- summary(fit)[[1]]
    rn <- trimws(rownames(tab))
    f_val <- tab[rn == "trt", "F value"]
    p_val <- tab[rn == "trt", "Pr(>F)"]
    df_e <- tab[rn == "Residuals", "Df"]
    mse <- tab[rn == "Residuals", "Mean Sq"]
    if (length(df_e) == 0 || df_e < 1) {
      stop("No error degrees of freedom for '", r, "'.", call. = FALSE)
    }
    n_i <- as.numeric(table(df$trt))
    balanced <- length(unique(n_i)) == 1 &&
      (is.null(blk) || all(table(df$blk, df$trt) == 1))
    r_h <- 1 / mean(1 / n_i)
    sem <- sqrt(mse / r_h)
    sed <- sqrt(2 * mse / r_h)
    t_crit <- stats::qt(1 - alpha / 2, df_e)
    cd <- t_crit * sed
    gm <- mean(df$y)

    lsm <- .ls_means(fit, df, present)
    sig <- !is.na(p_val) && p_val < alpha
    let <- rep("a", length(present))
    if (sig) {
      v <- lsm$vcov
      dd <- outer(lsm$mean, lsm$mean, "-")
      se_d <- sqrt(pmax(outer(diag(v), diag(v), "+") - 2 * v, 0))
      pair_sig <- abs(dd) > t_crit * se_d + 1e-12
      diag(pair_sig) <- FALSE
      let <- .cld(lsm$mean, pair_sig)
    }
    idx <- match(present, all_trt)
    m_full <- rep(NA_real_, length(all_trt))
    m_full[idx] <- lsm$mean
    l_full <- rep("", length(all_trt))
    l_full[idx] <- let
    s_full <- rep(NA_real_, length(all_trt))
    s_full[idx] <- as.numeric(tapply(df$y, df$trt, function(v) {
      if (length(v) > 1) stats::sd(v) / sqrt(length(v)) else NA_real_
    }))
    means[[r]] <- m_full
    lets[[r]] <- l_full
    ses[[r]] <- s_full
    stats_rows[[r]] <- data.frame(
      response = r, F_value = round(f_val, 3), p_value = signif(p_val, 4),
      significant = sig, MSE = signif(mse, 6), df_error = df_e,
      SEm = round(sem, 4), SEd = round(sed, 4), CD = round(cd, 4),
      CV_pct = round(100 * sqrt(mse) / gm, 2), grand_mean = round(gm, 4),
      balanced = balanced, stringsAsFactors = FALSE)
    models[[r]] <- fit
  }
  st <- do.call(rbind, stats_rows)
  rownames(st) <- NULL

  fmt <- function(v) ifelse(is.na(v), "-",
                            formatC(v, format = "f", digits = digits))
  pm <- if (isTRUE(l10n_info()[["UTF-8"]])) "\u00b1" else "+/-"
  tbl <- data.frame(treatment = c(all_trt, paste("SEm", pm),
                                  sprintf("CD (p = %s)", alpha), "CV (%)"),
                    stringsAsFactors = FALSE)
  for (r in response) {
    s <- st[st$response == r, ]
    tbl[[r]] <- c(trimws(paste(fmt(means[[r]]), lets[[r]])),
                  fmt(s$SEm),
                  if (s$significant) fmt(s$CD) else "NS",
                  formatC(s$CV_pct, format = "f", digits = 2))
  }
  if (verbose) {
    message("ANOVA (", if (is.null(block)) "CRD" else "RCBD", ") for ",
            length(response), " response(s); ", sum(st$significant),
            " significant at p < ", alpha, ".")
    if (any(!st$balanced)) {
      message("Unequal replication or missing plots for: ",
              paste(st$response[!st$balanced], collapse = ", "),
              ". Means are least-squares means; letters use exact pairwise ",
              "tests; the single SEm and CD are approximate.")
    }
  }
  structure(list(means = means, letters = lets, se = ses, stats = st,
                 table = tbl, models = models, alpha = alpha),
            class = "nexus_anova")
}

#' Print method for nexus_anova
#'
#' @param x An object of class \code{nexus_anova}.
#' @param ... Additional arguments passed to \code{print.data.frame}.
#'
#' @return Invisibly returns \code{x}. Called for the side effect of
#'   printing the formatted comparison table.
#'
#' @export
print.nexus_anova <- function(x, ...) {
  cat("Treatment comparison (means followed by the same letter do not",
      "differ significantly)\n\n")
  print.data.frame(x$table, row.names = FALSE, right = FALSE, ...)
  invisible(x)
}

# Least-squares means of treatments and their covariance matrix.
# CRD: equal to the raw means. RCBD: predictions averaged over blocks,
# which equal the raw means when every treatment occurs once per block.
.ls_means <- function(fit, df, present) {
  lmfit <- stats::lm(stats::formula(fit), data = df)
  b <- stats::coef(lmfit)
  if (anyNA(b)) {
    raw <- as.numeric(tapply(df$y, df$trt, mean))
    n_i <- as.numeric(table(df$trt))
    mse <- sum(stats::residuals(lmfit)^2) / lmfit$df.residual
    return(list(mean = raw, vcov = diag(mse / n_i, length(raw))))
  }
  tt <- stats::delete.response(stats::terms(lmfit))
  if ("blk" %in% names(df)) {
    nd <- expand.grid(blk = levels(df$blk),
                      trt = factor(present, levels = present))
  } else {
    nd <- data.frame(trt = factor(present, levels = present))
  }
  X <- stats::model.matrix(tt, nd, xlev = lmfit$xlevels,
                           contrasts.arg = lmfit$contrasts)
  L <- t(vapply(present, function(t) colMeans(X[nd$trt == t, , drop = FALSE]),
                numeric(ncol(X))))
  list(mean = as.numeric(L %*% b),
       vcov = L %*% stats::vcov(lmfit) %*% t(L))
}

# Compact letter display from a logical matrix of significant pairs
# (insert-and-absorb, Piepho 2004). The column holding the highest mean
# gets the first letter.
.cld <- function(means, sig) {
  k <- length(means)
  cols <- matrix(TRUE, nrow = k, ncol = 1)
  if (k > 1) {
    for (i in seq_len(k - 1)) {
      for (j in (i + 1):k) {
        if (!isTRUE(sig[i, j]) && !isTRUE(sig[j, i])) next
        hit <- which(cols[i, ] & cols[j, ])
        if (length(hit) == 0) next
        add <- cols[, hit, drop = FALSE]
        cols[i, hit] <- FALSE
        add[j, ] <- FALSE
        cols <- .absorb(cbind(cols, add))
      }
    }
  }
  top <- apply(cols, 2, function(cl) max(means[cl]))
  avg <- apply(cols, 2, function(cl) mean(means[cl]))
  cols <- cols[, order(-top, -avg), drop = FALSE]
  labs <- .cld_labels(ncol(cols))
  apply(cols, 1, function(rw) paste(labs[rw], collapse = ""))
}

# Drop columns that are subsets of (or duplicate) another column.
.absorb <- function(cols) {
  n <- ncol(cols)
  if (n < 2) return(cols)
  m <- cols * 1
  out_of <- crossprod(m, 1 - m)   # [a, b] = members of a not in b
  sub <- out_of == 0
  diag(sub) <- FALSE
  drop <- rep(FALSE, n)
  for (a in seq_len(n)) {
    for (b in which(sub[a, ] & !drop)) {
      if (!sub[b, a] || b < a) {
        drop[a] <- TRUE
        break
      }
    }
  }
  cols[, !drop, drop = FALSE]
}

.cld_labels <- function(n) {
  base <- c(letters, LETTERS)
  if (n <= length(base)) return(base[seq_len(n)])
  extra <- paste0(letters, rep(seq_len(ceiling((n - 52) / 26)), each = 26))
  c(base, extra)[seq_len(n)]
}

# Letters for a single critical difference (kept for compatibility).
.lsd_letters <- function(means, cd) {
  sig <- abs(outer(means, means, "-")) > cd
  diag(sig) <- FALSE
  .cld(means, sig)
}
