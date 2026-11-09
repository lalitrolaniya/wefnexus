# =============================================================================
# ONE-CALL WORKFLOW FOR REPLICATED FIELD DATA
# =============================================================================

#' Complete Nexus Analysis from Replicated Field Data
#'
#' Runs the full WEFNC workflow on plot-level data in one call: computes
#' plot-level indicators, compares treatments by ANOVA (with SEm, CD and
#' CV), builds the composite nexus index from treatment means with equal
#' or objective weights, and ranks treatments by TOPSIS.
#'
#' @param data A data frame with one row per plot (treatment x
#'   replication) or per treatment.
#' @param treatment Character. Name of the treatment column.
#' @param yield Character. Column with economic yield (kg/ha).
#' @param water Character. Column with water consumed (mm).
#' @param energy_in Character. Column with energy input (MJ/ha).
#' @param energy_out Character. Column with energy output (MJ/ha).
#' @param emission Character. Column with GHG emission (kg CO2-eq/ha).
#' @param n_applied Character. Column with N applied (kg/ha), or
#'   \code{NULL}.
#' @param n_uptake Character. Column with total N uptake (kg/ha), or
#'   \code{NULL}. At least one of \code{n_applied} and \code{n_uptake}
#'   is required.
#' @param block Character. Column with block (replication) for an RCBD,
#'   or \code{NULL} for a CRD.
#' @param weights Numeric vector of five weights (Water, Energy, Food,
#'   Nutrient, Carbon) or one of \code{"equal"} (default),
#'   \code{"entropy"}, \code{"critic"}, \code{"pca"}.
#' @param nutrient_metric Character. \code{"pfp"} or \code{"iue"}. The
#'   default picks \code{"iue"} when \code{n_uptake} is given and
#'   \code{"pfp"} otherwise.
#' @param carbon_basis Character. \code{"intensity"} (default, kg CO2-eq
#'   per kg yield) or \code{"emission"} (kg CO2-eq/ha).
#' @param alpha Numeric. Significance level for ANOVA. Default 0.05.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @details Plot-level indicators are WUE (kg/ha/mm), EUE, the nutrient
#'   indicator (PFP or IUE, kg/kg) and carbon intensity (kg CO2-eq/kg).
#'   The nexus index and TOPSIS ranking use treatment means of the raw
#'   variables. ANOVA is skipped when treatments are not replicated.
#'
#' @return An object of class \code{nexus_analysis}: a list with
#' \describe{
#'   \item{plot_data}{Input data with plot-level indicator columns
#'     \code{WUE}, \code{EUE}, \code{NUE} and \code{CI} added.}
#'   \item{treatment_means}{Treatment means of the raw variables.}
#'   \item{nexus}{A \code{nexus_result} from \code{\link{nexus_summary}}
#'     on the treatment means.}
#'   \item{anova}{A \code{nexus_anova} object for yield and the four
#'     indicators, or \code{NULL} without replication.}
#'   \item{topsis}{TOPSIS ranking from \code{\link{nexus_topsis}}.}
#'   \item{weights}{Named weights used for the nexus index.}
#' }
#'
#' @examples
#' data(arid_pulse_trial)
#' res <- nexus_analysis(
#'   arid_pulse_trial, treatment = "treatment", block = "replication",
#'   yield = "grain_yield", water = "total_water",
#'   energy_in = "energy_input", energy_out = "energy_output",
#'   emission = "ghg_emission", n_uptake = "n_uptake",
#'   weights = "entropy"
#' )
#' res
#' res$anova
#' \donttest{
#' plot(res$nexus)
#' }
#'
#' @seealso \code{\link{nexus_summary}}, \code{\link{nexus_anova}},
#'   \code{\link{nexus_weights}}, \code{\link{nexus_topsis}}
#' @export
nexus_analysis <- function(data, treatment, yield, water, energy_in,
                           energy_out, emission, n_applied = NULL,
                           n_uptake = NULL, block = NULL,
                           weights = "equal",
                           nutrient_metric = NULL,
                           carbon_basis = c("intensity", "emission"),
                           alpha = 0.05,
                           verbose = getOption("wefnexus.verbose", TRUE)) {
  carbon_basis <- match.arg(carbon_basis)
  if (!is.data.frame(data)) stop("'data' must be a data frame.", call. = FALSE)
  if (is.null(n_applied) && is.null(n_uptake)) {
    stop("Supply 'n_applied', 'n_uptake' or both.", call. = FALSE)
  }
  if (is.null(nutrient_metric)) {
    nutrient_metric <- if (!is.null(n_uptake)) "iue" else "pfp"
  }
  nutrient_metric <- match.arg(nutrient_metric, c("pfp", "iue"))
  vars <- c(yield, water, energy_in, energy_out, emission, n_applied,
            n_uptake)
  miss <- setdiff(c(treatment, block, vars), names(data))
  if (length(miss) > 0) {
    stop("Column(s) not found in 'data': ", paste(miss, collapse = ", "),
         call. = FALSE)
  }
  for (v in vars) .validate_numeric(data[[v]], v)

  pd <- data
  pd$WUE <- round(data[[yield]] / data[[water]], 4)
  pd$EUE <- round(data[[energy_out]] / data[[energy_in]], 4)
  pd$NUE <- if (nutrient_metric == "iue") {
    round(data[[yield]] / data[[n_uptake]], 4)
  } else {
    if (any(data[[n_applied]] <= 0, na.rm = TRUE)) {
      stop("PFP needs 'n_applied' > 0. Use nutrient_metric = \"iue\".",
           call. = FALSE)
    }
    round(data[[yield]] / data[[n_applied]], 4)
  }
  pd$CI <- round(data[[emission]] / data[[yield]], 4)

  trt <- factor(data[[treatment]], levels = unique(data[[treatment]]))
  tm <- data.frame(treatment = levels(trt), stringsAsFactors = FALSE)
  for (v in vars) tm[[v]] <- as.numeric(tapply(data[[v]], trt, mean,
                                               na.rm = TRUE))

  nx <- nexus_summary(
    yield = tm[[yield]], water_consumed = tm[[water]],
    energy_input = tm[[energy_in]], energy_output = tm[[energy_out]],
    n_applied = if (!is.null(n_applied)) tm[[n_applied]] else NULL,
    n_uptake = if (!is.null(n_uptake)) tm[[n_uptake]] else NULL,
    carbon_emission = tm[[emission]], treatment_names = tm$treatment,
    weights = weights, nutrient_metric = nutrient_metric,
    carbon_basis = carbon_basis, verbose = FALSE)

  ind <- data.frame(
    WUE = nx$WUE_kg_mm, EUE = nx$EUE_ratio, Yield = nx$yield_kg_ha,
    NUE = if (nutrient_metric == "iue") nx$IUE_kg_kgN else nx$PFP_kg_kgN,
    Carbon = if (carbon_basis == "intensity") nx$carbon_intensity else
      tm[[emission]])
  ts <- nexus_topsis(ind, weights = attr(nx, "weights"),
                     benefit = c(TRUE, TRUE, TRUE, TRUE, FALSE),
                     treatment_names = tm$treatment, verbose = FALSE)

  av <- NULL
  if (any(table(trt) > 1)) {
    av <- nexus_anova(pd, response = c(yield, "WUE", "EUE", "NUE", "CI"),
                      treatment = treatment, block = block, alpha = alpha,
                      verbose = FALSE)
  } else if (verbose) {
    message("Treatments are not replicated; ANOVA skipped.")
  }
  if (verbose) {
    message("Nexus analysis: ", nlevels(trt), " treatments, ", nrow(data),
            " rows. Best by nexus index: ",
            nx$treatment[which.max(nx$nexus_index)], "; by TOPSIS: ",
            ts$treatment[which.max(ts$closeness)], ".")
  }
  structure(list(plot_data = pd, treatment_means = tm, nexus = nx,
                 anova = av, topsis = ts, weights = attr(nx, "weights")),
            class = "nexus_analysis")
}

#' Print method for nexus_analysis
#'
#' @param x An object of class \code{nexus_analysis}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns \code{x}. Called for the side effect of
#'   printing the ranking of treatments.
#'
#' @export
print.nexus_analysis <- function(x, ...) {
  cat("WEFNC Nexus Analysis\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  cat("Weights:", paste0(names(x$weights), " = ", round(x$weights, 3),
                         collapse = ", "), "\n\n")
  out <- data.frame(
    treatment = x$nexus$treatment,
    nexus_index = x$nexus$nexus_index,
    nexus_rank = x$nexus$rank,
    topsis_closeness = x$topsis$closeness,
    topsis_rank = x$topsis$rank,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$nexus_rank), ]
  print.data.frame(out, row.names = FALSE)
  cat("\nComponents: $nexus, $anova, $topsis, $plot_data,",
      "$treatment_means\n")
  invisible(x)
}
