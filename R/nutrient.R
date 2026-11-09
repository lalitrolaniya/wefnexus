# =============================================================================
# NUTRIENT MODULE
# Nutrient Use Efficiency Metrics for N, P, K
# =============================================================================

#' Agronomic Efficiency (AE)
#'
#' Increase in economic yield per unit of nutrient applied.
#'
#' @param yield_treated Numeric vector. Yield with nutrient applied (kg/ha).
#' @param yield_control Numeric vector. Yield without nutrient, i.e., from
#'   control plot (kg/ha).
#' @param nutrient_applied Numeric vector. Amount of nutrient applied (kg/ha).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @return Numeric vector. Agronomic efficiency (kg grain per kg nutrient).
#'
#' @details
#' \deqn{AE = \frac{Y_f - Y_0}{F}}{AE = (Yf - Y0) / F}
#'
#' @examples
#' agronomic_efficiency(yield_treated = 4500, yield_control = 3000,
#'                      nutrient_applied = 120)
#'
#' @references
#' Dobermann, A. (2007). Nutrient use efficiency: measurement and
#' management. In \emph{Fertilizer Best Management Practices}, IFA,
#' Paris, pp. 1-28.
#' \url{https://digitalcommons.unl.edu/agronomyfacpub/316/}
#'
#' Fixen, P. et al. (2015). Nutrient/fertilizer use efficiency:
#' Measurement, current situation and trends. In \emph{Managing Water
#' and Fertilizer for Sustainable Agricultural Intensification},
#' IFA/IWMI/IPNI/IPI, pp. 8-38.
#'
#' Congreves, K.A. et al. (2021). Nitrogen use efficiency definitions
#' of today and tomorrow. \emph{Frontiers in Plant Science}, 12,
#' 637108. \doi{10.3389/fpls.2021.637108}
#'
#' @export
agronomic_efficiency <- function(yield_treated, yield_control,
                                 nutrient_applied, verbose = getOption("wefnexus.verbose", TRUE)) {
  .validate_numeric(yield_treated, "yield_treated")
  .validate_numeric(yield_control, "yield_control")
  .validate_positive(nutrient_applied, "nutrient_applied")
  ae <- (yield_treated - yield_control) / nutrient_applied
  if (verbose) message("Agronomic efficiency computed (kg/kg).")
  return(round(ae, 2))
}

#' Physiological Efficiency (PE)
#'
#' Yield increase per unit of nutrient uptake increase.
#'
#' @param yield_treated Numeric vector. Yield with nutrient applied (kg/ha).
#' @param yield_control Numeric vector. Yield without nutrient (kg/ha).
#' @param uptake_treated Numeric vector. Total nutrient uptake with
#'   fertilizer (kg/ha).
#' @param uptake_control Numeric vector. Total nutrient uptake without
#'   fertilizer (kg/ha).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @return Numeric vector. Physiological efficiency (kg grain per kg
#'   nutrient uptake). Returns \code{NA} where uptake difference is zero.
#'
#' @details
#' \deqn{PE = \frac{Y_f - Y_0}{U_f - U_0}}{PE = (Yf - Y0) / (Uf - U0)}
#'
#' @examples
#' physiological_efficiency(yield_treated = 4500, yield_control = 3000,
#'                          uptake_treated = 100, uptake_control = 60)
#'
#' @export
physiological_efficiency <- function(yield_treated, yield_control,
                                     uptake_treated, uptake_control,
                                     verbose = getOption("wefnexus.verbose", TRUE)) {
  .validate_numeric(yield_treated, "yield_treated")
  .validate_numeric(yield_control, "yield_control")
  .validate_numeric(uptake_treated, "uptake_treated")
  .validate_numeric(uptake_control, "uptake_control")
  delta_uptake <- uptake_treated - uptake_control
  if (any(delta_uptake == 0, na.rm = TRUE)) {
    warning("Zero difference in uptake detected; returning NA for those.")
  }
  pe <- ifelse(delta_uptake == 0, NA_real_,
               (yield_treated - yield_control) / delta_uptake)
  if (verbose) message("Physiological efficiency computed.")
  return(round(pe, 2))
}

#' Recovery Efficiency (RE)
#'
#' Proportion of applied nutrient that is recovered in crop biomass.
#'
#' @param uptake_treated Numeric vector. Total nutrient uptake with
#'   fertilizer (kg/ha).
#' @param uptake_control Numeric vector. Total nutrient uptake without
#'   fertilizer (kg/ha).
#' @param nutrient_applied Numeric vector. Nutrient applied (kg/ha).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @return Numeric vector. Recovery efficiency as a proportion (0 to 1,
#'   but can exceed 1 due to priming effects or added benefits).
#'
#' @details
#' \deqn{RE = \frac{U_f - U_0}{F}}{RE = (Uf - U0) / F}
#'
#' @examples
#' recovery_efficiency(uptake_treated = 100, uptake_control = 60,
#'                     nutrient_applied = 120)
#'
#' @export
recovery_efficiency <- function(uptake_treated, uptake_control,
                                nutrient_applied, verbose = getOption("wefnexus.verbose", TRUE)) {
  .validate_numeric(uptake_treated, "uptake_treated")
  .validate_numeric(uptake_control, "uptake_control")
  .validate_positive(nutrient_applied, "nutrient_applied")
  re <- (uptake_treated - uptake_control) / nutrient_applied
  if (verbose) message("Recovery efficiency computed.")
  return(round(re, 4))
}

#' Partial Factor Productivity (PFP)
#'
#' Total yield per unit of nutrient applied, without deduction of
#' control yield.
#'
#' @param yield Numeric vector. Crop yield (kg/ha).
#' @param nutrient_applied Numeric vector. Nutrient applied (kg/ha).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @return Numeric vector. Partial factor productivity (kg yield per
#'   kg nutrient applied).
#'
#' @details
#' \deqn{PFP = \frac{Y}{F}}{PFP = Y / F}
#'
#' @examples
#' partial_factor_productivity(yield = 4500, nutrient_applied = 120)
#'
#' @export
partial_factor_productivity <- function(yield, nutrient_applied,
                                        verbose = getOption("wefnexus.verbose", TRUE)) {
  .validate_numeric(yield, "yield")
  .validate_positive(nutrient_applied, "nutrient_applied")
  .validate_same_length(yield, nutrient_applied)
  result <- yield / nutrient_applied
  if (verbose) message("PFP computed (kg/kg).")
  return(round(result, 2))
}

#' Internal Utilization Efficiency (IUE)
#'
#' Yield produced per unit of total nutrient in the plant.
#'
#'
#' @details Formula: \deqn{IE = \frac{Yield}{Total Nutrient Uptake}}{IE = Yield / Total Nutrient Uptake (kg grain/kg nutrient)}
#'
#' @param yield Numeric vector. Crop yield (kg/ha).
#' @param total_uptake Numeric vector. Total plant nutrient uptake (kg/ha).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @return Numeric vector. Internal utilization efficiency (kg yield per
#'   kg nutrient uptake).
#'
#' @examples
#' internal_utilization_efficiency(yield = 4500, total_uptake = 100)
#'
#' @export
internal_utilization_efficiency <- function(yield, total_uptake,
                                            verbose = getOption("wefnexus.verbose", TRUE)) {
  .validate_numeric(yield, "yield")
  .validate_positive(total_uptake, "total_uptake")
  .validate_same_length(yield, total_uptake)
  result <- yield / total_uptake
  if (verbose) message("IUE computed (kg/kg).")
  return(round(result, 2))
}

#' Nutrient Harvest Index (NHI)
#'
#' Proportion of total plant nutrient partitioned to grain.
#'
#'
#' @details Formula: \deqn{NHI = \frac{U_{grain}}{U_{total}}}{NHI = U_grain / U_total}
#'   where \eqn{U_{grain}} is nutrient uptake in grain and
#'   \eqn{U_{total}} is total plant nutrient uptake (kg/ha).
#'
#' @param grain_nutrient_uptake Numeric vector. Nutrient uptake in grain
#'   (kg/ha).
#' @param total_nutrient_uptake Numeric vector. Total plant nutrient
#'   uptake (kg/ha).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @return Numeric vector. Nutrient harvest index (proportion, 0 to 1).
#'
#' @examples
#' nutrient_harvest_index(grain_nutrient_uptake = 75,
#'                        total_nutrient_uptake = 100)
#'
#' @export
nutrient_harvest_index <- function(grain_nutrient_uptake,
                                   total_nutrient_uptake,
                                   verbose = getOption("wefnexus.verbose", TRUE)) {
  .validate_numeric(grain_nutrient_uptake, "grain_nutrient_uptake")
  .validate_positive(total_nutrient_uptake, "total_nutrient_uptake")
  .validate_same_length(grain_nutrient_uptake, total_nutrient_uptake)
  nhi <- grain_nutrient_uptake / total_nutrient_uptake
  if (verbose) message("NHI computed.")
  return(round(nhi, 4))
}

#' Nutrient Balance Sheet
#'
#' Input-output nutrient balance for major nutrients (N, P, K).
#'
#'
#' @details Formula: \deqn{NB = Nutrient Applied - Nutrient Removed}{NB = Nutrient Applied - Nutrient Removed (kg/ha)}
#'
#' @param n_input Numeric. Total N input from all sources (kg/ha).
#' @param p_input Numeric. Total P input (kg/ha).
#' @param k_input Numeric. Total K input (kg/ha).
#' @param n_output Numeric. Total N removal or uptake (kg/ha).
#' @param p_output Numeric. Total P removal or uptake (kg/ha).
#' @param k_output Numeric. Total K removal or uptake (kg/ha).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @return A data frame with five columns:
#' \describe{
#'   \item{nutrient}{Character. Nutrient name: N, P, or K}
#'   \item{input_kg_ha}{Numeric. Total nutrient input (kg/ha)}
#'   \item{output_kg_ha}{Numeric. Total nutrient output (kg/ha)}
#'   \item{balance_kg_ha}{Numeric. Balance: input minus output (kg/ha).
#'     Positive = surplus; negative = depletion}
#'   \item{output_input_ratio}{Numeric. Output to input ratio
#'     (\code{NA} when the input is zero)}
#' }
#'
#' @details Zero inputs are allowed (for example no K applied), giving
#'   a negative balance that shows nutrient mining.
#'
#' @examples
#' nutrient_balance(n_input = 120, p_input = 60, k_input = 40,
#'                  n_output = 95, p_output = 25, k_output = 80)
#' # No potassium applied: balance shows K mining
#' nutrient_balance(n_input = 20, p_input = 40, k_input = 0,
#'                  n_output = 56.8, p_output = 8.2, k_output = 45)
#'
#' @export
nutrient_balance <- function(n_input, p_input, k_input,
                             n_output, p_output, k_output,
                             verbose = getOption("wefnexus.verbose", TRUE)) {
  inputs <- c(n_input, p_input, k_input)
  outputs <- c(n_output, p_output, k_output)
  .validate_non_negative(inputs, "nutrient inputs")
  .validate_non_negative(outputs, "nutrient outputs")
  result <- data.frame(
    nutrient = c("N", "P", "K"),
    input_kg_ha = inputs,
    output_kg_ha = outputs,
    balance_kg_ha = round(inputs - outputs, 2),
    output_input_ratio = ifelse(inputs > 0, round(outputs / inputs, 3),
                                NA_real_),
    stringsAsFactors = FALSE
  )
  if (verbose) message("Nutrient balance sheet computed for N, P, K.")
  return(result)
}

#' Comprehensive Nutrient Use Efficiency (NUE)
#'
#' Computes all major nutrient use efficiency metrics for a given
#' nutrient in a single call.
#'
#'
#' @details Returns agronomic efficiency (AE), physiological efficiency
#'   (PE), recovery efficiency (RE), partial factor productivity (PFP),
#'   internal utilization efficiency (IUE) and nutrient harvest index
#'   (NHI). See the individual functions for formulas. Versions before
#'   1.1.0 returned a fixed NHI of 0.75; NHI is now computed from
#'   \code{grain_uptake}.
#'
#' @param yield_treated Numeric. Yield with nutrient applied (kg/ha).
#' @param yield_control Numeric. Yield without nutrient (kg/ha).
#' @param nutrient_applied Numeric. Nutrient applied (kg/ha).
#' @param uptake_treated Numeric. Total nutrient uptake with
#'   fertilizer (kg/ha).
#' @param uptake_control Numeric. Total nutrient uptake without
#'   fertilizer (kg/ha).
#' @param nutrient_name Character. Name of the nutrient for labeling
#'   (e.g., "N", "P", "K"). Default "N".
#' @param grain_uptake Numeric. Nutrient uptake in grain of the treated
#'   crop (kg/ha). Used to compute the nutrient harvest index. If
#'   \code{NULL} (default), the nutrient harvest index is returned as
#'   \code{NA}.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @return A data frame with one row and seven columns:
#' \describe{
#'   \item{nutrient}{Character. Nutrient name}
#'   \item{agronomic_eff}{Numeric. Agronomic efficiency (kg/kg)}
#'   \item{physiological_eff}{Numeric. Physiological efficiency (kg/kg)}
#'   \item{recovery_eff}{Numeric. Recovery efficiency (proportion)}
#'   \item{partial_factor_prod}{Numeric. Partial factor productivity
#'     (kg/kg)}
#'   \item{internal_util_eff}{Numeric. Internal utilization efficiency
#'     (kg/kg)}
#'   \item{nutrient_harvest_idx}{Numeric. Nutrient harvest index
#'     (grain uptake / total uptake). \code{NA} when
#'     \code{grain_uptake} is not supplied.}
#' }
#'
#' @examples
#' nutrient_use_efficiency(
#'   yield_treated = 4500, yield_control = 3000,
#'   nutrient_applied = 120, uptake_treated = 100,
#'   uptake_control = 60, nutrient_name = "N", grain_uptake = 72
#' )
#'
#' @references
#' Dobermann, A. (2007). Nutrient use efficiency: measurement and
#' management. In \emph{Fertilizer Best Management Practices}, IFA,
#' Paris, pp. 1-28.
#' \url{https://digitalcommons.unl.edu/agronomyfacpub/316/}
#'
#' Congreves, K.A. et al. (2021). Nitrogen use efficiency definitions
#' of today and tomorrow. \emph{Frontiers in Plant Science}, 12,
#' 637108. \doi{10.3389/fpls.2021.637108}
#'
#' @export
nutrient_use_efficiency <- function(yield_treated, yield_control,
                                    nutrient_applied,
                                    uptake_treated, uptake_control,
                                    nutrient_name = "N",
                                    grain_uptake = NULL,
                                    verbose = getOption("wefnexus.verbose", TRUE)) {
  ae <- agronomic_efficiency(yield_treated, yield_control,
                             nutrient_applied, verbose = FALSE)
  pe <- physiological_efficiency(yield_treated, yield_control,
                                 uptake_treated, uptake_control,
                                 verbose = FALSE)
  re <- recovery_efficiency(uptake_treated, uptake_control,
                            nutrient_applied, verbose = FALSE)
  pfp <- partial_factor_productivity(yield_treated, nutrient_applied,
                                     verbose = FALSE)
  iue <- internal_utilization_efficiency(yield_treated, uptake_treated,
                                         verbose = FALSE)
  nhi <- if (is.null(grain_uptake)) {
    rep(NA_real_, length(ae))
  } else {
    nutrient_harvest_index(grain_uptake, uptake_treated, verbose = FALSE)
  }

  result <- data.frame(
    nutrient = nutrient_name,
    agronomic_eff = ae,
    physiological_eff = pe,
    recovery_eff = re,
    partial_factor_prod = pfp,
    internal_util_eff = iue,
    nutrient_harvest_idx = nhi,
    stringsAsFactors = FALSE
  )
  if (verbose) message("Complete NUE report for ", nutrient_name, ".")
  return(result)
}
