# =============================================================================
# ENERGY MODULE
# Energy Budgeting, EROI, and Energy Efficiency for Cropping Systems
# =============================================================================

#' Total Energy Input
#'
#' Computes total energy input for a cropping system from individual
#' input components with energy equivalents.
#'
#'
#' @details Formula: \deqn{EI = \sum (Input_i \times EE_i)}
#'
#' @param seed Numeric. Energy from seed (MJ/ha). Default 0.
#' @param fertilizer_n Numeric. Energy from nitrogen fertilizer (MJ/ha).
#'   Default 0.
#' @param fertilizer_p Numeric. Energy from phosphorus fertilizer (MJ/ha).
#'   Default 0.
#' @param fertilizer_k Numeric. Energy from potassium fertilizer (MJ/ha).
#'   Default 0.
#' @param fym Numeric. Energy from farmyard manure (MJ/ha). Default 0.
#' @param pesticide Numeric. Energy from pesticides: herbicides, insecticides,
#'   fungicides (MJ/ha). Default 0.
#' @param diesel Numeric. Energy from diesel fuel (MJ/ha). Default 0.
#' @param electricity Numeric. Energy from electricity for irrigation (MJ/ha).
#'   Default 0.
#' @param human_labour Numeric. Energy from human labour (MJ/ha). Default 0.
#' @param machinery Numeric. Energy from machinery depreciation (MJ/ha).
#'   Default 0.
#' @param irrigation Numeric. Energy for canal/pumped irrigation (MJ/ha).
#'   Default 0.
#' @param micronutrient Numeric. Energy from micronutrient fertilizers
#'   (MJ/ha). Default 0.
#' @param biofertilizer Numeric. Energy from biofertilizers (MJ/ha).
#'   Default 0.
#' @param solar_energy Numeric. Energy from solar pumping systems (MJ/ha).
#'   Default 0.
#' @param other Numeric. Any additional energy inputs (MJ/ha). Default 0.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric. Total energy input (MJ/ha), computed as the sum of all
#'   individual energy input components.
#'
#' @examples
#' energy_input(seed = 250, fertilizer_n = 3600, fertilizer_p = 500,
#'              diesel = 2800, human_labour = 180, machinery = 1200,
#'              irrigation = 1500)
#'
#' @references
#' Mittal, V.K. & Dhawan, K.C. (1988). Research manual on energy
#' requirements in agricultural sector. Punjab Agricultural University,
#' Ludhiana, India.
#'
#' Chaudhary, V.P. et al. (2009). Energy auditing of diversified
#' rice-wheat cropping systems in the Indo-Gangetic Plains.
#' \emph{Energy}, 34(9), 1091-1096. \doi{10.1016/j.energy.2009.04.017}
#'
#' @export
energy_input <- function(seed = 0, fertilizer_n = 0, fertilizer_p = 0,
                         fertilizer_k = 0, fym = 0, pesticide = 0,
                         diesel = 0, electricity = 0, human_labour = 0,
                         machinery = 0, irrigation = 0,
                         micronutrient = 0, biofertilizer = 0,
                         solar_energy = 0, other = 0,
                         verbose = TRUE) {
  total <- seed + fertilizer_n + fertilizer_p + fertilizer_k + fym +
    pesticide + diesel + electricity + human_labour + machinery +
    irrigation + micronutrient + biofertilizer + solar_energy + other
  if (verbose) message("Total energy input: ", round(total, 2), " MJ/ha.")
  return(total)
}

#' Total Energy Output
#'
#' Computes total energy output from grain and straw/stover yields using
#' crop-specific energy coefficients.
#'
#' @param grain_yield Numeric vector. Grain or economic yield (kg/ha).
#' @param straw_yield Numeric vector. Straw, stover, or by-product yield
#'   (kg/ha). Default 0.
#' @param grain_energy_coeff Numeric. Energy coefficient for grain (MJ/kg).
#'   Default 14.7 (wheat grain equivalent).
#' @param straw_energy_coeff Numeric. Energy coefficient for straw (MJ/kg).
#'   Default 12.5 (wheat straw equivalent).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. Total energy output (MJ/ha).
#'
#' @details
#' \deqn{E_{out} = (Y_g \times C_g) + (Y_s \times C_s)}{E_out = (Yg * Cg) + (Ys * Cs)}
#' where Y_g and Y_s are grain and straw yields, and C_g and C_s are
#' their respective energy coefficients.
#'
#' @examples
#' energy_output(grain_yield = 4500, straw_yield = 5500)
#' energy_output(grain_yield = 1500, straw_yield = 2000,
#'               grain_energy_coeff = 14.3, straw_energy_coeff = 12.5)
#'
#' @references
#' Devasenapathy, P., Senthilkumar, G. & Shanmugam, P.M. (2009).
#' Energy management in crop production. Indian Journal of Agronomy,
#' 54(1), 80-90.
#'
#' @export
energy_output <- function(grain_yield, straw_yield = 0,
                          grain_energy_coeff = 14.7,
                          straw_energy_coeff = 12.5,
                          verbose = TRUE) {
  .validate_numeric(grain_yield, "grain_yield")
  .validate_non_negative(grain_yield, "grain_yield")
  total <- (grain_yield * grain_energy_coeff) + (straw_yield * straw_energy_coeff)
  if (verbose) message("Energy output computed (MJ/ha).")
  return(round(total, 2))
}

#' Energy Use Efficiency (EUE)
#'
#' Ratio of total energy output to total energy input.
#'
#' @param energy_out Numeric vector. Total energy output (MJ/ha).
#' @param energy_in Numeric vector. Total energy input (MJ/ha).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. Energy use efficiency (dimensionless ratio).
#'   Values greater than 1 indicate a positive energy balance.
#'
#' @details
#' \deqn{EUE = \frac{E_{out}}{E_{in}}}{EUE = E_out / E_in}
#'
#' @examples
#' energy_use_efficiency(energy_out = 135000, energy_in = 12000)
#'
#' @export
energy_use_efficiency <- function(energy_out, energy_in, verbose = TRUE) {
  .validate_numeric(energy_out, "energy_out")
  .validate_positive(energy_in, "energy_in")
  .validate_same_length(energy_out, energy_in)
  result <- energy_out / energy_in
  if (verbose) message("EUE computed: ", length(result), " values.")
  return(round(result, 2))
}

#' Energy Return on Investment (EROI)
#'
#' Computes the Energy Return on Investment, a key metric for assessing
#' whether an agricultural system produces more energy than it consumes.
#' EROI is mathematically identical to energy use efficiency (EUE) but
#' is the preferred term in energy and sustainability science literature.
#'
#' @param energy_out Numeric vector. Total energy output from the
#'   agricultural system (MJ/ha), including grain, straw, and any
#'   co-products.
#' @param energy_in Numeric vector. Total energy input invested in
#'   the agricultural system (MJ/ha), including all direct (diesel,
#'   electricity) and indirect (fertilizer manufacture, machinery
#'   depreciation) energy inputs.
#' @param include_solar Logical. If \code{TRUE}, includes captured solar
#'   energy in the output. Default \code{FALSE} (standard practice).
#' @param verbose Logical. If \code{TRUE}, prints informational messages
#'   and interpretation. Default \code{TRUE}.
#'
#' @return Numeric vector. EROI values (dimensionless ratio). Interpreted
#'   as follows:
#' \describe{
#'   \item{EROI > 5}{Highly energy-profitable system}
#'   \item{1 < EROI < 5}{Energy-positive but moderate return}
#'   \item{EROI = 1}{Break-even: energy output equals input}
#'   \item{EROI < 1}{Energy sink: system consumes more than it produces}
#' }
#'
#' @details
#' \deqn{EROI = \frac{E_{out}}{E_{in}}}{EROI = E_out / E_in}
#'
#' EROI is the fundamental metric of net energy analysis. In agricultural
#' contexts, EROI typically ranges from 2 to 15 for conventional
#' cropping systems, with higher values for low-input or conservation
#' agriculture systems.
#'
#' @examples
#' # Conservation agriculture with low input
#' eroi(energy_out = 59800, energy_in = 8500)
#'
#' # Conventional tillage with high input
#' eroi(energy_out = 40800, energy_in = 12500)
#'
#' # Multiple treatments
#' eroi(energy_out = c(40800, 50500, 59800),
#'      energy_in = c(12500, 9800, 8500))
#'
#' @references
#' Hall, C.A.S., Lambert, J.G. & Balogh, S.B. (2014). EROI of different
#' fuels and the implications for society. \emph{Energy Policy}, 64,
#' 141-152. \doi{10.1016/j.enpol.2013.05.049}
#'
#' Murphy, D.J. & Hall, C.A.S. (2010). Year in review - EROI or energy
#' return on (energy) invested. \emph{Annals of the New York Academy of
#' Sciences}, 1185(1), 102-118. \doi{10.1111/j.1749-6632.2009.05282.x}
#'
#' Murphy, D.J. et al. (2022). Energy return on investment of major
#' energy carriers: Review and harmonization. \emph{Sustainability},
#' 14(12), 7098. \doi{10.3390/su14127098}
#'
#' @export
eroi <- function(energy_out, energy_in, include_solar = FALSE,
                 verbose = TRUE) {
  .validate_numeric(energy_out, "energy_out")
  .validate_positive(energy_in, "energy_in")
  .validate_same_length(energy_out, energy_in)
  result <- energy_out / energy_in
  if (verbose) {
    categories <- ifelse(result > 5, "highly profitable",
                         ifelse(result > 1, "energy-positive",
                                ifelse(result == 1, "break-even", "energy sink")))
    message("EROI computed: ", paste(round(result, 2), collapse = ", "),
            " (", paste(categories, collapse = ", "), ").")
  }
  return(round(result, 2))
}

#' Energy Productivity
#'
#' Crop yield produced per unit of energy input.
#'
#'
#' @details Formula: \deqn{EP = \frac{Yield}{Energy Input}}{EP = Yield / Energy Input}
#'
#' @param yield Numeric vector. Crop yield (kg/ha).
#' @param energy_in Numeric vector. Total energy input (MJ/ha).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. Energy productivity (kg/MJ). Higher values
#'   indicate better yield per unit energy invested.
#'
#' @examples
#' energy_productivity(yield = 4500, energy_in = 12000)
#'
#' @export
energy_productivity <- function(yield, energy_in, verbose = TRUE) {
  .validate_numeric(yield, "yield")
  .validate_positive(energy_in, "energy_in")
  .validate_same_length(yield, energy_in)
  result <- yield / energy_in
  if (verbose) message("Energy productivity computed (kg/MJ).")
  return(round(result, 4))
}

#' Energy Intensity
#'
#' Energy consumed per unit of crop yield.
#'
#'
#' @details Formula: \deqn{EI = \frac{Energy Input}{Yield}}{EI = Energy Input / Yield}
#'
#' @param energy_in Numeric vector. Total energy input (MJ/ha).
#' @param yield Numeric vector. Crop yield (kg/ha).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. Energy intensity (MJ/kg). Lower values indicate
#'   more energy-efficient production.
#'
#' @examples
#' energy_intensity(energy_in = 12000, yield = 4500)
#'
#' @export
energy_intensity <- function(energy_in, yield, verbose = TRUE) {
  .validate_numeric(energy_in, "energy_in")
  .validate_positive(yield, "yield")
  .validate_same_length(energy_in, yield)
  result <- energy_in / yield
  if (verbose) message("Energy intensity computed (MJ/kg).")
  return(round(result, 2))
}

#' Specific Energy
#'
#' Energy input per unit area per unit time, useful for comparing
#' cropping systems of different durations.
#'
#'
#' @details Formula: \deqn{SE = \frac{Energy Input}{Area}}{SE = Energy Input / Area}
#'
#' @param energy_in Numeric vector. Total energy input (MJ/ha).
#' @param duration Numeric vector. Crop duration (days).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. Specific energy (MJ/ha/day).
#'
#' @examples
#' specific_energy(energy_in = 12000, duration = 150)
#'
#' @export
specific_energy <- function(energy_in, duration, verbose = TRUE) {
  .validate_numeric(energy_in, "energy_in")
  .validate_positive(duration, "duration")
  .validate_same_length(energy_in, duration)
  result <- energy_in / duration
  if (verbose) message("Specific energy computed (MJ/ha/day).")
  return(round(result, 2))
}

#' Net Energy
#'
#' Difference between total energy output and total energy input.
#'
#'
#' @details Formula: \deqn{NE = Energy Output - Energy Input}{NE = Energy Output - Energy Input}
#'
#' @param energy_out Numeric vector. Total energy output (MJ/ha).
#' @param energy_in Numeric vector. Total energy input (MJ/ha).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. Net energy (MJ/ha). Positive values indicate
#'   net energy gain; negative values indicate net energy loss.
#'
#' @examples
#' net_energy(energy_out = 135000, energy_in = 12000)
#'
#' @export
net_energy <- function(energy_out, energy_in, verbose = TRUE) {
  .validate_numeric(energy_out, "energy_out")
  .validate_numeric(energy_in, "energy_in")
  .validate_same_length(energy_out, energy_in)
  result <- energy_out - energy_in
  if (verbose) message("Net energy computed (MJ/ha).")
  return(round(result, 2))
}

#' Energy Profitability
#'
#' Economic return per unit of energy invested.
#'
#'
#' @details Formula: \deqn{EPr = \frac{Energy Output - Energy Input}{Energy Input}}{EPr = (Energy Output - Energy Input) / Energy Input}
#'
#' @param gross_return Numeric vector. Gross economic return (currency/ha).
#' @param energy_in Numeric vector. Total energy input (MJ/ha).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. Energy profitability (currency/MJ).
#'
#' @examples
#' energy_profitability(gross_return = 112500, energy_in = 12000)
#'
#' @export
energy_profitability <- function(gross_return, energy_in, verbose = TRUE) {
  .validate_numeric(gross_return, "gross_return")
  .validate_positive(energy_in, "energy_in")
  .validate_same_length(gross_return, energy_in)
  result <- gross_return / energy_in
  if (verbose) message("Energy profitability computed (currency/MJ).")
  return(round(result, 2))
}

#' Human Energy Profitability
#'
#' Ratio of energy output to human labour energy input, measuring
#' the degree of human labour amplification by the farming system.
#'
#'
#' @details Formula: \deqn{HEP = \frac{Energy Output}{Human Energy Input}}{HEP = Energy Output / Human Energy Input}
#'
#' @param energy_out Numeric vector. Total energy output (MJ/ha).
#' @param human_energy Numeric vector. Human labour energy input (MJ/ha).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. Human energy profitability (dimensionless ratio).
#'
#' @examples
#' human_energy_profitability(energy_out = 135000, human_energy = 180)
#'
#' @export
human_energy_profitability <- function(energy_out, human_energy,
                                       verbose = TRUE) {
  .validate_numeric(energy_out, "energy_out")
  .validate_positive(human_energy, "human_energy")
  .validate_same_length(energy_out, human_energy)
  result <- energy_out / human_energy
  if (verbose) message("Human energy profitability computed.")
  return(round(result, 2))
}
