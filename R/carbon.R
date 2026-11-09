# =============================================================================
# CARBON MODULE
# Carbon Footprint, GHG Emissions, SOC Stocks & Global Warming Potential
# GWP values updated to IPCC AR6 (CH4 = 27, N2O = 273)
# =============================================================================

#' Carbon Footprint of Crop Production
#'
#' Estimates the carbon footprint from major emission sources in crop
#' production, with source-wise breakdown and yield-scaled intensity.
#' Uses IPCC AR6 default GWP values.
#'
#' @param diesel_use Numeric. Diesel consumption (L/ha). Default 0.
#' @param electricity_use Numeric. Electricity consumption (kWh/ha).
#'   Default 0.
#' @param n_fertilizer Numeric. Nitrogen fertilizer applied (kg N/ha).
#'   Default 0.
#' @param p_fertilizer Numeric. Phosphorus fertilizer applied
#'   (kg P2O5/ha). Default 0.
#' @param k_fertilizer Numeric. Potassium fertilizer applied
#'   (kg K2O/ha). Default 0.
#' @param pesticide_use Numeric. Pesticide active ingredient used
#'   (kg a.i./ha). Default 0.
#' @param seed_rate Numeric. Seed rate (kg/ha). Default 0.
#' @param n2o_direct Numeric. Direct N2O emissions from soil
#'   (kg N2O/ha). If \code{NULL} (default), estimated as
#'   \code{n_fertilizer * ef_n2o * 44/28} (IPCC Tier 1).
#' @param ef_n2o Numeric. Direct N2O emission factor (kg N2O-N per kg N
#'   applied). Default 0.01, the IPCC aggregated Tier 1 value. The 2019
#'   IPCC Refinement gives 0.005 for dry climates (relevant to arid
#'   zones) and 0.016 for synthetic N in wet climates.
#' @param ch4_emission Numeric. Methane emission (kg CH4/ha). Default 0.
#'   Relevant for rice paddies.
#'
#' @details Formula: \deqn{CF = \sum_i A_i \times EF_i}{CF = sum(A_i * EF_i)}
#'   in kg CO2-eq/ha, where \eqn{A_i} is the activity (input use) and
#'   \eqn{EF_i} its emission factor; N2O and CH4 are converted with the
#'   GWP values. When \code{yield} is given, the carbon intensity
#'   \eqn{CF / Y} (kg CO2-eq/kg) is also returned.
#'
#' @param yield Numeric. Crop yield (kg/ha) for intensity calculation.
#'   Default \code{NULL}.
#' @param ef_diesel Numeric. Emission factor for diesel (kg CO2/L).
#'   Default 2.68.
#' @param ef_electricity Numeric. Emission factor for grid electricity
#'   (kg CO2/kWh). Default 0.727, the weighted average for the Indian
#'   grid in 2023-24 (Central Electricity Authority, CO2 Baseline
#'   Database version 20.0). Versions before 1.1.0 used 0.82; set
#'   \code{ef_electricity = 0.82} to reproduce earlier results. Use
#'   your own country or year value where appropriate.
#' @param ef_n_manufacture Numeric. Emission factor for N fertilizer
#'   manufacture (kg CO2-eq/kg N). Default 4.96 (urea-based).
#' @param ef_p_manufacture Numeric. Emission factor for P fertilizer
#'   manufacture (kg CO2-eq/kg P2O5). Default 1.61.
#' @param ef_k_manufacture Numeric. Emission factor for K fertilizer
#'   manufacture (kg CO2-eq/kg K2O). Default 0.57.
#' @param ef_pesticide Numeric. Emission factor for pesticide
#'   (kg CO2-eq/kg a.i.). Default 10.97.
#' @param ef_seed Numeric. Emission factor for seed production
#'   (kg CO2-eq/kg). Default 0.58.
#' @param gwp_n2o Numeric. Global warming potential of N2O relative
#'   to CO2. Default 273 (IPCC AR6, 100-year horizon).
#' @param gwp_ch4 Numeric. Global warming potential of CH4 relative
#'   to CO2. Default 27 (IPCC AR6, 100-year horizon).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @return A list with components:
#' \describe{
#'   \item{total_cf}{Numeric. Total carbon footprint (kg CO2-eq/ha)}
#'   \item{cf_intensity}{Numeric. Carbon footprint intensity
#'     (kg CO2-eq/kg yield), returned only when \code{yield} is
#'     provided and positive}
#'   \item{breakdown}{Data frame with columns: source, emission_kg_CO2eq,
#'     and share_pct showing the contribution of each emission source}
#' }
#'
#' @examples
#' cf <- carbon_footprint(
#'   diesel_use = 60, electricity_use = 200,
#'   n_fertilizer = 120, p_fertilizer = 60, k_fertilizer = 40,
#'   pesticide_use = 1.5, seed_rate = 100, yield = 4500
#' )
#' cf$total_cf
#' cf$cf_intensity
#' cf$breakdown
#'
#' @references
#' Lal, R. (2004). Carbon emission from farm operations.
#' \emph{Environment International}, 30(7), 981-990.
#' \doi{10.1016/j.envint.2004.03.005}
#'
#' Central Electricity Authority (2024). \emph{CO2 Baseline Database for
#' the Indian Power Sector}, Version 20.0. Ministry of Power, Government
#' of India, New Delhi.
#'
#' Forster, P. et al. (2021). The Earth's energy budget, climate
#' feedbacks, and climate sensitivity. In \emph{Climate Change 2021:
#' The Physical Science Basis} (IPCC AR6 WGI Chapter 7).
#' \doi{10.1017/9781009157896.009}
#'
#' IPCC (2019). \emph{2019 Refinement to the 2006 IPCC Guidelines for
#' National Greenhouse Gas Inventories}. Volume 4: Agriculture,
#' Forestry and Other Land Use. ISBN:978-4-88788-232-4.
#'
#' @export
carbon_footprint <- function(diesel_use = 0, electricity_use = 0,
                             n_fertilizer = 0, p_fertilizer = 0,
                             k_fertilizer = 0, pesticide_use = 0,
                             seed_rate = 0, n2o_direct = NULL,
                             ch4_emission = 0, yield = NULL,
                             ef_diesel = 2.68, ef_electricity = 0.727,
                             ef_n_manufacture = 4.96,
                             ef_p_manufacture = 1.61,
                             ef_k_manufacture = 0.57,
                             ef_pesticide = 10.97, ef_seed = 0.58,
                             gwp_n2o = 273, gwp_ch4 = 27,
                             ef_n2o = 0.01,
                             verbose = getOption("wefnexus.verbose", TRUE)) {
  # Source-wise emissions
  em_diesel <- diesel_use * ef_diesel
  em_electricity <- electricity_use * ef_electricity
  em_n_fert <- n_fertilizer * ef_n_manufacture
  em_p_fert <- p_fertilizer * ef_p_manufacture
  em_k_fert <- k_fertilizer * ef_k_manufacture
  em_pesticide <- pesticide_use * ef_pesticide
  em_seed <- seed_rate * ef_seed

  # Direct N2O emissions (IPCC Tier 1: 1% of N as N2O-N, convert to N2O)
  if (is.null(n2o_direct)) {
    n2o_direct <- n_fertilizer * ef_n2o * (44 / 28)
  }
  em_n2o <- n2o_direct * gwp_n2o

  # CH4 emissions
  em_ch4 <- ch4_emission * gwp_ch4

  # Total
  total_cf <- em_diesel + em_electricity + em_n_fert + em_p_fert +
    em_k_fert + em_pesticide + em_seed + em_n2o + em_ch4

  # Vectorised: return data.frame when inputs are vectors
  n_obs <- max(length(total_cf), if (is.null(yield)) 1L else length(yield))
  if (n_obs > 1L) {
    rl <- function(v) rep_len(v, n_obs)
    total_cf <- rl(total_cf); em_diesel <- rl(em_diesel)
    em_electricity <- rl(em_electricity); em_n_fert <- rl(em_n_fert)
    em_p_fert <- rl(em_p_fert); em_k_fert <- rl(em_k_fert)
    em_pesticide <- rl(em_pesticide); em_seed <- rl(em_seed)
    em_n2o <- rl(em_n2o); em_ch4 <- rl(em_ch4)
    result <- data.frame(
      total_cf       = round(total_cf, 2),
      diesel         = round(em_diesel, 2),
      electricity    = round(em_electricity, 2),
      n_fertilizer   = round(em_n_fert, 2),
      p_fertilizer   = round(em_p_fert, 2),
      k_fertilizer   = round(em_k_fert, 2),
      pesticide      = round(em_pesticide, 2),
      seed           = round(em_seed, 2),
      n2o_direct     = round(em_n2o, 2),
      ch4            = round(em_ch4, 2),
      stringsAsFactors = FALSE
    )
    if (!is.null(yield)) {
      result$cf_intensity <- round(total_cf / yield, 4)
    }
    if (verbose) {
      message("Carbon footprint computed for ", n_obs, " observations.")
    }
    return(result)
  }

  # Scalar: return list with breakdown (original behaviour)
  sources <- c("Diesel", "Electricity", "N_fertilizer", "P_fertilizer",
               "K_fertilizer", "Pesticide", "Seed", "N2O_direct", "CH4")
  emissions <- c(em_diesel, em_electricity, em_n_fert, em_p_fert,
                  em_k_fert, em_pesticide, em_seed, em_n2o, em_ch4)

  breakdown <- data.frame(
    source = sources,
    emission_kg_CO2eq = round(emissions, 2),
    share_pct = if (total_cf > 0) round(emissions / total_cf * 100, 1) else NA_real_,
    stringsAsFactors = FALSE
  )

  result <- list(
    total_cf = round(total_cf, 2),
    breakdown = breakdown
  )

  if (!is.null(yield) && yield > 0) {
    result$cf_intensity <- round(total_cf / yield, 4)
  }

  if (verbose) {
    message("Carbon footprint: ", round(total_cf, 2), " kg CO2-eq/ha",
            " (GWP AR6: CH4=", gwp_ch4, ", N2O=", gwp_n2o, ").")
  }
  class(result) <- c("cf_result", "list")
  return(result)
}

#' Carbon Efficiency
#'
#' Crop yield per unit of greenhouse gas emitted.
#'
#'
#' @details Formula: \deqn{CE = \frac{Yield}{Carbon Footprint}}{CE = Yield / Carbon Footprint}
#'
#' @param yield Numeric vector. Crop yield (kg/ha).
#' @param carbon_emission Numeric vector. Total GHG emission
#'   (kg CO2-eq/ha).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @return Numeric vector. Carbon efficiency (kg yield per kg CO2-eq).
#'   Higher values indicate more yield per unit emission.
#'
#' @examples
#' carbon_efficiency(yield = 4500, carbon_emission = 2500)
#'
#' @export
carbon_efficiency <- function(yield, carbon_emission, verbose = getOption("wefnexus.verbose", TRUE)) {
  .validate_numeric(yield, "yield")
  .validate_positive(carbon_emission, "carbon_emission")
  .validate_same_length(yield, carbon_emission)
  result <- yield / carbon_emission
  if (verbose) message("Carbon efficiency computed (kg/kg CO2-eq).")
  return(round(result, 4))
}

#' Carbon Sustainability Index (CSI)
#'
#' Ratio of yield to net carbon emission, accounting for carbon
#' sequestration by the soil.
#'
#'
#' @details Formula: \deqn{CSI = \frac{Y}{E - S}}{CSI = Y / (E - S)}
#'   where Y is yield (kg/ha), E is GHG emission and S is carbon
#'   sequestered (both kg CO2-eq/ha).
#'
#' @param yield Numeric vector. Crop yield (kg/ha).
#' @param carbon_emission Numeric vector. Total GHG emission
#'   (kg CO2-eq/ha).
#' @param carbon_sequestered Numeric vector. Carbon sequestered in
#'   soil (kg CO2-eq/ha). Default 0.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @return Numeric vector. Carbon sustainability index. Higher values
#'   indicate better carbon sustainability. Returns \code{NA} where
#'   net emission is zero or negative (net sequestration exceeds emission).
#'
#' @examples
#' carbon_sustainability_index(yield = 4500, carbon_emission = 2500,
#'                              carbon_sequestered = 500)
#'
#' @references
#' Brentrup, F. et al. (2004). Environmental impact assessment of
#' agricultural production systems using the life cycle assessment
#' methodology. \emph{European Journal of Agronomy}, 20(3), 247-264.
#' \doi{10.1016/S1161-0301(03)00024-8}
#'
#' @export
carbon_sustainability_index <- function(yield, carbon_emission,
                                         carbon_sequestered = 0,
                                         verbose = getOption("wefnexus.verbose", TRUE)) {
  .validate_numeric(yield, "yield")
  .validate_numeric(carbon_emission, "carbon_emission")
  net_em <- carbon_emission - carbon_sequestered
  net_em <- ifelse(net_em <= 0, NA_real_, net_em)
  result <- yield / net_em
  if (verbose) message("CSI computed.")
  return(round(result, 4))
}

#' GHG Emission Estimation from Field Operations
#'
#' Quick estimation of greenhouse gas emissions (N2O, CH4, CO2) from
#' agricultural field operations using IPCC Tier 1 default factors.
#'
#' @details Formula: \deqn{GHG = \sum (Activity_i \times EF_i)}{GHG = sum(Activity_i * EF_i)} where EF is the emission factor for each activity.
#'
#' Residue burning follows IPCC (2006, Vol. 4, Ch. 2): mass burnt =
#' residue x combustion factor; CH4 and N2O = mass burnt x emission factor
#' (g/kg dry matter). CO2 from burning crop residue is biogenic and, as in
#' IPCC inventories, is reported but left out of the total unless
#' \code{include_burn_co2 = TRUE}. Versions before 1.1.0 added that CO2 to
#' the total and omitted burning CH4 and N2O.
#'
#' The tillage CO2 values (150, 100 and 50 kg CO2/ha for conventional,
#' reduced and zero tillage) are rough screening values. When diesel use
#' is known, \code{\link{carbon_footprint}} gives a better estimate.
#'
#' @param n_applied Numeric. N fertilizer applied (kg/ha).
#' @param residue_burned Logical. Whether crop residues are burned.
#'   Default \code{FALSE}.
#' @param residue_amount Numeric. Residue amount (kg dry matter/ha) if
#'   burned. Default 0.
#' @param paddy_days Numeric. Days under flooded paddy conditions.
#'   Default 0.
#' @param tillage Character. Tillage type: \code{"conventional"},
#'   \code{"reduced"}, or \code{"zero"}. Default \code{"conventional"}.
#' @param gwp_n2o Numeric. GWP for N2O. Default 273 (IPCC AR6).
#' @param gwp_ch4 Numeric. GWP for CH4. Default 27 (IPCC AR6).
#' @param ef_n2o Numeric. Direct N2O emission factor (kg N2O-N per kg N
#'   applied). Default 0.01, the IPCC aggregated Tier 1 value. The 2019
#'   IPCC Refinement gives 0.005 for dry climates (relevant to arid
#'   zones) and 0.016 for synthetic N in wet climates.
#' @param ef_ch4_paddy Numeric. Baseline CH4 emission factor for
#'   continuously flooded rice (kg CH4/ha/day). Default 1.3 (IPCC 2006);
#'   the 2019 Refinement gives 1.19.
#' @param combustion_factor Numeric. Fraction of residue actually burnt.
#'   Default 0.8 (IPCC 2006 Table 2.6 for maize and rice residues; 0.9
#'   for wheat).
#' @param ef_ch4_burn,ef_n2o_burn Numeric. Emission factors for burning
#'   agricultural residues (g per kg dry matter burnt). Defaults 2.7 and
#'   0.07 (IPCC 2006 Table 2.5).
#' @param include_burn_co2 Logical. Add biogenic CO2 from residue burning
#'   to the total. Default \code{FALSE}.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @return For a single observation, a data frame with columns
#'   \code{source}, \code{gas}, \code{emission_kg}, \code{CO2_eq_kg} and
#'   \code{counted} (whether the row enters the total); the total is
#'   stored in \code{attr(, "total_CO2eq")}. For several observations
#'   (any input of length > 1), a data frame with one row per observation
#'   and a \code{total_CO2eq} column.
#'
#' @examples
#' ghg_emission(n_applied = 120, tillage = "zero")
#' ghg_emission(n_applied = 150, paddy_days = 90)
#' ghg_emission(n_applied = 100, residue_burned = TRUE,
#'              residue_amount = 5000)
#' ghg_emission(n_applied = c(20, 40, 60), ef_n2o = 0.005)
#'
#' @references
#' IPCC (2006). \emph{2006 IPCC Guidelines for National Greenhouse Gas
#' Inventories}. Volume 4, Chapter 2: Generic methodologies applicable to
#' multiple land-use categories (Tables 2.5 and 2.6). IGES, Japan.
#'
#' IPCC (2019). \emph{2019 Refinement to the 2006 IPCC Guidelines for
#' National Greenhouse Gas Inventories}. Volume 4, Chapter 11:
#' N2O emissions from managed soils, and CO2 emissions from lime
#' and urea application. ISBN:978-4-88788-232-4.
#'
#' @export
ghg_emission <- function(n_applied, residue_burned = FALSE,
                         residue_amount = 0, paddy_days = 0,
                         tillage = "conventional",
                         gwp_n2o = 273, gwp_ch4 = 27,
                         ef_n2o = 0.01, ef_ch4_paddy = 1.3,
                         combustion_factor = 0.8,
                         ef_ch4_burn = 2.7, ef_n2o_burn = 0.07,
                         include_burn_co2 = FALSE,
                         verbose = getOption("wefnexus.verbose", TRUE)) {
  tillage <- match.arg(tillage, c("conventional", "reduced", "zero"))
  .validate_non_negative(n_applied, "n_applied")
  .validate_non_negative(residue_amount, "residue_amount")
  .validate_non_negative(paddy_days, "paddy_days")
  if (!is.numeric(combustion_factor) || any(combustion_factor < 0 |
                                            combustion_factor > 1)) {
    stop("'combustion_factor' must be between 0 and 1.", call. = FALSE)
  }
  n_obs <- max(length(n_applied), length(residue_amount),
               length(paddy_days), length(residue_burned))
  n_applied <- rep_len(n_applied, n_obs)
  residue_amount <- rep_len(residue_amount, n_obs)
  paddy_days <- rep_len(paddy_days, n_obs)
  burned <- rep_len(as.logical(residue_burned), n_obs)

  # N2O from soil: N x EF1 gives N2O-N; x 44/28 gives N2O
  n2o_kg <- n_applied * ef_n2o * (44 / 28)
  # CH4 from flooded rice
  ch4_paddy <- paddy_days * ef_ch4_paddy
  # Residue burning (IPCC 2006 Vol. 4 Ch. 2)
  mass_burnt <- ifelse(burned, residue_amount * combustion_factor, 0)
  ch4_burn <- mass_burnt * ef_ch4_burn / 1000
  n2o_burn <- mass_burnt * ef_n2o_burn / 1000
  co2_burn <- mass_burnt * 0.37 * (44 / 12)   # biogenic CO2, C fraction 0.37
  # CO2 from tillage operations (screening values)
  tillage_co2 <- switch(tillage, "conventional" = 150, "reduced" = 100,
                        "zero" = 50)

  total <- n2o_kg * gwp_n2o + ch4_paddy * gwp_ch4 + ch4_burn * gwp_ch4 +
    n2o_burn * gwp_n2o + tillage_co2 +
    if (include_burn_co2) co2_burn else 0

  if (n_obs > 1L) {
    result <- data.frame(
      n2o_emission_kg  = round(n2o_kg, 4),
      n2o_CO2eq        = round(n2o_kg * gwp_n2o, 2),
      ch4_CO2eq        = round(ch4_paddy * gwp_ch4, 2),
      burn_ch4_CO2eq   = round(ch4_burn * gwp_ch4, 2),
      burn_n2o_CO2eq   = round(n2o_burn * gwp_n2o, 2),
      residue_burn_CO2 = round(co2_burn, 2),
      tillage_CO2      = rep(tillage_co2, n_obs),
      total_CO2eq      = round(total, 2),
      stringsAsFactors = FALSE
    )
    if (verbose) message("GHG emissions estimated for ", n_obs, " observations.")
    return(result)
  }

  result <- data.frame(
    source = c("N2O_soil", "CH4_paddy", "CH4_residue_burn",
               "N2O_residue_burn", "CO2_residue_burn", "CO2_tillage"),
    gas = c("N2O", "CH4", "CH4", "N2O", "CO2", "CO2"),
    emission_kg = round(c(n2o_kg, ch4_paddy, ch4_burn, n2o_burn, co2_burn,
                          tillage_co2), 3),
    CO2_eq_kg = round(c(n2o_kg * gwp_n2o, ch4_paddy * gwp_ch4,
                        ch4_burn * gwp_ch4, n2o_burn * gwp_n2o, co2_burn,
                        tillage_co2), 2),
    counted = c(TRUE, TRUE, TRUE, TRUE, include_burn_co2, TRUE),
    stringsAsFactors = FALSE
  )
  attr(result, "total_CO2eq") <- round(total, 2)
  if (verbose) {
    message("GHG emissions: ", round(total, 2),
            " kg CO2-eq/ha (IPCC AR6 GWP", if (!include_burn_co2 && co2_burn > 0)
              "; biogenic CO2 from burning not counted", ").")
  }
  return(result)
}

#' Soil Organic Carbon Stock (SOC)
#'
#' Estimates soil organic carbon stock for a given soil depth.
#'
#' @param soc_pct Numeric vector. Soil organic carbon content (percent).
#' @param bulk_density Numeric vector. Soil bulk density (Mg/m3 or g/cm3).
#' @param depth Numeric. Soil sampling depth (cm). Default 30.
#' @param coarse_fraction Numeric vector. Volume fraction of coarse
#'   fragments greater than 2 mm (0 to 1). Default 0.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @return Numeric vector. SOC stock (Mg C/ha, equivalent to t C/ha).
#'
#' @details
#' \deqn{SOC = SOC_{pct} \times BD \times D \times (1 - CF)}{SOC = SOC_pct * BD * D * (1 - CF)}
#' where SOC_pct is organic carbon (percent), BD is bulk density
#' (Mg/m3), D is depth (cm) and CF is the coarse fragment fraction. The
#' product of percent, Mg/m3 and cm is already Mg C/ha:
#' (SOC/100) x BD x (D/100 m) x 10000 m2/ha.
#'
#' Versions before 1.1.0 multiplied by an extra factor of 0.1, which gave
#' kg C/m2 while labelling the result Mg C/ha, so stocks were 10 times
#' too low.
#'
#' @examples
#' soil_carbon_stock(soc_pct = 0.65, bulk_density = 1.45, depth = 30)
#'
#' @references
#' Batjes, N.H. (1996). Total carbon and nitrogen in the soils of
#' the world. \emph{European Journal of Soil Science}, 47(2), 151-163.
#' \doi{10.1111/j.1365-2389.1996.tb01386.x}
#'
#' @export
soil_carbon_stock <- function(soc_pct, bulk_density, depth = 30,
                               coarse_fraction = 0, verbose = getOption("wefnexus.verbose", TRUE)) {
  .validate_numeric(soc_pct, "soc_pct")
  .validate_positive(bulk_density, "bulk_density")
  .validate_positive(depth, "depth")
  .validate_non_negative(coarse_fraction, "coarse_fraction")
  if (any(coarse_fraction >= 1, na.rm = TRUE)) {
    stop("'coarse_fraction' must be below 1 (it is a volume fraction).",
         call. = FALSE)
  }
  stock <- soc_pct * bulk_density * depth * (1 - coarse_fraction)
  if (verbose) message("SOC stock computed (Mg C/ha).")
  return(round(stock, 2))
}

#' Carbon Sequestration Rate
#'
#' Annual rate of soil carbon change between two measurement time points.
#'
#'
#' @details Formula: \deqn{CSR = \frac{SOC_{final} - SOC_{initial}}{Years}}
#'
#' @param soc_initial Numeric vector. Initial SOC stock (Mg C/ha).
#' @param soc_final Numeric vector. Final SOC stock (Mg C/ha).
#' @param years Numeric. Number of years between measurements.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @return Numeric vector. Carbon sequestration rate (Mg C/ha/year).
#'   Positive values indicate net sequestration; negative values
#'   indicate net emission from soil.
#'
#' @examples
#' carbon_sequestration_rate(soc_initial = 28.5, soc_final = 31.2,
#'                           years = 5)
#'
#' @references
#' Minasny, B. et al. (2017). Soil carbon 4 per mille.
#' \emph{Geoderma}, 292, 59-86. \doi{10.1016/j.geoderma.2017.01.002}
#'
#' @export
carbon_sequestration_rate <- function(soc_initial, soc_final, years,
                                      verbose = getOption("wefnexus.verbose", TRUE)) {
  .validate_numeric(soc_initial, "soc_initial")
  .validate_numeric(soc_final, "soc_final")
  .validate_positive(years, "years")
  result <- (soc_final - soc_initial) / years
  if (verbose) message("C sequestration rate computed (Mg C/ha/yr).")
  return(round(result, 3))
}

#' Global Warming Potential (GWP)
#'
#' Converts individual greenhouse gas emissions to CO2 equivalents using
#' IPCC AR6 Global Warming Potential values.
#'
#'
#' @details Formula: \deqn{GWP = CO_2 + CH_4 \times 27 + N_2O \times 273}
#'
#' @param co2 Numeric. CO2 emission (kg/ha). Default 0.
#' @param ch4 Numeric. CH4 emission (kg/ha). Default 0.
#' @param n2o Numeric. N2O emission (kg/ha). Default 0.
#' @param gwp_ch4 Numeric. GWP of CH4. Default 27 (IPCC AR6, 100-year).
#' @param gwp_n2o Numeric. GWP of N2O. Default 273 (IPCC AR6, 100-year).
#' @param time_horizon Character. \code{"100yr"} (default) or
#'   \code{"20yr"} for different assessment periods.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @return Numeric. Total global warming potential (kg CO2-eq/ha).
#'
#' @details
#' IPCC AR6 100-year GWP values (with climate-carbon feedbacks):
#' CH4 = 27, N2O = 273.
#'
#' IPCC AR6 20-year GWP values for non-fossil (biogenic) methane, as
#' emitted by agriculture: CH4 = 79.7, N2O = 273. With
#' \code{time_horizon = "20yr"} these replace the defaults, unless you
#' supply \code{gwp_ch4} or \code{gwp_n2o} yourself. Versions before
#' 1.1.0 used 81 for CH4.
#'
#' Note: Previous IPCC AR5 100-year values were CH4 = 34, N2O = 298
#' (with climate-carbon feedbacks) or CH4 = 28, N2O = 265 (without).
#'
#' @examples
#' global_warming_potential(co2 = 500, ch4 = 50, n2o = 2)
#' global_warming_potential(co2 = 500, ch4 = 50, n2o = 2,
#'                          time_horizon = "20yr")
#'
#' @references
#' Forster, P. et al. (2021). The Earth's energy budget, climate
#' feedbacks, and climate sensitivity. In \emph{Climate Change 2021:
#' The Physical Science Basis} (IPCC AR6 WGI Chapter 7), Table 7.15.
#' \doi{10.1017/9781009157896.009}
#'
#' @export
global_warming_potential <- function(co2 = 0, ch4 = 0, n2o = 0,
                                     gwp_ch4 = 27, gwp_n2o = 273,
                                     time_horizon = "100yr",
                                     verbose = getOption("wefnexus.verbose", TRUE)) {
  time_horizon <- match.arg(time_horizon, c("100yr", "20yr"))
  if (time_horizon == "20yr") {
    if (missing(gwp_ch4)) gwp_ch4 <- 79.7   # AR6 GWP-20, non-fossil CH4
    if (missing(gwp_n2o)) gwp_n2o <- 273    # same as 100-year in AR6
  }
  total <- co2 + (ch4 * gwp_ch4) + (n2o * gwp_n2o)
  if (verbose) {
    message("GWP computed: ", round(total, 2), " kg CO2-eq/ha ",
            "(", time_horizon, " horizon, IPCC AR6).")
  }
  return(round(total, 2))
}
