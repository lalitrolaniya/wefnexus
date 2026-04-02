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
#'   (kg N2O/ha). If \code{NULL} (default), estimated using IPCC
#'   Tier 1 default: 1 percent of applied N emitted as N2O-N, then
#'   converted to N2O using factor 44/28.
#' @param ch4_emission Numeric. Methane emission (kg CH4/ha). Default 0.
#'   Relevant for rice paddies.
#'
#' @details Formula: \deqn{CF = \frac{\sum Activity_i \times EF_i}{Yield}}
#'
#' @param yield Numeric. Crop yield (kg/ha) for intensity calculation.
#'   Default \code{NULL}.
#' @param ef_diesel Numeric. Emission factor for diesel (kg CO2/L).
#'   Default 2.68.
#' @param ef_electricity Numeric. Emission factor for grid electricity
#'   (kg CO2/kWh). Default 0.82 (India grid average, CEA 2023).
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
#'   Default \code{TRUE}.
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
#' \doi{10.1016/j.envint.2004.01.005}
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
                             ef_diesel = 2.68, ef_electricity = 0.82,
                             ef_n_manufacture = 4.96,
                             ef_p_manufacture = 1.61,
                             ef_k_manufacture = 0.57,
                             ef_pesticide = 10.97, ef_seed = 0.58,
                             gwp_n2o = 273, gwp_ch4 = 27,
                             verbose = TRUE) {
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
    n2o_direct <- n_fertilizer * 0.01 * (44 / 28)
  }
  em_n2o <- n2o_direct * gwp_n2o

  # CH4 emissions
  em_ch4 <- ch4_emission * gwp_ch4

  # Total
  total_cf <- em_diesel + em_electricity + em_n_fert + em_p_fert +
    em_k_fert + em_pesticide + em_seed + em_n2o + em_ch4

  # Vectorised: return data.frame when inputs are vectors
  n_obs <- max(length(total_cf), 1L)
  if (n_obs > 1L) {
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
    share_pct = round(emissions / total_cf * 100, 1),
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
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. Carbon efficiency (kg yield per kg CO2-eq).
#'   Higher values indicate more yield per unit emission.
#'
#' @examples
#' carbon_efficiency(yield = 4500, carbon_emission = 2500)
#'
#' @export
carbon_efficiency <- function(yield, carbon_emission, verbose = TRUE) {
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
#' @details Formula: \deqn{CSI = \frac{Yield}{Carbon Footprint}}
#'
#' @param yield Numeric vector. Crop yield (kg/ha).
#' @param carbon_emission Numeric vector. Total GHG emission
#'   (kg CO2-eq/ha).
#' @param carbon_sequestered Numeric vector. Carbon sequestered in
#'   soil (kg CO2-eq/ha). Default 0.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
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
                                         verbose = TRUE) {
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
#'
#' @details Formula: \deqn{GHG = \sum (Activity_i \times EF_i)}{GHG = sum(Activity_i * EF_i)} where EF is the emission factor for each activity.
#'
#' @param n_applied Numeric. N fertilizer applied (kg/ha).
#' @param residue_burned Logical. Whether crop residues are burned.
#'   Default \code{FALSE}.
#' @param residue_amount Numeric. Residue amount (kg/ha) if burned.
#'   Default 0.
#' @param paddy_days Numeric. Days under flooded paddy conditions.
#'   Default 0.
#' @param tillage Character. Tillage type: \code{"conventional"},
#'   \code{"reduced"}, or \code{"zero"}. Default \code{"conventional"}.
#' @param gwp_n2o Numeric. GWP for N2O. Default 273 (IPCC AR6).
#' @param gwp_ch4 Numeric. GWP for CH4. Default 27 (IPCC AR6).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return A data frame with four columns:
#' \describe{
#'   \item{source}{Character. Emission source description}
#'   \item{gas}{Character. Greenhouse gas type}
#'   \item{emission_kg}{Numeric. Raw emission (kg/ha)}
#'   \item{CO2_eq_kg}{Numeric. Emission in CO2 equivalents (kg CO2-eq/ha)}
#' }
#'
#' @examples
#' ghg_emission(n_applied = 120, tillage = "zero")
#' ghg_emission(n_applied = 150, paddy_days = 90)
#'
#' @references
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
                         verbose = TRUE) {
  tillage <- match.arg(tillage, c("conventional", "reduced", "zero"))
  .validate_numeric(n_applied, "n_applied")

  # N2O from soil (IPCC Tier 1: EF1 = 0.01 kg N2O-N per kg N applied)
  n2o_n <- n_applied * 0.01
  n2o_kg <- n2o_n * (44 / 28)  # Convert N2O-N to N2O

  # CH4 from paddy (IPCC default: ~1.3 kg CH4/ha/day baseline)
  ch4_paddy <- paddy_days * 1.3

  # CO2 from residue burning (C fraction ~0.37, C to CO2 = 44/12)
  co2_burn <- if (residue_burned) residue_amount * 0.37 * (44 / 12) else 0

  # CO2 from tillage operations (approximate diesel-related CO2)
  tillage_co2 <- switch(tillage,
                        "conventional" = 150,
                        "reduced" = 100,
                        "zero" = 50)

  # Vectorised: return data.frame with per-observation rows
  n_obs <- length(n_applied)
  if (n_obs > 1L) {
    result <- data.frame(
      n2o_emission_kg   = round(n2o_kg, 4),
      n2o_CO2eq         = round(n2o_kg * gwp_n2o, 2),
      ch4_CO2eq         = round(ch4_paddy * gwp_ch4, 2),
      residue_burn_CO2  = round(co2_burn, 2),
      tillage_CO2       = tillage_co2,
      total_CO2eq       = round(n2o_kg * gwp_n2o + ch4_paddy * gwp_ch4 +
                                 co2_burn + tillage_co2, 2),
      stringsAsFactors  = FALSE
    )
    if (verbose) message("GHG emissions estimated for ", n_obs, " observations.")
    return(result)
  }

  # Scalar: return source-wise breakdown (original behaviour)
  result <- data.frame(
    source = c("N2O_soil", "CH4_paddy", "CO2_residue_burn", "CO2_tillage"),
    gas = c("N2O", "CH4", "CO2", "CO2"),
    emission_kg = round(c(n2o_kg, ch4_paddy, co2_burn, tillage_co2), 2),
    CO2_eq_kg = round(c(n2o_kg * gwp_n2o, ch4_paddy * gwp_ch4,
                         co2_burn, tillage_co2), 2),
    stringsAsFactors = FALSE
  )
  if (verbose) message("GHG emissions estimated (IPCC AR6 GWP).")
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
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. SOC stock (Mg C/ha, equivalent to t C/ha).
#'
#' @details
#' \deqn{SOC = SOC_{pct} \times BD \times D \times (1 - CF) \times 0.1}{SOC = SOC_pct * BD * D * (1-CF) * 0.1}
#' where BD is bulk density (Mg/m3), D is depth (cm), CF is coarse
#' fraction, and 0.1 is the conversion factor from (percent * Mg/m3 *
#' cm) to Mg/ha.
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
                               coarse_fraction = 0, verbose = TRUE) {
  .validate_numeric(soc_pct, "soc_pct")
  .validate_positive(bulk_density, "bulk_density")
  .validate_positive(depth, "depth")
  .validate_non_negative(coarse_fraction, "coarse_fraction")
  stock <- soc_pct * bulk_density * depth * (1 - coarse_fraction) * 0.1
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
#'   Default \code{TRUE}.
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
                                      verbose = TRUE) {
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
#'   Default \code{TRUE}.
#'
#' @return Numeric. Total global warming potential (kg CO2-eq/ha).
#'
#' @details
#' IPCC AR6 100-year GWP values (with climate-carbon feedbacks):
#' CH4 = 27, N2O = 273.
#'
#' IPCC AR6 20-year GWP values: CH4 = 81, N2O = 273.
#'
#' Note: Previous IPCC AR5 values were CH4 = 34, N2O = 298 (without
#' climate-carbon feedbacks) or CH4 = 28, N2O = 265 (without).
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
                                     verbose = TRUE) {
  time_horizon <- match.arg(time_horizon, c("100yr", "20yr"))
  if (time_horizon == "20yr") {
    gwp_ch4 <- 81
    gwp_n2o <- 273  # N2O 20yr GWP same as 100yr in AR6
  }
  total <- co2 + (ch4 * gwp_ch4) + (n2o * gwp_n2o)
  if (verbose) {
    message("GWP computed: ", round(total, 2), " kg CO2-eq/ha ",
            "(", time_horizon, " horizon, IPCC AR6).")
  }
  return(round(total, 2))
}
