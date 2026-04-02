#' Arid Pulse Nexus Dataset
#'
#' A sample dataset with six conservation agriculture treatments in
#' an arid pulse-based cropping system from western Rajasthan, India.
#' Includes water, energy, food, nutrient, and carbon data for WEFNC
#' nexus analysis.
#'
#' @format A data frame with 6 rows and 26 variables:
#' \describe{
#'   \item{treatment}{Character. Treatment combination name}
#'   \item{tillage}{Character. Tillage method: CT (conventional),
#'     ZT (zero), PB (permanent beds)}
#'   \item{irrigation}{Character. Irrigation method: Flood, Sprinkler,
#'     Drip, or SSDI (sub-surface drip)}
#'   \item{residue}{Logical. Whether crop residue was retained}
#'   \item{grain_yield}{Numeric. Grain yield (kg/ha)}
#'   \item{straw_yield}{Numeric. Straw yield (kg/ha)}
#'   \item{irrigation_applied}{Numeric. Irrigation water applied (mm)}
#'   \item{effective_rainfall}{Numeric. Effective rainfall (mm)}
#'   \item{total_water}{Numeric. Total water consumed (mm)}
#'   \item{crop_et}{Numeric. Crop evapotranspiration (mm)}
#'   \item{energy_input}{Numeric. Total energy input (MJ/ha)}
#'   \item{energy_output_grain}{Numeric. Energy output from grain (MJ/ha)}
#'   \item{energy_output_straw}{Numeric. Energy output from straw (MJ/ha)}
#'   \item{n_applied}{Numeric. Nitrogen applied (kg/ha)}
#'   \item{p_applied}{Numeric. Phosphorus applied (kg P2O5/ha)}
#'   \item{k_applied}{Numeric. Potassium applied (kg K2O/ha)}
#'   \item{n_uptake}{Numeric. Total plant nitrogen uptake (kg/ha)}
#'   \item{p_uptake}{Numeric. Total plant phosphorus uptake (kg/ha)}
#'   \item{grain_n_uptake}{Numeric. Grain nitrogen uptake (kg/ha)}
#'   \item{diesel_use}{Numeric. Diesel consumption (L/ha)}
#'   \item{electricity_kwh}{Numeric. Electricity consumption (kWh/ha)}
#'   \item{soc_pct}{Numeric. Soil organic carbon (percent)}
#'   \item{bulk_density}{Numeric. Soil bulk density (Mg/m3)}
#'   \item{ghg_emission}{Numeric. Total GHG emission (kg CO2-eq/ha)}
#'   \item{cost_cultivation}{Numeric. Cost of cultivation (INR/ha)}
#'   \item{gross_return}{Numeric. Gross return (INR/ha)}
#' }
#'
#' @source Simulated dataset based on typical experimental data from
#'   ICAR-Indian Institute of Pulses Research, Regional Centre, Bikaner,
#'   Rajasthan, India.
#'
#' @examples
#' data(arid_pulse_nexus)
#' str(arid_pulse_nexus)
"arid_pulse_nexus"
