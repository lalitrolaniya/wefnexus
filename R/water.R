# =============================================================================
# WATER MODULE
# Water Use Efficiency, Productivity, Footprint & Stress Indices
# =============================================================================

#' Water Use Efficiency (WUE)
#'
#' Computes water use efficiency as the ratio of economic yield to
#' total water consumed (evapotranspiration or irrigation plus rainfall).
#'
#' @param yield Numeric vector. Crop yield (kg/ha).
#' @param water_consumed Numeric vector. Total water consumed (mm), typically
#'   evapotranspiration (ET), or irrigation applied plus effective rainfall.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. Water use efficiency in kg/ha/mm. Higher values
#'   indicate more efficient use of water for crop production.
#'
#' @details
#' Water use efficiency is defined as:
#' \deqn{WUE = \frac{Y}{W}}{WUE = Y / W}
#' where Y is economic yield (kg/ha) and W is total water consumed (mm).
#'
#' @examples
#' # Wheat yield with different irrigation levels
#' yield <- c(4500, 4200, 3800, 3500)
#' water <- c(450, 400, 350, 300)
#' water_use_efficiency(yield, water)
#'
#' @references
#' Hoover, D.L. et al. (2023). Indicators of water use efficiency across
#' diverse agroecosystems and scales. \emph{Science of the Total Environment},
#' 864, 160992. \doi{10.1016/j.scitotenv.2022.160992}
#'
#' @export
water_use_efficiency <- function(yield, water_consumed, verbose = TRUE) {
  .validate_numeric(yield, "yield")
  .validate_positive(water_consumed, "water_consumed")
  .validate_same_length(yield, water_consumed)
  result <- yield / water_consumed
  if (verbose) message("WUE computed: ", length(result), " values (kg/ha/mm).")
  return(round(result, 4))
}

#' Water Productivity (WP)
#'
#' Computes physical or economic water productivity per unit of water used.
#'
#' @param yield Numeric vector. Crop yield (kg/ha).
#' @param water_applied Numeric vector. Total water applied (mm or m3/ha).
#' @param price Numeric. Market price per kg of produce (currency/kg).
#'   If \code{NULL} (default), returns physical water productivity.
#' @param unit Character. Unit of water input: \code{"mm"} (default) or
#'   \code{"m3"}.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. Physical water productivity in kg/m3 when
#'   \code{price} is \code{NULL}, or economic water productivity in
#'   currency/m3 when \code{price} is specified.
#'
#' @details
#' Physical water productivity:
#' \deqn{WP = \frac{Y}{W_{m3}}}{WP = Y / W_m3}
#'
#' Economic water productivity:
#' \deqn{WP_{econ} = \frac{Y \times P}{W_{m3}}}{WP_econ = Y * P / W_m3}
#'
#' When \code{unit = "mm"}, water is converted to m3/ha using 1 mm = 10 m3/ha.
#'
#' @examples
#' water_productivity(4500, 500, unit = "mm")
#' water_productivity(4500, 500, price = 25, unit = "mm")
#'
#' @references
#' Molden, D. et al. (2010). Improving agricultural water productivity:
#' Between optimism and caution. \emph{Agricultural Water Management},
#' 97(4), 528-535. \doi{10.1016/j.agwat.2009.03.023}
#'
#' @export
water_productivity <- function(yield, water_applied, price = NULL,
                               unit = "mm", verbose = TRUE) {
  .validate_numeric(yield, "yield")
  .validate_positive(water_applied, "water_applied")
  .validate_same_length(yield, water_applied)
  unit <- match.arg(unit, c("mm", "m3"))
  water_m3 <- if (unit == "mm") water_applied * 10 else water_applied
  if (is.null(price)) {
    result <- yield / water_m3
    if (verbose) message("Physical WP computed (kg/m3).")
  } else {
    result <- (yield * price) / water_m3
    if (verbose) message("Economic WP computed (currency/m3).")
  }
  return(round(result, 4))
}

#' Water Footprint (WF)
#'
#' Computes the water footprint of crop production decomposed into green,
#' blue, and grey components following the Water Footprint Assessment
#' framework.
#'
#' @param green_water Numeric vector. Green water use, i.e., effective
#'   rainfall consumed by the crop during growth (mm).
#' @param blue_water Numeric vector. Blue water use, i.e., irrigation
#'   water applied and consumed (mm).
#' @param grey_water Numeric vector. Grey water, i.e., freshwater volume
#'   required to dilute pollutant load to acceptable standards (mm).
#'   Default is 0.
#' @param yield Numeric vector. Crop yield (kg/ha).
#' @param area Numeric. Crop area in hectares. Default is 1.
#' @param per_ton Logical. If \code{TRUE}, returns water footprint in
#'   m3/ton. Default \code{FALSE}, returning m3/kg.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return A data frame with four numeric columns:
#' \describe{
#'   \item{green_wf}{Green water footprint (m3/kg or m3/ton)}
#'   \item{blue_wf}{Blue water footprint (m3/kg or m3/ton)}
#'   \item{grey_wf}{Grey water footprint (m3/kg or m3/ton)}
#'   \item{total_wf}{Total water footprint (m3/kg or m3/ton)}
#' }
#'
#' @details
#' The water footprint per unit mass of product is:
#' \deqn{WF = \frac{CWU \times 10}{Y}}{WF = (CWU * 10) / Y}
#' where CWU is crop water use (mm) and Y is yield (kg/ha). The factor 10
#' converts mm over 1 ha to m3.
#'
#' @examples
#' water_footprint(green_water = 300, blue_water = 200,
#'                 grey_water = 50, yield = 4000)
#'
#' @references
#' Hoekstra, A.Y., Chapagain, A.K., Aldaya, M.M. & Mekonnen, M.M. (2011).
#' \emph{The Water Footprint Assessment Manual: Setting the Global Standard}.
#' Earthscan, London. ISBN:9781849712798.
#'
#' Mialyk, O. et al. (2024). Water footprints and crop water use of 175
#' individual crops for 1990-2019 simulated with a global crop model.
#' \emph{Scientific Data}, 11, 200. \doi{10.1038/s41597-024-03051-3}
#'
#' @export
water_footprint <- function(green_water, blue_water, grey_water = 0,
                            yield, area = 1, per_ton = FALSE,
                            verbose = TRUE) {
  .validate_numeric(green_water, "green_water")
  .validate_numeric(blue_water, "blue_water")
  .validate_numeric(grey_water, "grey_water")
  .validate_positive(yield, "yield")
  factor <- 10 * area
  divisor <- if (per_ton) yield / 1000 else yield
  green_wf <- (green_water * factor) / divisor
  blue_wf  <- (blue_water * factor) / divisor
  grey_wf  <- (grey_water * factor) / divisor
  total_wf <- green_wf + blue_wf + grey_wf
  if (verbose) {
    unit <- if (per_ton) "m3/ton" else "m3/kg"
    message("Water footprint computed (", unit, ").")
  }
  data.frame(
    green_wf = round(green_wf, 2),
    blue_wf  = round(blue_wf, 2),
    grey_wf  = round(grey_wf, 2),
    total_wf = round(green_wf, 2) + round(blue_wf, 2) + round(grey_wf, 2)
  )
}

#' Irrigation Efficiency
#'
#' Computes multiple irrigation efficiency metrics: conveyance, application,
#' overall, and consumptive use efficiency.
#'
#'
#' @details Formula: \deqn{IE = \frac{Water_{crop}}{Water_{applied}} \times 100}
#'
#' @param water_delivered Numeric vector. Water delivered to field (mm or m3).
#' @param water_diverted Numeric vector. Water diverted from source (mm or m3).
#' @param water_stored Numeric vector. Water stored in root zone (mm or m3).
#'   If \code{NULL}, application and overall efficiency are not computed.
#' @param crop_et Numeric vector. Crop evapotranspiration (mm).
#'   If \code{NULL}, consumptive use efficiency is not computed.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return A data frame with efficiency metrics expressed as percentages.
#'   Columns depend on inputs provided:
#' \describe{
#'   \item{conveyance_eff}{Conveyance efficiency (percent), always computed}
#'   \item{application_eff}{Application efficiency (percent), if
#'     \code{water_stored} is provided}
#'   \item{overall_eff}{Overall efficiency (percent), if \code{water_stored}
#'     is provided}
#'   \item{consumptive_eff}{Consumptive use efficiency (percent), if
#'     \code{crop_et} is provided}
#' }
#'
#' @examples
#' irrigation_efficiency(water_delivered = 400, water_diverted = 500,
#'                       water_stored = 350, crop_et = 320)
#'
#' @references
#' Grafton, R.Q. et al. (2018). The paradox of irrigation efficiency.
#' \emph{Science}, 361(6404), 748-750. \doi{10.1126/science.aat9314}
#'
#' @export
irrigation_efficiency <- function(water_delivered, water_diverted,
                                  water_stored = NULL, crop_et = NULL,
                                  verbose = TRUE) {
  .validate_positive(water_delivered, "water_delivered")
  .validate_positive(water_diverted, "water_diverted")
  result <- data.frame(
    conveyance_eff = round((water_delivered / water_diverted) * 100, 2)
  )
  if (!is.null(water_stored)) {
    .validate_numeric(water_stored, "water_stored")
    result$application_eff <- round((water_stored / water_delivered) * 100, 2)
    result$overall_eff <- round((water_stored / water_diverted) * 100, 2)
  }
  if (!is.null(crop_et)) {
    .validate_numeric(crop_et, "crop_et")
    result$consumptive_eff <- round((crop_et / water_delivered) * 100, 2)
  }
  if (verbose) message("Irrigation efficiency computed.")
  return(result)
}

#' Crop Water Stress Index (CWSI)
#'
#' Computes the Crop Water Stress Index based on the ratio of actual
#' to potential evapotranspiration.
#'
#' @param actual_et Numeric vector. Actual evapotranspiration (mm).
#' @param potential_et Numeric vector. Potential or reference
#'   evapotranspiration (mm).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. CWSI values between 0 (no stress) and 1
#'   (maximum stress). Values close to 0 indicate well-watered conditions;
#'   values close to 1 indicate severe water deficit.
#'
#' @details
#' \deqn{CWSI = 1 - \frac{ET_a}{ET_p}}{CWSI = 1 - (ETa / ETp)}
#'
#' @examples
#' crop_water_stress_index(c(4.5, 3.8, 3.0, 2.2), c(5.0, 5.0, 5.0, 5.0))
#'
#' @references
#' Idso, S.B. et al. (1981). Normalizing the stress-degree-day parameter
#' for environmental variability. \emph{Agricultural Meteorology}, 24, 45-55.
#' \doi{10.1016/0002-1571(81)90032-7}
#'
#' @export
crop_water_stress_index <- function(actual_et, potential_et,
                                    verbose = TRUE) {
  .validate_numeric(actual_et, "actual_et")
  .validate_positive(potential_et, "potential_et")
  .validate_same_length(actual_et, potential_et)
  cwsi <- 1 - (actual_et / potential_et)
  cwsi <- pmax(0, pmin(1, cwsi))
  if (verbose) message("CWSI computed: ", length(cwsi), " values.")
  return(round(cwsi, 4))
}

#' Crop Water Productivity (CWP)
#'
#' Computes crop water productivity as the ratio of yield to total
#' evapotranspiration, analogous to the "more crop per drop" concept.
#'
#' @param yield Numeric vector. Crop yield (kg/ha).
#' @param et Numeric vector. Seasonal evapotranspiration (mm).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. Crop water productivity (kg/m3).
#'
#' @details
#' \deqn{CWP = \frac{Y}{ET \times 10}}{CWP = Y / (ET * 10)}
#' The factor 10 converts mm to m3/ha.
#'
#' @examples
#' crop_water_productivity(yield = 4500, et = 400)
#'
#' @references
#' Zwart, S.J. & Bastiaanssen, W.G.M. (2004). Review of measured crop
#' water productivity values for irrigated wheat, rice, cotton and maize.
#' \emph{Agricultural Water Management}, 69(2), 115-133.
#' \doi{10.1016/j.agwat.2004.04.007}
#'
#' @export
crop_water_productivity <- function(yield, et, verbose = TRUE) {
  .validate_numeric(yield, "yield")
  .validate_positive(et, "et")
  .validate_same_length(yield, et)
  cwp <- yield / (et * 10)
  if (verbose) message("Crop water productivity computed (kg/m3).")
  return(round(cwp, 4))
}

#' Depleted Fraction (DF)
#'
#' Ratio of water beneficially consumed (crop ET) to total water inflow,
#' used to assess basin-level water allocation.
#'
#'
#' @details Formula: \deqn{DF = \frac{ET_{actual}}{Water_{supplied}}}
#'
#' @param crop_et Numeric vector. Beneficial crop evapotranspiration (mm).
#' @param total_inflow Numeric vector. Total water inflow: irrigation plus
#'   effective rainfall (mm).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. Depleted fraction (proportion, 0 to 1).
#'
#' @examples
#' depleted_fraction(crop_et = 320, total_inflow = 500)
#'
#' @references
#' Perry, C., Steduto, P., Allen, R.G. & Burt, C.M. (2009). Increasing
#' productivity in irrigated agriculture: Agronomic constraints and
#' hydrological realities. \emph{Agricultural Water Management}, 96(11),
#' 1517-1524. \doi{10.1016/j.agwat.2009.05.005}
#'
#' @export
depleted_fraction <- function(crop_et, total_inflow, verbose = TRUE) {
  .validate_numeric(crop_et, "crop_et")
  .validate_positive(total_inflow, "total_inflow")
  .validate_same_length(crop_et, total_inflow)
  df <- crop_et / total_inflow
  if (verbose) message("Depleted fraction computed.")
  return(round(df, 4))
}
