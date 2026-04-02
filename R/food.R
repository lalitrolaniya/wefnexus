# =============================================================================
# FOOD MODULE
# Food Productivity, Yield Indices & Production Efficiency
# =============================================================================

#' Food Productivity Index (FPI)
#'
#' Computes a composite food productivity index considering yield
#' relative to potential, and optionally nutritional quality.
#'
#'
#' @details Formula: \deqn{FPI = \frac{Yield}{Reference Yield} \times 100}{FPI = (Yield / Reference Yield) * 100}
#'
#' @param yield Numeric vector. Crop yield (kg/ha).
#' @param reference_yield Numeric. Reference or potential yield (kg/ha).
#' @param protein_content Numeric vector. Protein content (percent).
#'   Default \code{NULL}.
#' @param caloric_value Numeric vector. Caloric value (kcal/100g).
#'   Default \code{NULL}.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. Food productivity index on a 0 to 1 scale.
#'   When protein and caloric values are provided, weights are 0.50 for
#'   yield ratio, 0.25 for protein, and 0.25 for caloric content.
#'
#' @examples
#' food_productivity_index(yield = 4500, reference_yield = 6000,
#'                         protein_content = 12.5, caloric_value = 340)
#'
#' @export
food_productivity_index <- function(yield, reference_yield,
                                    protein_content = NULL,
                                    caloric_value = NULL,
                                    verbose = TRUE) {
  .validate_numeric(yield, "yield")
  .validate_positive(reference_yield, "reference_yield")
  yield_ratio <- pmin(yield / reference_yield, 1)
  if (!is.null(protein_content) && !is.null(caloric_value)) {
    prot_norm <- pmin(protein_content / 40, 1)
    cal_norm <- pmin(caloric_value / 400, 1)
    fpi <- (0.50 * yield_ratio) + (0.25 * prot_norm) + (0.25 * cal_norm)
  } else {
    fpi <- yield_ratio
  }
  if (verbose) message("FPI computed: ", length(fpi), " values.")
  return(round(fpi, 4))
}

#' Crop Yield Index (CYI)
#'
#' Relative yield performance compared to a check or control treatment.
#'
#'
#' @details Formula: \deqn{CYI = \frac{Yield}{Maximum Yield} \times 100}{CYI = (Yield / Maximum Yield) * 100}
#'
#' @param yield Numeric vector. Treatment yields (kg/ha).
#' @param check_yield Numeric. Control or check yield (kg/ha).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. Yield index where 1.0 equals the check yield.
#'   Values above 1 indicate superiority over check.
#'
#' @examples
#' crop_yield_index(yield = c(4500, 4200, 3800), check_yield = 4000)
#'
#' @export
crop_yield_index <- function(yield, check_yield, verbose = TRUE) {
  .validate_numeric(yield, "yield")
  .validate_positive(check_yield, "check_yield")
  result <- yield / check_yield
  if (verbose) message("Crop yield index computed.")
  return(round(result, 4))
}

#' Harvest Index (HI)
#'
#' Proportion of economic yield to total above-ground biological yield.
#'
#' @param economic_yield Numeric vector. Grain or economic yield (kg/ha).
#' @param biological_yield Numeric vector. Total above-ground biomass
#'   yield (kg/ha), which is grain plus straw/stover.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. Harvest index as a proportion (0 to 1).
#'   A warning is issued if any value exceeds 1.
#'
#' @details
#' \deqn{HI = \frac{Y_{econ}}{Y_{biol}}}{HI = Y_econ / Y_biol}
#'
#' @examples
#' harvest_index(economic_yield = 4500, biological_yield = 10000)
#'
#' @references
#' Hay, R.K.M. (1995). Harvest index: A review of its use in plant
#' breeding and crop physiology. \emph{Annals of Applied Biology},
#' 126(1), 197-216. \doi{10.1111/j.1744-7348.1995.tb05015.x}
#'
#' @export
harvest_index <- function(economic_yield, biological_yield,
                          verbose = TRUE) {
  .validate_numeric(economic_yield, "economic_yield")
  .validate_positive(biological_yield, "biological_yield")
  .validate_same_length(economic_yield, biological_yield)
  hi <- economic_yield / biological_yield
  if (any(hi > 1, na.rm = TRUE)) {
    warning("Harvest index > 1 detected. Check yield values.")
  }
  if (verbose) message("Harvest index computed.")
  return(round(hi, 4))
}

#' Land Equivalent Ratio (LER)
#'
#' Evaluates the productivity advantage of intercropping systems
#' over sole cropping.
#'
#' @param yield_inter_a Numeric vector. Yield of crop A in intercrop
#'   (kg/ha).
#' @param yield_sole_a Numeric vector. Yield of crop A in sole crop
#'   (kg/ha).
#' @param yield_inter_b Numeric vector. Yield of crop B in intercrop
#'   (kg/ha).
#' @param yield_sole_b Numeric vector. Yield of crop B in sole crop
#'   (kg/ha).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. LER values. Values greater than 1 indicate
#'   an intercropping advantage; less than 1 indicates a disadvantage.
#'
#' @details
#' \deqn{LER = \frac{Y_{iA}}{Y_{sA}} + \frac{Y_{iB}}{Y_{sB}}}{LER = Y_iA/Y_sA + Y_iB/Y_sB}
#'
#' @examples
#' land_equivalent_ratio(yield_inter_a = 3500, yield_sole_a = 4500,
#'                       yield_inter_b = 800, yield_sole_b = 1200)
#'
#' @references
#' Mead, R. & Willey, R.W. (1980). The concept of a land equivalent
#' ratio and advantages in yields from intercropping. \emph{Experimental
#' Agriculture}, 16(3), 217-228. \doi{10.1017/S0014479700010978}
#'
#' @export
land_equivalent_ratio <- function(yield_inter_a, yield_sole_a,
                                   yield_inter_b, yield_sole_b,
                                   verbose = TRUE) {
  .validate_numeric(yield_inter_a, "yield_inter_a")
  .validate_positive(yield_sole_a, "yield_sole_a")
  .validate_numeric(yield_inter_b, "yield_inter_b")
  .validate_positive(yield_sole_b, "yield_sole_b")
  ler <- (yield_inter_a / yield_sole_a) + (yield_inter_b / yield_sole_b)
  if (verbose) message("LER computed.")
  return(round(ler, 4))
}

#' System Productivity Index (SPI)
#'
#' Converts yields from a cropping system to a common crop-equivalent
#' yield based on economic value.
#'
#' @param yields Numeric vector. Yields of individual crops in the
#'   system (kg/ha).
#' @param prices Numeric vector. Market prices of individual crops
#'   (currency/kg). Must be the same length as \code{yields}.
#' @param base_price Numeric. Price of the base or reference crop
#'   (currency/kg).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric. System productivity expressed as base crop equivalent
#'   yield (kg/ha).
#'
#' @details
#' \deqn{SPI = \frac{\sum(Y_i \times P_i)}{P_{base}}}{SPI = sum(Yi * Pi) / P_base}
#'
#' @examples
#' system_productivity_index(yields = c(5000, 4500),
#'                           prices = c(22, 25), base_price = 25)
#'
#' @export
system_productivity_index <- function(yields, prices, base_price,
                                      verbose = TRUE) {
  if (length(yields) != length(prices)) {
    stop("'yields' and 'prices' must have the same length.", call. = FALSE)
  }
  .validate_positive(base_price, "base_price")
  spi <- sum(yields * prices) / base_price
  if (verbose) message("SPI computed: ", round(spi, 2), " kg/ha base equivalent.")
  return(round(spi, 2))
}

#' Caloric Yield
#'
#' Total caloric output per hectare from crop production.
#'
#'
#' @details Formula: \deqn{CY = Yield \times Caloric Value}{CY = Yield * Caloric Value (kcal/kg)}
#'
#' @param yield Numeric vector. Crop yield (kg/ha).
#' @param caloric_value Numeric vector. Caloric content (kcal/kg).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. Caloric yield (kcal/ha).
#'
#' @examples
#' caloric_yield(yield = 4500, caloric_value = 3400)
#'
#' @export
caloric_yield <- function(yield, caloric_value, verbose = TRUE) {
  .validate_numeric(yield, "yield")
  .validate_numeric(caloric_value, "caloric_value")
  .validate_same_length(yield, caloric_value)
  result <- yield * caloric_value
  if (verbose) message("Caloric yield computed (kcal/ha).")
  return(result)
}

#' Protein Yield
#'
#' Total protein output per hectare from crop production.
#'
#'
#' @details Formula: \deqn{PY = Yield \times Protein Content / 100}
#'
#' @param yield Numeric vector. Crop yield (kg/ha).
#' @param protein_content Numeric vector. Protein content (percent).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. Protein yield (kg/ha).
#'
#' @examples
#' protein_yield(yield = 4500, protein_content = 12.5)
#'
#' @export
protein_yield <- function(yield, protein_content, verbose = TRUE) {
  .validate_numeric(yield, "yield")
  .validate_numeric(protein_content, "protein_content")
  .validate_same_length(yield, protein_content)
  result <- yield * protein_content / 100
  if (verbose) message("Protein yield computed (kg/ha).")
  return(round(result, 2))
}

#' Production Efficiency Index (PEI)
#'
#' Yield produced per unit of production cost, measuring economic
#' efficiency of crop production.
#'
#'
#' @details Formula: \deqn{PEI = \frac{Yield}{Total Input Cost} \times 100}{PEI = (Yield / Total Input Cost) * 100}
#'
#' @param yield Numeric vector. Crop yield (kg/ha).
#' @param cost Numeric vector. Total cost of production (currency/ha).
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{TRUE}.
#'
#' @return Numeric vector. Production efficiency index (kg/currency unit).
#'
#' @examples
#' production_efficiency_index(yield = 4500, cost = 28500)
#'
#' @export
production_efficiency_index <- function(yield, cost, verbose = TRUE) {
  .validate_numeric(yield, "yield")
  .validate_positive(cost, "cost")
  .validate_same_length(yield, cost)
  result <- yield / cost
  if (verbose) message("Production efficiency index computed.")
  return(round(result, 4))
}
