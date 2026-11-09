# =============================================================================
# ENERGY BUDGET FROM PHYSICAL QUANTITIES
# Converts input/output quantities to MJ/ha using energy equivalents
# =============================================================================

#' Energy Budget from Physical Input Quantities
#'
#' Converts physical quantities of inputs (litres of diesel, kg of
#' fertilizer, labour hours, and so on) and outputs (grain and straw) to
#' energy using the equivalents in \code{\link{energy_coefficients}} or a
#' user-supplied table. Returns input energy by source, direct and
#' indirect energy, renewable and non-renewable energy, and, when outputs
#' are given, the main energy efficiency indicators.
#'
#' @param data A data frame with one row per treatment or plot, or a
#'   named numeric vector for a single treatment. Columns (or names)
#'   that match the \code{input} column of \code{coefficients} are
#'   converted; other columns are kept as identifiers (character or
#'   factor) or reported as unmatched (numeric).
#' @param coefficients A data frame with columns \code{input},
#'   \code{unit}, \code{mj_per_unit}, \code{type} (\code{"input"} or
#'   \code{"output"}), \code{form} (\code{"direct"} or \code{"indirect"})
#'   and \code{source} (\code{"renewable"} or \code{"non-renewable"}).
#'   Default \code{\link{energy_coefficients}}. Edit a copy to use
#'   values from your own reference.
#' @param yield_col Character. Name of the output column holding economic
#'   yield (kg/ha) for energy productivity and intensity. Default
#'   \code{"grain"}.
#' @param verbose Logical. If \code{TRUE}, prints informational messages.
#'   Default \code{getOption("wefnexus.verbose", TRUE)}, so
#'   \code{options(wefnexus.verbose = FALSE)} silences all functions.
#'
#' @details Energy of each item is quantity multiplied by its energy
#'   equivalent (MJ per unit). Shares are percentages of total input
#'   energy. When outputs are present the summary also includes energy
#'   output, net energy, energy use efficiency (output/input), energy
#'   productivity (kg/MJ) and energy intensity (MJ/kg).
#'
#' @return An object of class \code{energy_budget}: a list with
#' \describe{
#'   \item{summary}{Data frame, one row per treatment: identifier columns,
#'     \code{total_input_MJ}, \code{direct_MJ}, \code{indirect_MJ},
#'     \code{renewable_MJ}, \code{non_renewable_MJ}, their percentage
#'     shares, and (if outputs are present) \code{total_output_MJ},
#'     \code{net_energy_MJ}, \code{EUE}, \code{energy_productivity_kg_MJ}
#'     and \code{energy_intensity_MJ_kg}.}
#'   \item{detail}{Long data frame with the energy of every item per
#'     treatment and its share of total input energy.}
#'   \item{unmatched}{Character vector of numeric columns not found in
#'     \code{coefficients}.}
#' }
#'
#' @examples
#' inputs <- data.frame(
#'   treatment = c("CT", "ZT"),
#'   human_labour_man = c(320, 240),
#'   diesel = c(65, 38),
#'   electricity = c(180, 75),
#'   nitrogen = c(20, 20),
#'   phosphorus = c(40, 40),
#'   seed = c(20, 20),
#'   herbicide = c(0, 0.75),
#'   grain = c(980, 1280),
#'   straw = c(1450, 1850)
#' )
#' eb <- energy_budget(inputs)
#' eb
#' eb$summary
#'
#' # Use your own energy equivalent for diesel
#' my_coef <- energy_coefficients
#' my_coef$mj_per_unit[my_coef$input == "diesel"] <- 47.8
#' energy_budget(inputs, coefficients = my_coef, verbose = FALSE)$summary
#'
#' @seealso \code{\link{energy_coefficients}}, \code{\link{energy_input}},
#'   \code{\link{energy_use_efficiency}}
#' @export
energy_budget <- function(data, coefficients = wefnexus::energy_coefficients,
                          yield_col = "grain",
                          verbose = getOption("wefnexus.verbose", TRUE)) {
  if (is.numeric(data) && !is.null(names(data))) {
    data <- as.data.frame(as.list(data))
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame or a named numeric vector.",
         call. = FALSE)
  }
  need <- c("input", "unit", "mj_per_unit", "type", "form", "source")
  miss <- setdiff(need, names(coefficients))
  if (length(miss) > 0) {
    stop("'coefficients' is missing column(s): ",
         paste(miss, collapse = ", "), call. = FALSE)
  }
  coefficients <- as.data.frame(coefficients, stringsAsFactors = FALSE)
  bad_type <- setdiff(stats::na.omit(coefficients$type), c("input", "output"))
  bad_form <- setdiff(stats::na.omit(coefficients$form[coefficients$type == "input"]),
                      c("direct", "indirect"))
  bad_src <- setdiff(stats::na.omit(coefficients$source[coefficients$type == "input"]),
                     c("renewable", "non-renewable"))
  if (length(c(bad_type, bad_form, bad_src)) > 0) {
    stop("Unrecognised category in 'coefficients': ",
         paste(unique(c(bad_type, bad_form, bad_src)), collapse = ", "),
         ". Use type input/output, form direct/indirect and source ",
         "renewable/non-renewable.", call. = FALSE)
  }
  matched <- intersect(names(data), coefficients$input)
  if (length(matched) == 0) {
    stop("No columns of 'data' match 'coefficients$input'. See ",
         "energy_coefficients$input for recognised names.", call. = FALSE)
  }
  num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  unmatched <- setdiff(num_cols, matched)
  id_cols <- setdiff(names(data), num_cols)
  n <- nrow(data)
  row_id <- if (length(id_cols) > 0) {
    do.call(paste, c(data[id_cols], sep = " | "))
  } else {
    paste("Row", seq_len(n))
  }

  coef <- coefficients[match(matched, coefficients$input), ]
  for (cl in matched) .validate_non_negative(data[[cl]], cl)
  q <- as.matrix(data[, matched, drop = FALSE])
  if (anyNA(q)) {
    warning("Missing quantities treated as zero for: ",
            paste(matched[colSums(is.na(q)) > 0], collapse = ", "),
            ". Enter 0 explicitly where an input was not used.",
            call. = FALSE)
    q[is.na(q)] <- 0
  }
  mj <- sweep(q, 2, coef$mj_per_unit, "*")

  is_in <- coef$type == "input"
  is_out <- coef$type == "output"
  sum_if <- function(sel) {
    if (!any(sel)) return(rep(0, n))
    rowSums(mj[, sel, drop = FALSE])
  }
  total_in <- sum_if(is_in)
  direct <- sum_if(is_in & coef$form == "direct")
  indirect <- sum_if(is_in & coef$form == "indirect")
  renew <- sum_if(is_in & coef$source == "renewable")
  nonrenew <- sum_if(is_in & coef$source == "non-renewable")
  pct <- function(v) ifelse(total_in > 0, round(100 * v / total_in, 2),
                            NA_real_)

  summ <- data.frame(data[id_cols], stringsAsFactors = FALSE)
  if (length(id_cols) == 0) summ <- data.frame(row = row_id)
  summ$total_input_MJ <- round(total_in, 2)
  summ$direct_MJ <- round(direct, 2)
  summ$indirect_MJ <- round(indirect, 2)
  summ$renewable_MJ <- round(renew, 2)
  summ$non_renewable_MJ <- round(nonrenew, 2)
  summ$direct_pct <- pct(direct)
  summ$indirect_pct <- pct(indirect)
  summ$renewable_pct <- pct(renew)
  summ$non_renewable_pct <- pct(nonrenew)
  if (any(is_out)) {
    total_out <- sum_if(is_out)
    summ$total_output_MJ <- round(total_out, 2)
    summ$net_energy_MJ <- round(total_out - total_in, 2)
    summ$EUE <- ifelse(total_in > 0, round(total_out / total_in, 2),
                       NA_real_)
    if (yield_col %in% matched) {
      y <- data[[yield_col]]
      summ$energy_productivity_kg_MJ <- ifelse(total_in > 0,
                                               round(y / total_in, 4),
                                               NA_real_)
      summ$energy_intensity_MJ_kg <- ifelse(y > 0, round(total_in / y, 2),
                                            NA_real_)
    }
  }

  detail <- data.frame(
    row = rep(row_id, times = length(matched)),
    item = rep(matched, each = n),
    type = rep(coef$type, each = n),
    form = rep(coef$form, each = n),
    source = rep(coef$source, each = n),
    quantity = as.vector(q),
    unit = rep(coef$unit, each = n),
    mj_per_unit = rep(coef$mj_per_unit, each = n),
    energy_MJ = round(as.vector(mj), 2),
    stringsAsFactors = FALSE
  )
  detail$share_of_input_pct <- ifelse(
    detail$type == "input" & rep(total_in, times = length(matched)) > 0,
    round(100 * detail$energy_MJ / rep(total_in, times = length(matched)), 2),
    NA_real_)

  if (verbose) {
    message("Energy budget computed for ", n, " row(s) using ",
            length(matched), " item(s).")
    if (length(unmatched) > 0) {
      message("Not in coefficient table (ignored): ",
              paste(unmatched, collapse = ", "))
    }
  }
  structure(list(summary = summ, detail = detail, unmatched = unmatched),
            class = "energy_budget")
}

#' Print method for energy_budget
#'
#' @param x An object of class \code{energy_budget}.
#' @param ... Additional arguments passed to \code{print.data.frame}.
#'
#' @return Invisibly returns \code{x}. Called for the side effect of
#'   printing the energy budget summary.
#'
#' @export
print.energy_budget <- function(x, ...) {
  cat("Energy Budget (MJ/ha)\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  print.data.frame(x$summary, row.names = FALSE, ...)
  if (length(x$unmatched) > 0) {
    cat("\nIgnored (not in coefficient table):",
        paste(x$unmatched, collapse = ", "), "\n")
  }
  cat("\nItem-wise detail in $detail\n")
  invisible(x)
}

#' Energy Equivalents of Agricultural Inputs and Outputs
#'
#' A reference table of typical energy equivalents used in energy
#' budgeting of crop production, for use with
#' \code{\link{energy_budget}}.
#'
#' @format A data frame with 20 rows and 7 variables:
#' \describe{
#'   \item{input}{Character. Item name; use these as column names in the
#'     data passed to \code{\link{energy_budget}}.}
#'   \item{unit}{Character. Physical unit of the item.}
#'   \item{mj_per_unit}{Numeric. Energy equivalent (MJ per unit).}
#'   \item{type}{Character. \code{"input"} or \code{"output"}.}
#'   \item{form}{Character. \code{"direct"} or \code{"indirect"} energy.}
#'   \item{source}{Character. \code{"renewable"} or
#'     \code{"non-renewable"}.}
#'   \item{note}{Character. Short description.}
#' }
#'
#' @details Energy equivalents differ between sources, regions and
#'   years. These are commonly used values in Indian and international
#'   crop energy studies and are provided as a starting point. Check them
#'   against the reference you cite and edit a copy of the table where
#'   needed (see the examples in \code{\link{energy_budget}}).
#'
#' @source Values compiled from commonly cited energy budgeting
#'   literature, including Mittal, J.P. & Dhawan, K.C. (1988).
#'   \emph{Research Manual on Energy Requirements in Agricultural Sector}.
#'   ICAR, New Delhi; and Pimentel, D. (Ed.) (1980). \emph{Handbook of
#'   Energy Utilization in Agriculture}. CRC Press, Boca Raton.
#'
#' @examples
#' energy_coefficients
#' subset(energy_coefficients, type == "input" & form == "direct")
"energy_coefficients"
