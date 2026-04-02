# ===========================================================================
# ADDITIONAL TESTS for wefnexus v1.0.0
# Covers functions missing from test-all-modules.R
# ===========================================================================

# --- WATER MODULE (missing) ---

test_that("depleted_fraction: ET / inflow", {
  df <- depleted_fraction(380, 420, verbose = FALSE)
  expect_equal(df, round(380 / 420, 4))
  expect_error(depleted_fraction("a", 420, verbose = FALSE))
})

test_that("irrigation_efficiency: returns data.frame", {
  ie <- irrigation_efficiency(350, 220, verbose = FALSE)
  expect_s3_class(ie, "data.frame")
  expect_true("conveyance_eff" %in% names(ie))
  expect_equal(ie$conveyance_eff, round(350 / 220 * 100, 2))
})

test_that("irrigation_efficiency: optional components", {
  ie <- irrigation_efficiency(350, 220, water_stored = 300,
                               crop_et = 280, verbose = FALSE)
  expect_true(all(c("application_eff", "overall_eff",
                     "consumptive_eff") %in% names(ie)))
})

# --- ENERGY MODULE (missing) ---

test_that("energy_productivity: yield / energy_in", {
  ep <- energy_productivity(4500, 12000, verbose = FALSE)
  expect_equal(ep, round(4500 / 12000, 4))
  expect_error(energy_productivity(4500, 0, verbose = FALSE))
})

test_that("specific_energy: energy / duration", {
  se <- specific_energy(12000, 120, verbose = FALSE)
  expect_equal(se, 100)
  expect_error(specific_energy(12000, 0, verbose = FALSE))
})

test_that("energy_profitability: return / energy", {
  epr <- energy_profitability(247500, 12000, verbose = FALSE)
  expect_equal(epr, round(247500 / 12000, 2))
})

test_that("human_energy_profitability: out / human", {
  hep <- human_energy_profitability(135000, 3600, verbose = FALSE)
  expect_equal(hep, round(135000 / 3600, 2))
})

test_that("energy_intensity: inverse of productivity", {
  ei <- energy_intensity(12000, 4500, verbose = FALSE)
  ep <- energy_productivity(4500, 12000, verbose = FALSE)
  expect_equal(round(ei * ep, 2), 1)
})

# --- FOOD MODULE (missing) ---

test_that("food_productivity_index: yield / reference (capped at 1)", {
  fpi <- food_productivity_index(1800, reference_yield = 1500, verbose = FALSE)
  expect_equal(fpi, 1)  # pmin caps at 1.0
  fpi2 <- food_productivity_index(1200, reference_yield = 1500, verbose = FALSE)
  expect_equal(fpi2, round(1200 / 1500, 4))
})

test_that("crop_yield_index: yield / check", {
  cyi <- crop_yield_index(1350, 1780, verbose = FALSE)
  expect_equal(cyi, round(1350 / 1780, 4))
})

test_that("production_efficiency_index: yield / cost", {
  pei <- production_efficiency_index(1500, 22000, verbose = FALSE)
  expect_equal(pei, round(1500 / 22000, 4))
})

# --- NUTRIENT MODULE (missing) ---

test_that("internal_utilization_efficiency: yield / uptake", {
  iue <- internal_utilization_efficiency(4500, 100, verbose = FALSE)
  expect_equal(iue, 45)
})

test_that("nutrient_harvest_index: grain / total", {
  nhi <- nutrient_harvest_index(75, 100, verbose = FALSE)
  expect_equal(nhi, 0.75)
  expect_error(nutrient_harvest_index(75, 0, verbose = FALSE))
})

# --- CARBON MODULE (missing) ---

test_that("carbon_efficiency: yield / emission", {
  ce <- carbon_efficiency(4500, 2500, verbose = FALSE)
  expect_equal(ce, 1.8)
})

test_that("carbon_sustainability_index: with sequestration", {
  csi <- carbon_sustainability_index(4500, 2500, 500, verbose = FALSE)
  expect_equal(csi, round(4500 / 2000, 4))
  # Net negative => NA
  csi_na <- carbon_sustainability_index(4500, 500, 1000, verbose = FALSE)
  expect_true(is.na(csi_na))
})

test_that("ghg_emission: scalar returns source breakdown", {
  ghg <- ghg_emission(120, verbose = FALSE)
  expect_s3_class(ghg, "data.frame")
  expect_equal(nrow(ghg), 4)
  expect_true(all(c("source", "gas", "emission_kg", "CO2_eq_kg") %in%
                    names(ghg)))
})

test_that("ghg_emission: vector returns per-observation rows", {
  ghg <- ghg_emission(c(60, 50, 45), verbose = FALSE)
  expect_s3_class(ghg, "data.frame")
  expect_equal(nrow(ghg), 3)
  expect_true("total_CO2eq" %in% names(ghg))
})

test_that("ghg_emission: tillage affects CO2", {
  conv <- ghg_emission(120, tillage = "conventional", verbose = FALSE)
  zero <- ghg_emission(120, tillage = "zero", verbose = FALSE)
  expect_gt(conv$CO2_eq_kg[conv$source == "CO2_tillage"],
            zero$CO2_eq_kg[zero$source == "CO2_tillage"])
})

test_that("carbon_footprint: vector inputs return data.frame", {
  cf <- carbon_footprint(diesel_use = c(60, 45),
                          n_fertilizer = c(120, 100),
                          yield = c(4500, 3800), verbose = FALSE)
  expect_s3_class(cf, "data.frame")
  expect_equal(nrow(cf), 2)
  expect_true(all(c("total_cf", "cf_intensity") %in% names(cf)))
})

test_that("carbon_footprint: scalar returns list", {
  cf <- carbon_footprint(diesel_use = 60, n_fertilizer = 120,
                          yield = 4500, verbose = FALSE)
  expect_type(cf, "list")
  expect_true("breakdown" %in% names(cf))
})

# --- NEXUS MODULE (missing) ---

test_that("normalize_minmax: inverse option", {
  nm <- normalize_minmax(c(1, 2, 3, 4, 5), inverse = TRUE)
  expect_equal(nm[1], 1)  # smallest value gets highest score
  expect_equal(nm[5], 0)
})

test_that("normalize_zscore: mean=0, sd=1", {
  zs <- normalize_zscore(c(10, 20, 30, 40, 50))
  expect_equal(round(mean(zs), 4), 0)
  expect_equal(round(sd(zs), 2), 1)
})

test_that("nexus_heatmap: runs without error", {
  mat <- matrix(runif(15), nrow = 3, ncol = 5)
  expect_invisible(
    nexus_heatmap(mat, row_labels = c("T1","T2","T3"),
                  col_labels = c("W","E","F","N","C"))
  )
})

test_that("nexus_radar: runs without error", {
  mat <- matrix(runif(15), nrow = 3, ncol = 5)
  expect_invisible(
    nexus_radar(mat, treatment_names = c("T1","T2","T3"))
  )
})

# --- NEW FUNCTIONS ---

test_that("nexus_sensitivity: returns correct structure", {
  sa <- nexus_sensitivity(
    water_score = c(0.9, 0.5), energy_score = c(0.6, 0.8),
    food_score = c(0.8, 0.7), nutrient_score = c(0.7, 0.6),
    carbon_score = c(0.5, 0.9),
    treatment_names = c("A", "B"), steps = 5, verbose = FALSE
  )
  expect_s3_class(sa, "data.frame")
  expect_true(all(c("dimension", "weight", "treatment", "nexus_index")
                    %in% names(sa)))
  # 5 dims x 6 steps x 2 treatments = 60

  expect_equal(nrow(sa), 5 * 6 * 2)
})

test_that("nexus_sensitivity: equal weights give same index", {
  sa <- nexus_sensitivity(
    water_score = c(0.5), energy_score = c(0.5),
    food_score = c(0.5), nutrient_score = c(0.5),
    carbon_score = c(0.5), steps = 5, verbose = FALSE
  )
  # When all scores are equal, index should always be 0.5
  expect_true(all(sa$nexus_index == 0.5))
})

test_that("S3 print.nexus_result works", {
  ns <- nexus_summary(
    yield = c(4500, 3800), water_consumed = c(450, 350),
    energy_input = c(12000, 9500), energy_output = c(135000, 112000),
    n_applied = c(120, 120), n_uptake = c(100, 80),
    carbon_emission = c(2500, 1800), verbose = FALSE
  )
  expect_s3_class(ns, "nexus_result")
  expect_output(print(ns), "WEFNC Nexus Analysis")
})

test_that("S3 print.cf_result works", {
  cf <- carbon_footprint(diesel_use = 60, n_fertilizer = 120,
                          yield = 4500, verbose = FALSE)
  expect_s3_class(cf, "cf_result")
  expect_output(print(cf), "Carbon Footprint")
})

test_that("S3 print.sustainability_result works", {
  nss <- nexus_sustainability_score(0.8, 0.7, 0.9, 0.6, 0.8,
                                    verbose = FALSE)
  expect_s3_class(nss, "sustainability_result")
  expect_output(print(nss), "Sustainability")
})
