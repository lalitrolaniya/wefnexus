# ===========================================================================
# COMPREHENSIVE TESTS for wefnexus package
# Verifies every formula and edge case
# ===========================================================================

# --- WATER MODULE ---

test_that("water_use_efficiency: WUE = yield / water", {
  expect_equal(water_use_efficiency(4500, 450, verbose = FALSE), 10)
  expect_equal(water_use_efficiency(c(4500, 3000), c(450, 300),
               verbose = FALSE), c(10, 10))
  expect_error(water_use_efficiency(4500, 0, verbose = FALSE))
  expect_error(water_use_efficiency(4500, -10, verbose = FALSE))
  expect_error(water_use_efficiency("a", 450, verbose = FALSE))
})

test_that("water_productivity: mm-to-m3 conversion", {
  # 500 mm = 5000 m3/ha; 4500/5000 = 0.9 kg/m3
  wp <- water_productivity(4500, 500, unit = "mm", verbose = FALSE)
  expect_equal(wp, 0.9)
  # Economic: 4500*25/5000 = 22.5
  wp_econ <- water_productivity(4500, 500, price = 25, unit = "mm",
                                verbose = FALSE)
  expect_equal(wp_econ, 22.5)
})

test_that("water_footprint: green+blue+grey = total", {
  wf <- water_footprint(300, 200, 50, 4000, verbose = FALSE)
  expect_s3_class(wf, "data.frame")
  expect_equal(ncol(wf), 4)
  expect_equal(wf$total_wf, wf$green_wf + wf$blue_wf + wf$grey_wf)
  expect_error(water_footprint(300, 200, 50, 0, verbose = FALSE))
})

test_that("crop_water_stress_index: bounded 0-1", {
  cwsi <- crop_water_stress_index(c(3, 5, 0), c(5, 5, 5), verbose = FALSE)
  expect_true(all(cwsi >= 0 & cwsi <= 1))
  expect_equal(cwsi[2], 0)  # 1 - 5/5 = 0
  expect_equal(cwsi[3], 1)  # 1 - 0/5 = 1
})

test_that("crop_water_productivity: conversion correct", {
  # 4500 / (400 * 10) = 1.125
  cwp <- crop_water_productivity(4500, 400, verbose = FALSE)
  expect_equal(cwp, 1.125)
})

# --- ENERGY MODULE ---

test_that("energy_input: sum of components", {
  total <- energy_input(seed = 250, fertilizer_n = 3600, diesel = 2800,
                        verbose = FALSE)
  expect_equal(total, 6650)
})

test_that("energy_output: grain*coeff + straw*coeff", {
  eo <- energy_output(1000, 1000, grain_energy_coeff = 14.7,
                      straw_energy_coeff = 12.5, verbose = FALSE)
  expect_equal(eo, 27200)  # 1000*14.7 + 1000*12.5
})

test_that("energy_use_efficiency: EUE = out/in", {
  expect_equal(energy_use_efficiency(120000, 12000, verbose = FALSE), 10)
  expect_error(energy_use_efficiency(120000, 0, verbose = FALSE))
})

test_that("eroi: identical to EUE numerically", {
  eue <- energy_use_efficiency(120000, 12000, verbose = FALSE)
  eroi_val <- eroi(120000, 12000, verbose = FALSE)
  expect_equal(eue, eroi_val)
})

test_that("eroi: handles vector input", {
  result <- eroi(c(40800, 59800), c(12500, 8500), verbose = FALSE)
  expect_length(result, 2)
  expect_equal(result[1], round(40800 / 12500, 2))
})

test_that("net_energy: out - in", {
  expect_equal(net_energy(135000, 12000, verbose = FALSE), 123000)
  expect_equal(net_energy(10000, 12000, verbose = FALSE), -2000)
})

test_that("energy_intensity: inverse of productivity", {
  ei <- energy_intensity(12000, 4500, verbose = FALSE)
  ep <- energy_productivity(4500, 12000, verbose = FALSE)
  expect_equal(round(ei * ep, 2), 1)
})

# --- FOOD MODULE ---

test_that("harvest_index: economic/biological", {
  hi <- harvest_index(4500, 10000, verbose = FALSE)
  expect_equal(hi, 0.45)
  expect_warning(harvest_index(11000, 10000, verbose = FALSE))
})

test_that("land_equivalent_ratio: formula correct", {
  ler <- land_equivalent_ratio(3500, 4500, 800, 1200, verbose = FALSE)
  expected <- round(3500 / 4500 + 800 / 1200, 4)
  expect_equal(ler, expected)
})

test_that("caloric_yield: simple multiplication", {
  expect_equal(caloric_yield(1000, 3400, verbose = FALSE), 3400000)
})

test_that("protein_yield: percentage conversion", {
  expect_equal(protein_yield(4500, 12.5, verbose = FALSE), 562.5)
})

test_that("system_productivity_index: crop equivalent", {
  spi <- system_productivity_index(c(5000, 4500), c(22, 25), 25,
                                    verbose = FALSE)
  expected <- round((5000 * 22 + 4500 * 25) / 25, 2)
  expect_equal(spi, expected)
})

# --- NUTRIENT MODULE ---

test_that("agronomic_efficiency: (Yf - Y0) / F", {
  ae <- agronomic_efficiency(4500, 3000, 120, verbose = FALSE)
  expect_equal(ae, 12.5)
})

test_that("physiological_efficiency: handles zero delta", {
  expect_warning(
    pe <- physiological_efficiency(4500, 3000, 60, 60, verbose = FALSE)
  )
  expect_true(is.na(pe))
})

test_that("recovery_efficiency: (Uf - U0) / F", {
  re <- recovery_efficiency(100, 60, 120, verbose = FALSE)
  expect_equal(re, round(40 / 120, 4))
})

test_that("partial_factor_productivity: Y / F", {
  pfp <- partial_factor_productivity(4500, 120, verbose = FALSE)
  expect_equal(pfp, 37.5)
})

test_that("nutrient_balance: structure correct", {
  nb <- nutrient_balance(120, 60, 40, 95, 25, 80, verbose = FALSE)
  expect_s3_class(nb, "data.frame")
  expect_equal(nrow(nb), 3)
  expect_equal(nb$nutrient, c("N", "P", "K"))
  expect_equal(nb$balance_kg_ha, c(25, 35, -40))
})

test_that("nutrient_use_efficiency: all metrics present", {
  nue <- nutrient_use_efficiency(4500, 3000, 120, 100, 60,
                                  verbose = FALSE)
  expect_s3_class(nue, "data.frame")
  expect_true(all(c("agronomic_eff", "recovery_eff",
                     "partial_factor_prod") %in% names(nue)))
})

# --- CARBON MODULE ---

test_that("carbon_footprint: AR6 GWP defaults (CH4=27, N2O=273)", {
  cf <- carbon_footprint(n_fertilizer = 100, verbose = FALSE)
  # N2O: 100 * 0.01 * 44/28 = 1.5714 kg N2O
  # N2O CO2-eq: 1.5714 * 273 = 429.0
  # N manufacture: 100 * 4.96 = 496
  n2o_expected <- 100 * 0.01 * (44 / 28) * 273
  n_manuf <- 100 * 4.96
  expect_equal(cf$total_cf, round(n2o_expected + n_manuf, 2))
})

test_that("carbon_footprint: returns list with correct structure", {
  cf <- carbon_footprint(diesel_use = 60, n_fertilizer = 120,
                         yield = 4500, verbose = FALSE)
  expect_type(cf, "list")
  expect_true(all(c("total_cf", "breakdown", "cf_intensity") %in% names(cf)))
  expect_s3_class(cf$breakdown, "data.frame")
  expect_true(cf$total_cf > 0)
  expect_true(cf$cf_intensity > 0)
})

test_that("soil_carbon_stock: SOC% * BD * D * (1-CF) * 0.1", {
  soc <- soil_carbon_stock(0.65, 1.45, 30, verbose = FALSE)
  expected <- round(0.65 * 1.45 * 30 * 0.1, 2)
  expect_equal(soc, expected)
})

test_that("global_warming_potential: AR6 values", {
  # 100yr: CO2=1, CH4=27, N2O=273
  gwp100 <- global_warming_potential(co2 = 100, ch4 = 10, n2o = 1,
                                     verbose = FALSE)
  expect_equal(gwp100, 100 + 10 * 27 + 1 * 273)

  # 20yr: CH4=81, N2O=273
  gwp20 <- global_warming_potential(co2 = 100, ch4 = 10, n2o = 1,
                                    time_horizon = "20yr", verbose = FALSE)
  expect_equal(gwp20, 100 + 10 * 81 + 1 * 273)
  expect_true(gwp20 > gwp100)
})

test_that("carbon_sequestration_rate: (final - initial) / years", {
  csr <- carbon_sequestration_rate(28.5, 31.2, 5, verbose = FALSE)
  expect_equal(csr, round((31.2 - 28.5) / 5, 3))
})

# --- NEXUS MODULE ---

test_that("normalize_minmax: 0-1 range", {
  nm <- normalize_minmax(c(10, 20, 30, 40, 50))
  expect_equal(nm[1], 0)
  expect_equal(nm[5], 1)
  # Inverse
  nm_inv <- normalize_minmax(c(10, 20, 30, 40, 50), inverse = TRUE)
  expect_equal(nm_inv[1], 1)
  expect_equal(nm_inv[5], 0)
  # Constant input
  nm_const <- normalize_minmax(c(5, 5, 5))
  expect_true(all(nm_const == 0.5))
})

test_that("nexus_index: equal scores = that score", {
  ni <- nexus_index(0.8, 0.8, 0.8, 0.8, 0.8, verbose = FALSE)
  expect_equal(ni, 0.8)
})

test_that("nexus_index: rejects bad weights", {
  expect_error(nexus_index(0.8, 0.8, 0.8, 0.8, 0.8,
               weights = c(0.5, 0.5), verbose = FALSE))
  expect_error(nexus_index(0.8, 0.8, 0.8, 0.8, 0.8,
               weights = rep(0.3, 5), verbose = FALSE))
})

test_that("nexus_sustainability_score: categories correct", {
  nss <- nexus_sustainability_score(0.9, 0.85, 0.95, 0.80, 0.90,
                                     verbose = FALSE)
  expect_equal(nss$category, "Highly Sustainable")
  nss2 <- nexus_sustainability_score(0.3, 0.2, 0.25, 0.3, 0.2,
                                      verbose = FALSE)
  expect_equal(nss2$category, "Unsustainable")
})

test_that("nexus_summary: returns correct structure", {
  ns <- nexus_summary(
    yield = c(4500, 3800), water_consumed = c(450, 350),
    energy_input = c(12000, 9500), energy_output = c(135000, 112000),
    n_applied = c(120, 120), n_uptake = c(100, 80),
    carbon_emission = c(2500, 1800), verbose = FALSE
  )
  expect_s3_class(ns, "data.frame")
  expect_true(all(c("nexus_index", "EROI", "WUE_kg_mm") %in% names(ns)))
  expect_equal(nrow(ns), 2)
})

test_that("nexus_tradeoff: returns correlation matrix", {
  df <- data.frame(a = c(1, 2, 3, 4, 5), b = c(5, 4, 3, 2, 1))
  ct <- nexus_tradeoff(df, verbose = FALSE)
  expect_true(is.matrix(ct))
  expect_equal(ct["a", "b"], -1)  # Perfect negative correlation
})
