# ===========================================================================
# Tests for features and fixes added in wefnexus 1.1.0
# ===========================================================================

# --- Fixes -------------------------------------------------------------------

test_that("global option silences messages", {
  old <- options(wefnexus.verbose = FALSE)
  on.exit(options(old))
  expect_silent(water_use_efficiency(4500, 450))
  options(wefnexus.verbose = TRUE)
  expect_message(water_use_efficiency(4500, 450), "WUE")
})

test_that("nutrient_use_efficiency: NHI computed from grain uptake", {
  r <- nutrient_use_efficiency(4500, 3000, 120, 100, 60,
                               grain_uptake = 72, verbose = FALSE)
  expect_equal(r$nutrient_harvest_idx, 0.72)
  r2 <- nutrient_use_efficiency(4500, 3000, 120, 100, 60, verbose = FALSE)
  expect_true(is.na(r2$nutrient_harvest_idx))
})

test_that("water_footprint: area is ignored with a warning", {
  base <- water_footprint(300, 200, 50, 4000, verbose = FALSE)
  expect_warning(w2 <- water_footprint(300, 200, 50, 4000, area = 5,
                                       verbose = FALSE), "deprecated")
  expect_equal(w2, base)
  expect_silent(water_footprint(300, 200, 50, 4000, area = 1L,
                                verbose = FALSE))
  # 300 mm green over 4000 kg/ha = 3000 m3 / 4000 kg = 0.75 m3/kg
  expect_equal(base$green_wf, 0.75)
})

test_that("emission factors can be changed", {
  # dry-climate EF1 = 0.005: 100 kg N * 0.005 * 44/28 * 273
  g <- ghg_emission(100, ef_n2o = 0.005, verbose = FALSE)
  expect_equal(g$CO2_eq_kg[g$source == "N2O_soil"],
               round(100 * 0.005 * 44 / 28 * 273, 2))
  cf <- carbon_footprint(n_fertilizer = 100, ef_n2o = 0.005,
                         verbose = FALSE)
  expect_equal(cf$breakdown$emission_kg_CO2eq[8],
               round(100 * 0.005 * 44 / 28 * 273, 2))
  gv <- ghg_emission(c(50, 100), tillage = "zero", verbose = FALSE)
  expect_equal(gv$tillage_CO2, c(50, 50))
})

test_that("carbon_footprint: zero total gives NA shares, no error", {
  cf <- carbon_footprint(verbose = FALSE)
  expect_true(all(is.na(cf$breakdown$share_pct)))
})

test_that("nexus_index warns on scores outside 0-1 and length mismatch", {
  expect_warning(nexus_index(1.5, 0.5, 0.5, 0.5, 0.5, verbose = FALSE))
  expect_error(nexus_index(c(0.1, 0.2), c(0.1, 0.2, 0.3), 0.5, 0.5, 0.5,
                           verbose = FALSE))
  expect_error(nexus_index(0.5, 0.5, 0.5, 0.5, 0.5,
                           weights = c(1.2, -0.2, 0, 0, 0),
                           verbose = FALSE))
})

test_that("nexus_summary: legumes with zero N via IUE", {
  r <- nexus_summary(
    yield = c(950, 870, 780), water_consumed = c(260, 240, 220),
    energy_input = c(5200, 4800, 4100),
    energy_output = c(36000, 33000, 29500), n_applied = c(0, 0, 0),
    n_uptake = c(62, 57, 50), carbon_emission = c(610, 560, 470),
    nutrient_metric = "iue", carbon_basis = "intensity", verbose = FALSE)
  expect_s3_class(r, "nexus_result")
  expect_true(all(is.na(r$PFP_kg_kgN)))
  expect_equal(r$IUE_kg_kgN, round(c(950 / 62, 870 / 57, 780 / 50), 2))
  expect_equal(sort(r$rank), 1:3)
  expect_error(nexus_summary(
    yield = c(950, 870), water_consumed = c(260, 240),
    energy_input = c(5200, 4800), energy_output = c(36000, 33000),
    n_applied = c(0, 0), carbon_emission = c(610, 560),
    verbose = FALSE), "iue")
})

test_that("nexus_summary: defaults reproduce version 1.0.0 index", {
  expect_warning(r <- nexus_summary(
    yield = c(4500, 4200, 3800), water_consumed = c(450, 400, 350),
    energy_input = c(12000, 11000, 9500),
    energy_output = c(135000, 125000, 112000),
    n_applied = c(120, 120, 120), n_uptake = c(100, 90, 80),
    carbon_emission = c(2500, 2200, 1800), verbose = FALSE),
    "same for every treatment")
  w <- normalize_minmax(c(10, 10.5, 3800 / 350))
  e <- normalize_minmax(round(c(135000 / 12000, 125000 / 11000,
                                112000 / 9500), 2))
  f <- normalize_minmax(c(4500, 4200, 3800))
  n <- normalize_minmax(round(c(4500, 4200, 3800) / 120, 2))
  cc <- normalize_minmax(c(2500, 2200, 1800), inverse = TRUE)
  expect_equal(r$nexus_index, round(0.2 * (w + e + f + n + cc), 4),
               tolerance = 1e-3)
  expect_equal(unname(attr(r, "weights")), rep(0.2, 5))
})

# --- nexus_weights -----------------------------------------------------------

ind <- data.frame(a = c(1, 2, 3, 4), b = c(10, 10, 10, 11),
                  c = c(4, 3, 2, 1))

test_that("nexus_weights: sum to 1 for all methods", {
  for (m in c("equal", "entropy", "critic", "pca")) {
    w <- nexus_weights(ind, method = m, verbose = FALSE)
    expect_equal(sum(w), 1)
    expect_true(all(w >= 0))
    expect_named(w, c("a", "b", "c"))
  }
})

test_that("nexus_weights: entropy matches hand calculation", {
  x <- data.frame(p = c(0, 0.5, 1), q = c(0, 0, 1))
  # p: shares 0, 1/3, 2/3; q: shares 0, 0, 1 (e = 0)
  sp <- c(1 / 3, 2 / 3)
  ep <- -sum(sp * log(sp)) / log(3)
  w <- nexus_weights(x, "entropy", verbose = FALSE)
  expect_equal(as.numeric(w), c(1 - ep, 1) / (2 - ep), tolerance = 1e-8)
})

test_that("nexus_weights: CRITIC matches hand calculation", {
  x <- data.frame(p = c(0, 0.5, 1), q = c(0, 1, 0.5))
  r <- stats::cor(x$p, x$q)
  s <- c(stats::sd(x$p), stats::sd(x$q))
  cj <- s * (1 - r)
  w <- nexus_weights(x, "critic", verbose = FALSE)
  expect_equal(as.numeric(w), cj / sum(cj), tolerance = 1e-8)
})

test_that("nexus_weights: constant column gets zero weight", {
  x <- data.frame(p = c(1, 2, 3), q = c(5, 5, 5), r = c(3, 1, 2))
  for (m in c("entropy", "critic", "pca")) {
    expect_equal(unname(nexus_weights(x, m, verbose = FALSE)[2]), 0)
  }
  expect_error(nexus_weights(ind, benefit = TRUE, verbose = FALSE))
})

# --- nexus_topsis ------------------------------------------------------------

test_that("nexus_topsis: hand-calculated closeness", {
  x <- data.frame(b = c(3, 4), c = c(2, 1))
  r <- nexus_topsis(x, weights = c(0.5, 0.5), benefit = c(TRUE, FALSE),
                    verbose = FALSE)
  v1 <- c(3, 4) / 5 * 0.5
  v2 <- c(2, 1) / sqrt(5) * 0.5
  dp <- sqrt((v1 - max(v1))^2 + (v2 - min(v2))^2)
  dn <- sqrt((v1 - min(v1))^2 + (v2 - max(v2))^2)
  expect_equal(r$closeness, round(dn / (dp + dn), 4))
  expect_equal(r$rank, c(2L, 1L))
})

test_that("nexus_topsis: dominant treatment is ranked first", {
  x <- data.frame(y = c(1, 2, 3), w = c(1, 2, 3), cf = c(3, 2, 1))
  r <- nexus_topsis(x, weights = "entropy",
                    benefit = c(TRUE, TRUE, FALSE),
                    treatment_names = c("A", "B", "C"), verbose = FALSE)
  expect_equal(r$treatment[r$rank == 1], "C")
  expect_equal(r$closeness[3], 1)
  expect_error(nexus_topsis(x, weights = c(0.5, 0.5), verbose = FALSE))
})

# --- nexus_anova -------------------------------------------------------------

test_that("nexus_anova: RCBD statistics match aov()", {
  data(arid_pulse_trial, envir = environment())
  r <- nexus_anova(arid_pulse_trial, "grain_yield", "treatment",
                   block = "replication", verbose = FALSE)
  fit <- stats::aov(grain_yield ~ factor(replication) + factor(treatment),
                    data = arid_pulse_trial)
  tab <- summary(fit)[[1]]
  mse <- tab["Residuals", "Mean Sq"]
  expect_equal(r$stats$df_error, 10)
  expect_equal(r$stats$SEm, round(sqrt(mse / 3), 4))
  expect_equal(r$stats$CD, round(stats::qt(0.975, 10) *
                                   sqrt(2 * mse / 3), 4))
  expect_equal(r$stats$CV_pct,
               round(100 * sqrt(mse) / mean(arid_pulse_trial$grain_yield),
                     2))
  expect_s3_class(r, "nexus_anova")
  expect_equal(nrow(r$table), 6 + 3)
})

test_that("nexus_anova: CRD and non-significant case", {
  set.seed(1)
  d <- data.frame(t = rep(c("A", "B", "C"), each = 4),
                  y = rnorm(12, 10, 1))
  d$y[d$t == "C"] <- d$y[d$t == "C"]
  r <- nexus_anova(d, "y", "t", verbose = FALSE)
  expect_equal(r$stats$df_error, 9)
  if (!r$stats$significant) {
    expect_true(all(r$letters$y == "a"))
    expect_equal(r$table$y[5], "NS")
  }
  expect_error(nexus_anova(d, "z", "t", verbose = FALSE), "not found")
  expect_error(nexus_anova(d[c(1, 5, 9), ], "y", "t", verbose = FALSE),
               "replicated")
})

test_that("LSD letters: hand-checked grouping", {
  f <- getFromNamespace(".lsd_letters", "wefnexus")
  expect_equal(f(c(10, 9.5, 8, 7.8, 5), cd = 1), c("a", "a", "b", "b", "c"))
  expect_equal(f(c(10, 9.4, 8.8), cd = 1), c("a", "ab", "b"))
  expect_equal(f(c(3, 2, 1), cd = 5), c("a", "a", "a"))
})

# --- energy_budget -----------------------------------------------------------

test_that("energy_budget: totals and shares", {
  x <- data.frame(trt = "T1", diesel = 10, nitrogen = 20, seed = 10,
                  human_labour_man = 100, grain = 1000, straw = 1500)
  eb <- energy_budget(x, verbose = FALSE)
  tot <- 10 * 56.31 + 20 * 60.60 + 10 * 14.70 + 100 * 1.96
  expect_equal(eb$summary$total_input_MJ, round(tot, 2))
  expect_equal(eb$summary$direct_MJ, round(10 * 56.31 + 100 * 1.96, 2))
  expect_equal(eb$summary$renewable_MJ, round(10 * 14.70 + 100 * 1.96, 2))
  expect_equal(eb$summary$direct_pct + eb$summary$indirect_pct, 100,
               tolerance = 0.02)
  expect_equal(eb$summary$total_output_MJ, 1000 * 14.7 + 1500 * 12.5)
  expect_equal(eb$summary$EUE, round((1000 * 14.7 + 1500 * 12.5) / tot, 2))
  expect_equal(eb$summary$energy_intensity_MJ_kg, round(tot / 1000, 2))
  expect_s3_class(eb, "energy_budget")
})

test_that("energy_budget: named vector, custom table, errors", {
  eb <- energy_budget(c(diesel = 10, nitrogen = 20), verbose = FALSE)
  expect_equal(eb$summary$total_input_MJ, round(563.1 + 1212, 2))
  cf <- energy_coefficients
  cf$mj_per_unit[cf$input == "diesel"] <- 50
  eb2 <- energy_budget(c(diesel = 10), coefficients = cf, verbose = FALSE)
  expect_equal(eb2$summary$total_input_MJ, 500)
  expect_error(energy_budget(data.frame(foo = 1), verbose = FALSE))
  expect_error(energy_budget(c(diesel = -1), verbose = FALSE))
  expect_message(energy_budget(c(diesel = 1, xyz = 2), verbose = TRUE),
                 "xyz")
})

test_that("energy_coefficients: well formed", {
  expect_true(all(c("input", "unit", "mj_per_unit", "type", "form",
                    "source") %in% names(energy_coefficients)))
  expect_false(anyDuplicated(energy_coefficients$input) > 0)
  expect_true(all(energy_coefficients$mj_per_unit > 0))
})

# --- nexus_analysis ----------------------------------------------------------

test_that("nexus_analysis: full workflow on replicated data", {
  data(arid_pulse_trial, envir = environment())
  r <- nexus_analysis(arid_pulse_trial, treatment = "treatment",
                      block = "replication", yield = "grain_yield",
                      water = "total_water", energy_in = "energy_input",
                      energy_out = "energy_output",
                      emission = "ghg_emission", n_uptake = "n_uptake",
                      weights = "entropy", verbose = FALSE)
  expect_s3_class(r, "nexus_analysis")
  expect_s3_class(r$nexus, "nexus_result")
  expect_s3_class(r$anova, "nexus_anova")
  expect_equal(nrow(r$nexus), 6)
  expect_equal(sum(r$weights), 1)
  expect_equal(r$plot_data$WUE[1],
               round(arid_pulse_trial$grain_yield[1] /
                       arid_pulse_trial$total_water[1], 4))
  expect_equal(r$treatment_means$grain_yield[1],
               mean(arid_pulse_trial$grain_yield[1:3]))
  expect_output(print(r), "TOPSIS|topsis")
})

test_that("nexus_analysis: unreplicated data skips ANOVA", {
  d <- arid_pulse_nexus
  d$energy_output <- d$energy_output_grain + d$energy_output_straw
  expect_warning(
    r <- nexus_analysis(d, treatment = "treatment", yield = "grain_yield",
                        water = "total_water", energy_in = "energy_input",
                        energy_out = "energy_output",
                        emission = "ghg_emission", n_applied = "n_applied",
                        verbose = FALSE),
    "same for every treatment")
  expect_null(r$anova)
  expect_error(nexus_analysis(d, "treatment", "grain_yield",
                              "total_water", "energy_input",
                              "energy_output", "ghg_emission",
                              verbose = FALSE), "n_applied")
})

# --- Audit fixes (units and defaults) -----------------------------------------

test_that("soil_carbon_stock returns Mg C/ha (no extra 0.1 factor)", {
  # 0.5 % C, BD 1.5 Mg/m3, 0-30 cm: 4,500 Mg soil/ha x 0.005 = 22.5 Mg C/ha
  expect_equal(soil_carbon_stock(0.5, 1.5, 30, verbose = FALSE), 22.5)
})

test_that("nutrient_balance accepts zero inputs", {
  nb <- nutrient_balance(20, 40, 0, 56.8, 8.2, 45, verbose = FALSE)
  expect_equal(nb$balance_kg_ha, c(-36.8, 31.8, -45))
  expect_true(is.na(nb$output_input_ratio[3]))
  expect_error(nutrient_balance(-1, 40, 0, 5, 5, 5, verbose = FALSE))
})

test_that("carbon_footprint electricity default is CEA v20 (0.727)", {
  cf <- carbon_footprint(electricity_use = 100, verbose = FALSE)
  expect_equal(cf$total_cf, 72.7)
  cf_old <- carbon_footprint(electricity_use = 100, ef_electricity = 0.82,
                             verbose = FALSE)
  expect_equal(cf_old$total_cf, 82)
})

# --- Fixes from the independent review ------------------------------------------

test_that("compact letters stay valid with many groups (no wrap at 26)", {
  f <- getFromNamespace(".lsd_letters", "wefnexus")
  m <- seq(10, 300, 10)                 # 30 means, all differ by > cd
  l <- f(m, cd = 1)
  expect_equal(length(unique(l)), 30)
  expect_false(any(duplicated(l)))
})

test_that("compact letter display is exact for random significance patterns", {
  cld <- getFromNamespace(".cld", "wefnexus")
  set.seed(11)
  for (rep in 1:150) {
    k <- sample(3:9, 1)
    m <- sort(runif(k, 0, 10), decreasing = TRUE)
    se <- runif(k, 0.2, 1.2)
    sig <- abs(outer(m, m, "-")) > 2 * sqrt(outer(se^2, se^2, "+"))
    diag(sig) <- FALSE
    l <- cld(m, sig)
    share <- outer(seq_len(k), seq_len(k), Vectorize(function(i, j)
      length(intersect(strsplit(l[i], "")[[1]], strsplit(l[j], "")[[1]])) > 0))
    expect_true(all(share[sig] == FALSE))           # differing pairs share none
    off <- !sig & row(sig) != col(sig)
    expect_true(all(share[off]))                     # others share a letter
  }
})

test_that("nexus_anova uses block-adjusted means when a plot is missing", {
  # T1 is truly 3 units above T2; block R4 is 40 units higher; T1's R4 plot lost
  d <- expand.grid(rep = paste0("R", 1:4), trt = c("T1", "T2", "T3"))
  blk_eff <- c(R1 = 0, R2 = 0, R3 = 0, R4 = 40)
  trt_eff <- c(T1 = 3, T2 = 0, T3 = -5)
  set.seed(2)
  d$y <- 100 + blk_eff[as.character(d$rep)] + trt_eff[as.character(d$trt)] +
    rnorm(nrow(d), 0, 0.3)
  d <- d[!(d$trt == "T1" & d$rep == "R4"), ]
  r <- nexus_anova(d, "y", "trt", block = "rep", verbose = FALSE)
  raw <- tapply(d$y, d$trt, mean)
  expect_lt(raw[["T1"]], raw[["T2"]])                 # raw means mislead
  expect_gt(r$means$y[1], r$means$y[2])                # adjusted means do not
  expect_false(r$stats$balanced)
  expect_equal(r$letters$y[1], "a")
})

test_that("nexus_anova: exact pairwise LSD with unequal replication", {
  set.seed(5)
  d <- data.frame(t = rep(c("A", "B", "C", "D"), times = c(2, 6, 6, 6)))
  d$y <- c(A = 12, B = 10, C = 10.4, D = 8)[d$t] + rnorm(20, 0, 0.8)
  r <- nexus_anova(d, "y", "t", verbose = FALSE)
  fit <- stats::lm(y ~ t, data = d)
  mse <- sum(stats::residuals(fit)^2) / fit$df.residual
  n <- table(d$t)[c("A", "B", "C", "D")]
  m <- tapply(d$y, d$t, mean)[c("A", "B", "C", "D")]
  tc <- stats::qt(0.975, fit$df.residual)
  for (i in 1:3) for (j in (i + 1):4) {
    differ <- abs(m[i] - m[j]) > tc * sqrt(mse * (1 / n[i] + 1 / n[j]))
    share <- length(intersect(strsplit(r$letters$y[i], "")[[1]],
                              strsplit(r$letters$y[j], "")[[1]])) > 0
    expect_equal(unname(differ), !share)
  }
})

test_that("nexus_anova survives a treatment with no data for a response", {
  d <- data.frame(t = rep(c("A", "B", "C"), each = 3), y = c(1, 2, 3, 7, 8, 9, NA, NA, NA),
                  z = c(1, 2, 1, 5, 6, 5, 9, 9, 10))
  r <- nexus_anova(d, c("y", "z"), "t", verbose = FALSE)
  expect_true(is.na(r$means$y[3]))
  expect_equal(r$letters$y[3], "")
  expect_equal(r$table$y[3], "-")
})

test_that("ghg_emission counts burning CH4 and N2O, not biogenic CO2", {
  g <- ghg_emission(100, residue_burned = TRUE, residue_amount = 5000,
                    tillage = "zero", verbose = FALSE)
  mb <- 5000 * 0.8
  expect_equal(g$emission_kg[g$source == "CH4_residue_burn"], mb * 2.7 / 1000)
  expect_equal(g$emission_kg[g$source == "N2O_residue_burn"], mb * 0.07 / 1000)
  expect_false(g$counted[g$source == "CO2_residue_burn"])
  tot <- 100 * 0.01 * 44 / 28 * 273 + mb * 2.7 / 1000 * 27 +
    mb * 0.07 / 1000 * 273 + 50
  expect_equal(attr(g, "total_CO2eq"), round(tot, 2))
  g2 <- ghg_emission(100, residue_burned = TRUE, residue_amount = 5000,
                     tillage = "zero", include_burn_co2 = TRUE, verbose = FALSE)
  expect_equal(attr(g2, "total_CO2eq"),
               round(tot + mb * 0.37 * 44 / 12, 2))
  gv <- ghg_emission(100, paddy_days = c(0, 90), verbose = FALSE)
  expect_equal(nrow(gv), 2)
  expect_equal(gv$ch4_CO2eq, c(0, 90 * 1.3 * 27))
})

test_that("GWP-20 uses biogenic CH4 79.7 and respects user values", {
  expect_equal(global_warming_potential(ch4 = 10, time_horizon = "20yr",
                                        verbose = FALSE), 797)
  expect_equal(global_warming_potential(ch4 = 10, gwp_ch4 = 81,
                                        time_horizon = "20yr",
                                        verbose = FALSE), 810)
})

test_that("input checks added after review", {
  expect_error(soil_carbon_stock(0.5, 1.5, 30, coarse_fraction = 1.5,
                                 verbose = FALSE), "below 1")
  cf <- carbon_footprint(diesel_use = 50, yield = c(1000, 2000),
                         verbose = FALSE)
  expect_equal(nrow(cf), 2)
  expect_equal(cf$cf_intensity, round(50 * 2.68 / c(1000, 2000), 4))
  expect_error(nexus_topsis(data.frame(a = 1:3, b = 3:1), weights = c(0, 0),
                            verbose = FALSE), "all be zero")
  bad <- energy_coefficients
  bad$source[bad$input == "diesel"] <- "nonrenewable"
  expect_error(energy_budget(c(diesel = 10), coefficients = bad,
                             verbose = FALSE), "Unrecognised")
  expect_warning(energy_budget(data.frame(t = c("a", "b"), diesel = c(10, NA)),
                               verbose = FALSE), "Missing quantities")
})

test_that("CRITIC ignores constant indicators when weighting the others", {
  x <- data.frame(a = c(1, 3, 2, 5), b = c(2, 1, 4, 3), c = c(9, 1, 5, 2))
  w3 <- nexus_weights(x, "critic", verbose = FALSE)
  w4 <- nexus_weights(cbind(x, k = 7), "critic", verbose = FALSE)
  expect_equal(unname(w4[1:3]), unname(as.numeric(w3)), tolerance = 1e-10)
  expect_equal(unname(w4[4]), 0)
})

test_that("ggplot2 heatmap checks label lengths", {
  skip_if_not_installed("ggplot2")
  expect_error(nexus_heatmap_gg(matrix(0.5, 4, 5),
                                treatment_names = c("A", "B")),
               "one label per row")
  expect_error(nexus_heatmap_gg(matrix(0.5, 2, 10)), "one label per column")
})
