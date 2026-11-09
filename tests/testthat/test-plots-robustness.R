# ===========================================================================
# Figures, trade-off p-values and ranking robustness (wefnexus 1.1.0)
# ===========================================================================

trial_res <- function() {
  data(arid_pulse_trial, envir = environment())
  nexus_analysis(arid_pulse_trial, treatment = "treatment",
                 block = "replication", yield = "grain_yield",
                 water = "total_water", energy_in = "energy_input",
                 energy_out = "energy_output", emission = "ghg_emission",
                 n_uptake = "n_uptake", verbose = FALSE)
}

test_that("nexus_heatmap_gg keeps each value with its own treatment", {
  skip_if_not_installed("ggplot2")
  sc <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.5,
                 0.9, 0.8, 0.7, 0.6, 0.55), nrow = 2, byrow = TRUE)
  p <- nexus_heatmap_gg(sc, treatment_names = c("Low", "High"))
  d <- p$data
  expect_equal(d$Value[d$Treatment == "Low" & d$Dimension == "Water"], 0.1)
  expect_equal(d$Value[d$Treatment == "High" & d$Dimension == "Carbon"],
               0.55)
  # first treatment is drawn on the top row
  expect_equal(levels(d$Treatment)[2], "Low")
})

test_that("plot methods return ggplot objects", {
  skip_if_not_installed("ggplot2")
  res <- trial_res()
  expect_s3_class(plot(res), "ggplot")
  expect_s3_class(plot(res, which = "anova"), "ggplot")
  expect_s3_class(plot(res, which = "topsis"), "ggplot")
  expect_s3_class(plot(res, which = "heatmap"), "ggplot")
  expect_s3_class(plot(res$anova, error = "sem"), "ggplot")
  expect_s3_class(plot(res$anova, error = "none",
                       labels = c(WUE = "WUE (kg/ha/mm)")), "ggplot")
  expect_error(plot(res$anova, response = "nope"), "Unknown")
  expect_s3_class(nexus_radar_gg(matrix(runif(10), 2), facet = TRUE),
                  "ggplot")
  expect_s3_class(nexus_radar_gg(matrix(runif(10), 2)), "ggplot")
})

test_that("plot.nexus_anova draws letters and SE error bars", {
  skip_if_not_installed("ggplot2")
  res <- trial_res()
  p <- plot(res$anova, response = "grain_yield")
  d <- p$data
  expect_equal(d$letter, res$anova$letters$grain_yield)
  expect_equal(d$err, res$anova$se$grain_yield)
  se1 <- stats::sd(res$plot_data$grain_yield[1:3]) / sqrt(3)
  expect_equal(res$anova$se$grain_yield[1], se1)
})

test_that("plot.energy_budget: items, form and source", {
  skip_if_not_installed("ggplot2")
  x <- data.frame(trt = c("A", "B"), diesel = c(60, 30),
                  nitrogen = c(20, 20), seed = c(20, 18),
                  human_labour_man = c(300, 200), herbicide = c(0, 1),
                  electricity = c(100, 50), phosphorus = c(40, 40),
                  insecticide = c(0.5, 0.5), fym = c(1000, 0))
  eb <- energy_budget(x, verbose = FALSE)
  p <- plot(eb, top = 3)
  expect_true("Other" %in% as.character(p$data$group))
  expect_s3_class(plot(eb, type = "form", percent = TRUE), "ggplot")
  expect_s3_class(plot(eb, type = "source"), "ggplot")
})

test_that("nexus_tradeoff attaches p-values and n", {
  set.seed(3)
  df <- data.frame(a = 1:10, b = (1:10) + rnorm(10, 0, 0.1),
                   c = rnorm(10))
  tm <- nexus_tradeoff(df, verbose = FALSE)
  pv <- attr(tm, "p_values")
  expect_equal(dim(pv), c(3, 3))
  expect_true(pv["a", "b"] < 0.001)
  expect_equal(pv["a", "c"],
               signif(stats::cor.test(df$a, df$c)$p.value, 4))
  expect_equal(attr(tm, "n")["a", "b"], 10)
  expect_true(all(is.na(diag(pv))))
  expect_s3_class(tm, "tradeoff_matrix")
  expect_true(is.matrix(tm))
  expect_output(print(tm), "p-values")
  skip_if_not_installed("ggplot2")
  expect_s3_class(plot_tradeoff(tm), "ggplot")
  expect_s3_class(plot(tm), "ggplot")
  expect_s3_class(plot_tradeoff(tm, triangle = "full"), "ggplot")
  po <- plot_tradeoff(tm, benefit = c(TRUE, TRUE, FALSE))
  r_raw <- tm["c", "a"]
  expect_equal(po$data$r[po$data$row == "c" & po$data$col == "a"], -r_raw)
  expect_equal(po$data$r[po$data$row == "b" & po$data$col == "a"],
               tm["b", "a"])
  expect_error(plot_tradeoff(tm, benefit = TRUE), "length 3")
  expect_error(plot_tradeoff(matrix(1:6, 2)))
})

test_that("nexus_topsis returns a topsis_result that still acts as a data frame", {
  x <- data.frame(y = c(1, 2, 3), cf = c(3, 2, 1))
  ts <- nexus_topsis(x, benefit = c(TRUE, FALSE), verbose = FALSE)
  expect_s3_class(ts, "topsis_result")
  expect_s3_class(ts, "data.frame")
  expect_equal(nrow(ts[ts$rank == 1, ]), 1)
  skip_if_not_installed("ggplot2")
  expect_s3_class(plot(ts), "ggplot")
})

test_that("nexus_rank_compare: Kendall's W known cases", {
  # Perfectly concordant indicators -> every method gives the same ranking
  x <- data.frame(a = c(1, 2, 3, 4, 5), b = c(2, 4, 6, 8, 10),
                  c = c(5, 4, 3, 2, 1))
  expect_warning(
    rc <- nexus_rank_compare(x, benefit = c(TRUE, TRUE, FALSE),
                             treatment_names = LETTERS[1:5],
                             verbose = FALSE),
    "perfectly correlated")
  expect_equal(rc$kendall_w, 1)
  expect_equal(rc$ranks$consensus, 5:1)
  expect_equal(ncol(rc$ranks), 1 + 8 + 2)
  expect_equal(dim(rc$weights), c(4, 3))
  expect_true(all(abs(rowSums(rc$weights) - 1) < 1e-8))
  expect_s3_class(rc, "rank_compare")
  expect_output(print(rc), "Kendall")
})

test_that("Kendall's W matches the textbook formula", {
  # three rankings of four objects, no ties
  R <- cbind(c(1, 2, 3, 4), c(2, 1, 3, 4), c(1, 3, 2, 4))
  m <- 3; n <- 4
  S <- sum((rowSums(R) - mean(rowSums(R)))^2)
  W <- 12 * S / (m^2 * (n^3 - n))
  expect_equal(round(W, 4), 0.7778)
})

test_that("plot.rank_compare and plot_sensitivity return ggplot", {
  skip_if_not_installed("ggplot2")
  data(arid_pulse_nexus, envir = environment())
  d <- arid_pulse_nexus
  ind <- data.frame(WUE = d$grain_yield / d$total_water,
                    Yield = d$grain_yield, CF = d$ghg_emission)
  rc <- nexus_rank_compare(ind, benefit = c(TRUE, TRUE, FALSE),
                           treatment_names = d$treatment, verbose = FALSE)
  expect_s3_class(plot(rc), "ggplot")
  sa <- nexus_sensitivity(c(0.9, 0.5), c(0.6, 0.8), c(0.8, 0.7),
                          c(0.7, 0.6), c(0.5, 0.9), steps = 4,
                          verbose = FALSE)
  expect_s3_class(plot_sensitivity(sa), "ggplot")
})

test_that("text colour helper picks the higher-contrast ink", {
  f <- getFromNamespace(".text_on", "wefnexus")
  expect_equal(f(c("#ffffff", "#0d366b")), c("#0b0b0b", "white"))
})
