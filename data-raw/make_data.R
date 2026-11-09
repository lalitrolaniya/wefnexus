# Builds energy_coefficients and arid_pulse_trial. Run from package root.

energy_coefficients <- data.frame(
  input = c("human_labour_man", "human_labour_woman", "bullock_pair",
            "diesel", "electricity", "machinery", "irrigation_water",
            "nitrogen", "phosphorus", "potassium", "zinc_sulphate",
            "sulphur", "fym", "seed", "herbicide", "insecticide",
            "fungicide", "biofertilizer", "grain", "straw"),
  unit = c("man-h", "woman-h", "pair-h", "L", "kWh", "kg", "m3",
           "kg N", "kg P2O5", "kg K2O", "kg", "kg", "kg", "kg",
           "kg a.i.", "kg a.i.", "kg a.i.", "kg", "kg", "kg"),
  mj_per_unit = c(1.96, 1.57, 10.10, 56.31, 11.93, 62.70, 1.02,
                  60.60, 11.10, 6.70, 20.90, 1.12, 0.30, 14.70,
                  238.00, 199.00, 92.00, 10.00, 14.70, 12.50),
  type = c(rep("input", 18), rep("output", 2)),
  form = c("direct", "direct", "direct", "direct", "direct", "indirect",
           "direct", rep("indirect", 11), rep(NA, 2)),
  source = c("renewable", "renewable", "renewable", "non-renewable",
             "non-renewable", "non-renewable", "renewable",
             rep("non-renewable", 5), "renewable", "renewable",
             rep("non-renewable", 3), "renewable", rep(NA, 2)),
  note = c("Adult male labour", "Adult female labour", "Pair of bullocks",
           "High-speed diesel", "Grid electricity at farm",
           paste("Embodied energy per kg of machine mass apportioned to",
                 "the crop; some studies apply 62.7 MJ per machine-hour"),
           paste("Water delivered to field; avoid double counting if",
                 "pumping diesel or electricity is entered separately"),
           "Fertilizer N", "Fertilizer P as P2O5", "Fertilizer K as K2O",
           "Zinc sulphate", "Elemental sulphur", "Farmyard manure (dry)",
           "Seed of cereals and pulses", "Herbicide active ingredient",
           "Insecticide active ingredient", "Fungicide active ingredient",
           "Carrier-based biofertilizer",
           "Grain of cereals and pulses", "Straw or haulm"),
  stringsAsFactors = FALSE
)

# Replicated trial (RCBD, 3 replications) simulated around the
# treatment means of arid_pulse_nexus.
load("data/arid_pulse_nexus.rda")
m <- arid_pulse_nexus
set.seed(2026)
blk <- c(-0.03, 0, 0.03)
rows <- list()
for (i in seq_len(nrow(m))) {
  for (r in 1:3) {
    e <- 1 + blk[r] + rnorm(1, 0, 0.045)
    gy <- round(m$grain_yield[i] * e)
    sy <- round(m$straw_yield[i] * (1 + blk[r] + rnorm(1, 0, 0.05)))
    nu <- round(m$n_uptake[i] * (1 + blk[r] + rnorm(1, 0, 0.05)), 1)
    rows[[length(rows) + 1]] <- data.frame(
      treatment = m$treatment[i], replication = paste0("R", r),
      grain_yield = gy, straw_yield = sy,
      total_water = m$total_water[i],
      energy_input = m$energy_input[i],
      energy_output = round(gy * 14.7 + sy * 12.5),
      n_applied = m$n_applied[i], n_uptake = nu,
      grain_n_uptake = round(nu * m$grain_n_uptake[i] / m$n_uptake[i], 1),
      ghg_emission = round(m$ghg_emission[i] *
                             (1 + rnorm(1, 0, 0.04))),
      stringsAsFactors = FALSE)
  }
}
arid_pulse_trial <- do.call(rbind, rows)

save(energy_coefficients, file = "data/energy_coefficients.rda",
     compress = "bzip2")
save(arid_pulse_trial, file = "data/arid_pulse_trial.rda",
     compress = "bzip2")
