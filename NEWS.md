# wefnexus 1.1.0

This release is dedicated to the memory of co-authors Dr. Monika Punia
and Dr. Hemant Poonia.

## New features

* `nexus_analysis()` runs the full WEFNC workflow on replicated
  (plot-level) field data in one call: plot-level indicators, ANOVA,
  composite nexus index from treatment means, and TOPSIS ranking.
* `nexus_anova()` gives treatment means with letter groups (Fisher's LSD),
  SEm, CD and CV for CRD and RCBD experiments, as a table ready for a
  manuscript. Means are least-squares means (block-adjusted when plots
  are missing), every pair is tested with its own standard error, and
  letters use the insert-and-absorb algorithm of Piepho (2004), so they
  stay exact with unequal replication and beyond 26 groups.
* `nexus_weights()` derives objective dimension weights by the entropy,
  CRITIC or PCA method, as an alternative to equal weights.
* `nexus_topsis()` ranks treatments by TOPSIS on raw indicators, handling
  lower-is-better indicators such as carbon footprint directly.
* `energy_budget()` converts physical input quantities (diesel, fertilizer,
  labour hours, seed, chemicals) and outputs to energy, with direct,
  indirect, renewable and non-renewable shares and efficiency indicators.
* New datasets: `energy_coefficients` (editable table of 20 energy
  equivalents) and `arid_pulse_trial` (6 treatments x 3 replications,
  RCBD).
* `nexus_rank_compare()` ranks treatments eight ways (composite index and
  TOPSIS, each with equal, entropy, CRITIC and PCA weights) and reports
  Kendall's W, a consensus ranking and Spearman correlations between
  rankings, so the robustness of a recommendation can be shown.
* Publication-ready ggplot2 figures: `plot()` methods for `nexus_anova`
  (means with error bars and letter groups), `energy_budget` (stacked
  input energy by item, direct/indirect or renewable/non-renewable),
  `topsis_result`, `rank_compare` and `nexus_analysis`; and
  `plot_tradeoff()` for the trade-off matrix with significance stars.
* `nexus_tradeoff()` now attaches correlation-test p-values and pair
  counts as attributes (`attr(x, "p_values")`, `attr(x, "n")`).
* `nexus_radar_gg()` gains `facet = TRUE` (one panel per treatment, with
  the mean profile as a reference) and a `colours` argument;
  `nexus_heatmap_gg()` gains `colours`.
* All plots now use a colour-blind-checked palette and a single-hue
  sequential scale for scores, replacing the red-green defaults. Radar
  charts start at the top and run clockwise.
* `nexus_summary()` gains `nutrient_metric` (`"pfp"` or `"iue"`) for
  legumes grown without fertilizer N, `carbon_basis` (`"emission"` or
  `"intensity"`), and accepts a weighting method name in `weights`. The
  output adds `IUE_kg_kgN` and `rank` columns and stores the weights used.
* `carbon_footprint()` and `ghg_emission()` gain `ef_n2o` so the 2019 IPCC
  disaggregated factors (0.005 dry, 0.016 wet climates) can be used;
  `ghg_emission()` also gains `ef_ch4_paddy`.
* All functions now default to `verbose = getOption("wefnexus.verbose",
  TRUE)`, so `options(wefnexus.verbose = FALSE)` silences messages
  globally.

## Bug fixes

* `ghg_emission()` added biogenic CO2 from residue burning to the total
  and left out burning CH4 and N2O. Burning now follows IPCC (2006, Vol.
  4, Ch. 2): combustion factor 0.8 and emission factors of 2.7 g CH4 and
  0.07 g N2O per kg dry matter burnt; biogenic CO2 is reported but only
  counted with `include_burn_co2 = TRUE`. The single-observation output
  gains `counted` and an `attr(, "total_CO2eq")`, and vector inputs of
  any argument are recycled.
* `global_warming_potential(time_horizon = "20yr")` used 81 for CH4 and
  overrode user-supplied values. It now uses the AR6 20-year value for
  non-fossil methane (79.7, consistent with 27 at 100 years) and keeps
  any `gwp_ch4` or `gwp_n2o` you supply. The AR5 note is corrected.
* `soil_carbon_stock()` returned values 10 times too low. SOC (percent) x
  bulk density (Mg/m3) x depth (cm) is already Mg C/ha, but an extra
  factor of 0.1 was applied, so the result was in kg C/m2 while labelled
  Mg C/ha. Stocks and any sequestration rates derived from them should
  be recalculated.
* `nexus_heatmap_gg()` placed the values of the last treatment against the
  label of the first (rows were reversed relative to their labels). The
  base-graphics `nexus_heatmap()` was not affected.
* `nutrient_use_efficiency()` returned a fixed nutrient harvest index of
  0.75 for every input. It now computes NHI from the new `grain_uptake`
  argument and returns `NA` when that is not supplied.
* `water_footprint()` multiplied the footprint by `area`, although water
  (mm) and yield (kg/ha) are both per hectare. `area` is now deprecated
  and ignored, with a warning when a value other than 1 is given.
* `nexus_summary()` required `n_uptake` but never used it, and failed when
  N applied was zero. Both are fixed.
* `ghg_emission()` returned a single tillage value for vector input; it is
  now repeated for each observation.
* `carbon_footprint()` no longer divides by zero in source shares when the
  total footprint is zero.
* `nexus_index()` now warns when scores fall outside 0 to 1, checks that
  score vectors have matching lengths, and rejects negative weights.
* `crop_water_stress_index()` now cites Jackson et al. (1981) for the
  evapotranspiration form of the index.

* `carbon_footprint()` no longer fails when `yield` is a vector and the
  other inputs are single values.
* `soil_carbon_stock()` now rejects a coarse fragment fraction of 1 or
  more.
* `nexus_summary()` warns when every treatment receives the same N, since
  PFP is then proportional to yield and yield would carry double weight.
* `nexus_weights()` CRITIC weights no longer change when a constant
  indicator is present; `nexus_topsis()` rejects all-zero weights;
  `nexus_heatmap_gg()`, `nexus_radar_gg()` and `nexus_heatmap()` check
  that labels match the score matrix; `energy_budget()` warns about
  missing quantities and rejects unknown categories in the coefficient
  table.
* `nutrient_balance()` no longer fails when an input is zero (for example
  no K applied); the output-to-input ratio is `NA` for that nutrient.
* The reference DOI for Lal (2004) in `carbon_footprint()` pointed to an
  unrelated article; corrected to 10.1016/j.envint.2004.03.005. All other
  DOIs were checked against Crossref.

## Changed defaults

* `carbon_footprint()` grid electricity factor updated from 0.82 to 0.727
  kg CO2/kWh (Central Electricity Authority, CO2 Baseline Database v20,
  2023-24). Use `ef_electricity = 0.82` to reproduce earlier results.

## Documentation

* `nexus_rank_compare()` reports the chi-square statistic for reference
  only: the rankings share the same data, so W is read descriptively.
* Help-page formulas corrected to match the code (no change in results)
  for `energy_profitability()`, `crop_yield_index()`,
  `production_efficiency_index()`, `nutrient_harvest_index()`,
  `carbon_footprint()`, `carbon_sustainability_index()`,
  `food_productivity_index()`, `specific_energy()` and
  `nexus_sustainability_score()`.

* Vignette extended with energy budgeting, objective weighting, TOPSIS,
  ranking robustness, figures and the replicated-trial workflow.
* README function counts corrected (59 exported functions).

# wefnexus 1.0.0

## Initial CRAN release

### Water Module (7 functions)
* `water_use_efficiency()` - WUE (kg/ha/mm)
* `water_productivity()` - Physical and economic WP (kg/m3)
* `water_footprint()` - Green, blue, grey WF decomposition
* `irrigation_efficiency()` - Conveyance, application, overall, consumptive
* `crop_water_stress_index()` - CWSI bounded 0-1
* `crop_water_productivity()` - CWP (kg/m3) from ET
* `depleted_fraction()` - Basin-level water allocation metric

### Energy Module (10 functions)
* `energy_input()` - 15-component energy input aggregation
* `energy_output()` - Grain + straw energy with customizable coefficients
* `energy_use_efficiency()` - EUE ratio
* `eroi()` - Energy Return on Investment with interpretation
* `energy_productivity()` - Yield per MJ input
* `energy_intensity()` - MJ per kg yield
* `specific_energy()` - MJ/ha/day for duration comparisons
* `net_energy()` - Energy balance (output minus input)
* `energy_profitability()` - Economic return per MJ
* `human_energy_profitability()` - Labour energy amplification ratio

### Food Module (8 functions)
* `food_productivity_index()` - Composite yield-protein-calorie index
* `crop_yield_index()` - Relative to check treatment
* `harvest_index()` - Economic to biological yield ratio
* `land_equivalent_ratio()` - Intercropping advantage (LER)
* `system_productivity_index()` - Multi-crop system equivalent
* `caloric_yield()` - kcal/ha output
* `protein_yield()` - kg protein/ha
* `production_efficiency_index()` - Yield per unit cost

### Nutrient Module (8 functions)
* `agronomic_efficiency()` - AE (kg yield increase/kg nutrient)
* `physiological_efficiency()` - PE (yield/uptake increase)
* `recovery_efficiency()` - RE (uptake increase/applied)
* `partial_factor_productivity()` - PFP (total yield/applied)
* `internal_utilization_efficiency()` - IUE (yield/total uptake)
* `nutrient_harvest_index()` - NHI (grain/total plant nutrient)
* `nutrient_balance()` - N-P-K input-output balance sheet
* `nutrient_use_efficiency()` - Comprehensive single-call NUE report

### Carbon Module (7 functions)
* `carbon_footprint()` - Source-wise CF with IPCC AR6 GWP defaults
* `carbon_efficiency()` - Yield per kg CO2-eq
* `carbon_sustainability_index()` - CSI with sequestration offset
* `ghg_emission()` - N2O/CH4/CO2 estimation (IPCC Tier 1)
* `soil_carbon_stock()` - SOC stock (Mg C/ha)
* `carbon_sequestration_rate()` - Annual SOC change
* `global_warming_potential()` - GWP with AR6 values (100yr and 20yr)

### Nexus Integration (8 functions)
* `normalize_minmax()` - Min-max 0-1 scaling with inverse option
* `normalize_zscore()` - Z-score standardization
* `nexus_index()` - Weighted composite WEFNC index
* `nexus_tradeoff()` - Correlation-based trade-off matrix
* `nexus_radar()` - Spider/web chart visualization
* `nexus_heatmap()` - Treatment x dimension heatmap
* `nexus_sustainability_score()` - Categorized sustainability assessment
* `nexus_summary()` - One-call complete nexus analysis from raw data

### Dataset
* `arid_pulse_nexus` - 6 CA treatments x 26 variables from western Rajasthan

### Technical Notes
* GWP defaults updated to IPCC AR6: CH4 = 27, N2O = 273 (100-yr)
* India-specific grid electricity EF: 0.82 kg CO2/kWh (CEA 2023)
* All functions use `verbose = TRUE` parameter (CRAN compliant)
* All `@return` tags present in documentation
* No `cat()`/`print()` outside `if(verbose)` blocks
* All examples executable (no `\dontrun`)
