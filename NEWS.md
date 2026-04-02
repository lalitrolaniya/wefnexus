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
