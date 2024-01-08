# Calculate State-Level CO2 Emissions
# Master Script


source("emissions_libraries.R")
# data retrieval scripts here (API, etc.)?
source("read_seds_data.R")
source("msn_descriptions.R")
source("residential_adjustments.R")
source("commercial_adjustments.R")
source("industrial_adjustments.R")
source("transportation_adjustments.R")
source("electrical_power_adjustments.R")
source("ibf_adjustments.R")
source("neu_adjustments.R")
source("carbon_factors.R")
source("state_breakouts.R")
source("final_state_summaries.R")
source("emissions_state_final") # need a final script to collate results (?)
# Probably create a set of shared national-state scripts for generating
# Markdown docs, etc. 
# Might exclude the data retrieval scripts from this master script, as we
# won't want to download data every time





