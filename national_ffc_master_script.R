# Calculate  National-Level CO2 Emissions
# Master Script




# Load required packages
source("ffc_libraries.R")

# # data retrieval scripts here (API, etc.)?
# source("eia_api.R")
# 
# # QAQC functions to be used in subsequent scripts
# source("qa_qc.R")

# EIA SEDS code MSN descriptors, US state postal codes
source("ffc_msn_descriptions.R")

# Retrieve EIA Consumption Data from API
source("state_ffc_read_seds_data.R")

# Apply motor gasoline adjustments
source("national_ffc_mogas_adjustments.R")

# Apply residential, commercial, and electric power adjustments (none)
source("national_ffc_unadjusted.R")