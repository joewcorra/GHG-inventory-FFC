# Calculate State-Level CO2 Emissions
# Master Script


# Load required packages
source("ffc_libraries.R")


# Build local datasets---------------------------------

# EIA SEDS code MSN descriptors, US state postal codes
source("ffc_msn_descriptions.R")


# API and datascraping------------------------------------------------

# Retrieve SEDS data from EIA's API
# When implemented, supersedes state_ffc_read_seds_data.R
# source("state_ffc_eia_api.R")

# Read SEDS data (EIA) from local drive
source("state_ffc_read_seds_data.R")

# Scrape other data from web sources
source("ffc_datascraping.R")
# When implemented, replaces state_ffc_fhwa_distribution_data.R


# Retrieve national FFC data for adjustments---------------------------

# When implemented, replaces all of the following scripts
# source("state_ffc_national_corrections_data_ALL.R")

# Read gasoline and diesel consumption distribution data (FHWA)
# source("state_ffc_fhwa_distribution_data.R")
# Superceded by ffc_datascraping.R (see above)

# Read industrial corrections & consumption input data (National Inventory)
source("state_ffc_national_corrections_data.R")

 # Read international bunker fuels adjustment data
source("state_ffc_ibf_corrections_data.R")

# Read NEU adjustments data
source("state_ffc_neu_corrections_data.R")

# Read IPPU distributions data (for I & S, petrochemicals, and ammonia)
source("state_ffc_ippu_distribution_data.R")

# Read FOKS IBF distribution data (EIA; no longer available as of 2021)
source("state_ffc_foks_data.R")

# Carbon Factors------------------------------------------------------

# Read carbon factors data
source("ffc_carbon_factors_data.R")

# QA/QC----------------------------------------------------------------

# QAQC functions to be used in subsequent scripts
# source("ffc_qa_qc.R")

# Perform QA/QC on loaded datasets (pre-calculation)
# source("state_ffc_qa_qc_precalc.R")

# Apply adjustments to SEDS data---------------------------------------

# Apply adjustments to residential fossil fuels
source("state_ffc_res_adjustments.R")

# Apply adjustments to commercial fossil fuels
source("state_ffc_com_adjustments.R")

# Apply adjustments to industrial fossil fuels
source("state_ffc_ind_adjustments.R")
# 
# Apply adjustments to transportation fossil fuels
source("state_ffc_tra_adjustments.R")

# Apply adjustments to electrical power fossil fuels
source("state_ffc_ele_adjustments.R")

# Determine adjustments for international bunker fuels
source("state_ffc_ibf_adjustments.R")

# Determine adjustments for non-energy uses
source("state_ffc_neu_adjustments.R")

# Calculate US territories consumption-----------------------------------

source("state_ffc_territories.R")

# Calculate emissions----------------------------------------------------

# Break out data by state/year; compute CO2 equivalent emissions
source("state_ffc_breakouts.R")


# Data outputs-----------------------------------------------------------

# Create figures (for Markdown report)
source("state_ffc_figures.R")

# Create tables (for Markdown report)
source("state_ffc_tables.R")

# # R SHiny Dashboard for viewing data 
# source("state_ffc_dashboards.R")
# 
# Format data for InvDB; write data to InvDB Excel workbook
# source("ffc_invdb.R")
# 
# R Markdown report
# source("state_ffc_final_report.Rmd")

