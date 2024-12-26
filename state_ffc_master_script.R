# Calculate State-Level CO2 Emissions
# Master Script


# Load required packages
source("ffc_libraries.R")


# Build local datasets---------------------------------

# GHGI variables, EIA SEDS code MSN descriptors, US state postal codes
source("ffc_data_setup.R")

# API and datascraping------------------------------------------------

# Retrieve SEDS data from EIA's API
# When implemented, supersedes state_ffc_read_seds_data.R
# source("state_ffc_eia_api.R")

# Read SEDS data (EIA) from local drive
# includes code to query API but currently reads from previously downloaded csv
source("state_ffc_get_seds_data.R")

# Scrape other data from web sources
source("ffc_datascraping.R")

# Retrieve national FFC corrections data for adjustments---------------------------

source("state_ffc_get_corrections_data.R")

# Carbon Factors------------------------------------------------------

# Read carbon factors data
source("ffc_carbon_factors_data.R")

# QA/QC----------------------------------------------------------------

# QAQC functions to be used in subsequent scripts
# source("ffc_qa_qc.R")

# Perform QA/QC on loaded datasets (pre-calculation)
# source("state_ffc_qa_qc_precalc.R")

# Apply adjustments to SEDS data---------------------------------------

# Apply adjustments to data
source("state_ffc_adjust_data.R")

# Calculate US territories consumption-----------------------------------

source("state_ffc_territories.R")

# Calculate emissions----------------------------------------------------

# Compute CO2 equivalent emissions
source("state_ffc_calculate_emissions.R")

# Data outputs-----------------------------------------------------------

# Create figures (for Markdown report)
source("state_ffc_figures.R")

# Create tables (for Markdown report)
source("state_ffc_tables.R")

# # R Shiny Dashboard for viewing data 
# source("state_ffc_dashboards.R")
# 
# Format data for InvDB; write data to InvDB Excel workbook
# source("ffc_invdb.R")
# 
# R Markdown report
# source("state_ffc_final_report.Rmd")

