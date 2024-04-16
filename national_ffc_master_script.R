# Calculate  National-Level CO2 Emissions
# Master Script


# Load required packages
source("ffc_libraries.R")

# QAQC functions to be used in subsequent scripts
# source("ffa_qa_qc.R")

# EIA SEDS code MSN descriptors, US state postal codes
source("ffc_msn_descriptions.R")

# Retrieve EIA Consumption Data from API
source("national_ffc_read_eia_data.R")

# Scrape data where required; 
# National FFC uses FHWA MF-226 (same as state FFC) for gasoline use
source("ffc_datascraping.R")

# Retrieve and calculate motor gasoline corrections data
source("national_ffc_mogas_corrections_data.R")

# Retrieve and calculate distillate fuel corrections data
source("national_ffc_dist_fuel_corrections_data.R")

# Apply residential, commercial, and electric power adjustments 
# Motor gas and dist fuel should be the only adjustments here
source("national_ffc_res_com_ele_adjustments.R")

# Apply industrial adjustments
source("national_ind_adjustments.R")

# Apply industrial adjustments
source("national_tra_adjustments.R")

# Read carbon factors data
source("ffc_carbon_factors_data.R")

# Calculate carbon emissions and collate final data set
source("national_ffc_emissions_final.R")

# Create figures (for Markdown report)
source("national_ffc_figures.R")

# Create tables (for Markdown report)
source("national_ffc_tables.R")

# R Shiny Dashboard for viewing data (in-work)
source("national_ffc_dashboards.R")

# Format data for InvDB; write data to InvDB Excel workbook
source("ffc_invdb.R")

# R Markdown report
source("national_ffc_final_report.Rmd")

