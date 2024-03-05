# Calculate State-Level CO2 Emissions
# Master Script


# Load required packages
source("ffc_libraries.R")

# data retrieval scripts here (API, etc.)?

# QAQC functions to be used in subsequent scripts
source("qa_qc.R")

# EIA SEDS code MSN descriptors, US state postal codes
source("msn_descriptions.R")

# Read SEDS data (EIA) & 'us compare' adjustments data (National Inventory)
source("read_seds_data.R")

# Read industrial corrections data & consumption input data (National Inventory)
source("national_corrections_data.R")

# Read international bunker fuels adjustment data
source("ibf_corrections_data.R")

# Read NEU adjustments data 
source("neu_corrections_data.R")

# Read IPPU distributions data (for I & S, petrochemicals, and ammonia)
source("ippu_distribution_data.R")

# Read gasoline and diesel consumption distribution data (FHWA)
source("fhwa_distribution_data.R")

# Read FOKS IBF distribution data (EIA; no longer available as of 2021)
source("foks_data.R")

# Apply adjustments to residential fossil fuels
source("residential_adjustments.R")

# Apply adjustments to commercial fossil fuels
source("commercial_adjustments.R")

# Apply adjustments to industrial fossil fuels
source("industrial_adjustments.R")

# Apply adjustments to transportation fossil fuels
source("transportation_adjustments.R")

# Apply adjustments to electrical power fossil fuels
source("electrical_power_adjustments.R")

# Determine adjustments for international bunker fuels
source("ibf_adjustments.R")

# Determine adjustments for non-energy uses
source("neu_adjustments.R")

# Read carbon factors data
source("carbon_factors_data.R")

# Break out data by state/year; compute CO2 equivalent emissions
source("state_breakouts.R")

# Create figures (for Markdown report)
source("figures.R")

# Create tables (for Markdown report)
source("tables.R")

# # R SHiny Dashboard for viewing data (in-work)
# source("dashboards.R")

# # Format data for InvDB; write data to InvDB Excel workbook
# source("invdb.R") 

# R Markdown report
# source("final_state_report.Rmd")



# Sharepoint Link:
# https://usepa.sharepoint.com/sites/U.S._GHG_Inventory_Report/Shared%20Documents/Forms/AllItems.aspx?csf=1&web=1&e=QHgRM5&cid=7ef871ac%2Df5c1%2D4767%2Dafc3%2D906ccfaa5764&RootFolder=%2Fsites%2FU%2ES%2E%5FGHG%5FInventory%5FReport%2FShared%20Documents%2FReports%2FState%5FGHGI%5F90%2D21%2FState%5FFinal%5F90%2D21%2F1%2EEnergy%2F2021%20Calculations&FolderCTID=0x012000F803AFF28AED384BBFB1EA0D0AEFED57
