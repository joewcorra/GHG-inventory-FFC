# Calculate State-Level CO2 Emissions
# Master Script

# Sharepoint Link:
# https://usepa.sharepoint.com/sites/U.S._GHG_Inventory_Report/Shared%20Documents/Forms/AllItems.aspx?csf=1&web=1&e=QHgRM5&cid=7ef871ac%2Df5c1%2D4767%2Dafc3%2D906ccfaa5764&RootFolder=%2Fsites%2FU%2ES%2E%5FGHG%5FInventory%5FReport%2FShared%20Documents%2FReports%2FState%5FGHGI%5F90%2D21%2FState%5FFinal%5F90%2D21%2F1%2EEnergy%2F2021%20Calculations&FolderCTID=0x012000F803AFF28AED384BBFB1EA0D0AEFED57



source("emissions_libraries.R")
# data retrieval scripts here (API, etc.)?
source("msn_descriptions.R")
source("read_seds_data.R")
source("national_corrections_data.R")
source("ibf_corrections_data.R")
source("neu_corrections_data.R")
source("ippu_distibution_data.R")
source("residential_adjustments.R")
source("commercial_adjustments.R")
source("industrial_adjustments.R")
source("transportation_adjustments.R")
source("electrical_power_adjustments.R")
source("ibf_adjustments.R")
source("neu_adjustments.R")
source("carbon_factors_data.R")
source("state_breakouts.R")
source("final_state_summaries.R")
source("emissions_state_final") # need a final script to collate results (?)
# Probably create a set of shared national-state scripts for generating
# Markdown docs, etc. 
# Might exclude the data retrieval scripts from this master script, as we
# won't want to download data every time
