# Calculate National-Level CO2 Emissions
# Master Script

source("emissions_libraries.R")
# data retrieval scripts here (API, etc.)?
source("emissions_national_step_1.R")
source("emissions_national_step_2.R")
source("emissions_national_step_3.R")
source("emissions_national_step_4.R")
source("emissions_national_step_5.R")
source("emissions_national_step_6.R")
source("emissions_national_step_7.R")
source("emissions_national_step_8.R")
source("emissions_national_step_9.R")
source("emissions_national_step_10.R")
source("emissions_national_final") # need a final script to collate results (?)
# Probably create a set of shared national-state scripts for generating
# Markdown docs, etc. 
# Might exclude the data retrieval scripts from this master script, as we
# won't want to download data every time
