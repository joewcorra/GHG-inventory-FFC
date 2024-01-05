# Calculate State-Level CO2 Emissions
# Master Script

# Link to state spreadsheet
# https://gcc02.safelinks.protection.outlook.com/ap/x-59584e83/?url=https%3A%2F%2Fusepa-my.sharepoint.com%2F%3Ax%3A%2Fg%2Fpersonal%2Fcamobreco_vincent_epa_gov%2FEVIug-LIAexMlEamBhSekvgB0MO3Ma7vR7TPsTOXGmDuSg&data=05%7C01%7CCorra.Joseph%40epa.gov%7C91d1f1e49a054b67b7a608dbf12d5163%7C88b378b367484867acf976aacbeca6a7%7C0%7C0%7C638368949162806888%7CUnknown%7CTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7C3000%7C%7C%7C&sdata=9Vl3GVNYfybQfCJErIc2ddiIcHY96qnUcDp7ugZCJDs%3D&reserved=0


source("emissions_libraries.R")
# data retrieval scripts here (API, etc.)?
source("msn_descriptions.R")
source("emissions_state_step_1.R")
source("emissions_state_step_2.R")
# "emissions_state_step_3.R" No step 3 for state
source("emissions_state_step_4.R")
source("emissions_state_step_5.R")
source("emissions_state_step_6.R")
source("emissions_state_step_7.R")
source("emissions_state_step_8.R")
source("emissions_state_step_9.R")
# "emissions_state_step_10.R" No step 10 for state
source("emissions_state_final") # need a final script to collate results (?)
# Probably create a set of shared national-state scripts for generating
# Markdown docs, etc. 
# Might exclude the data retrieval scripts from this master script, as we
# won't want to download data every time




sources <- c("FFC", "NEU")
# NEU subsources: transportation, industrial, or us territories only.
# FFC subsources: residential, commercial, industrial, transportation, or
# electricity generation.
subsources <- c("residential", "commercial", "industrial", "transportation",
                "electricity generation", "us territories")
fuels <- c("coal", "natural gas", "petroleum", "geothermal")

years <- as_factor(1990:2024)
# MSNs: 5-character codes. First 2 characters = energy source; 
# 3rd 4th characters = energy activity; 5th character = type of data. 
# There are hundreds of MSNs.

# Data vectors or files to create: 1) sectors; 2) sources; 3) subsources;
# 4) fuels; 5) GHG's; 6) states and territory codes; 7) MSNs; 
# 8) factors, which will vary by source and year; 9) subsource adjustments; 
# 10) ???

# All SEDS data (35 years 1990-2023 inclusive, 54 states, all categories)
# = ~1.4 million rows of data

# Final State Summaries break down by state, source, subsource, year, with subtotals for each source/subsource; fuel = "geothermal" is excluded (?)

# Final Full Dataset will look like this:
tibble(sector = "energy", source = "", subsource = "", fuel = "",
                     state = "", ghg = "CO2", year = "2022", value = 0)

final_full <- crossing(year = years, state = states, fuel = fuels,
                       source = sources, subsource = subsources) %>%
  filter(!(source == "NEU" & subsource %in% c("residential", "commercial", "electricity generation"))) %>%
  filter(!(source == "FFC" & subsource == "us territories")) %>%
  filter(!(fuel == "geothermal" & subsource != "electricity generation")) %>%
  mutate(value = rnorm(row_number()))


final_states <- final_full %>% 
  filter(year == "2021") %>%
  group_by(state) %>% 
  group_split() %>% 
  map(\(.x) gt(.x, rowname_col = "year",
          groupname_col = c("source", "subsource")) %>%
        summary_rows(columns = value, fns = "sum") %>% 
        tab_header(
          title = md("**State Summaries**"),
          subtitle = md("by source and subsource")) %>%
        opt_stylize(style = 1,  color = "cyan",
                    add_row_striping = TRUE)) # This may take a while

final_states[10]

