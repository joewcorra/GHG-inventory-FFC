library(tidyverse)
# assorted functions for cleaning up data frames
library(janitor)
# Read Excel files
library(readxl)
# working with HTML
library(httr)
# working with HTML
library(rvest) 

# Temporary file storage path
local_excel_path <- tempfile(fileext = ".xls")

# URL for SWC data by year and facility
swc_url <- paste0('https://ghgdata.epa.gov/ghgp/service/export?q=&tr=current&ds=E&ryr=2011&cyr=2011&lowE=-20000&highE=23000000&st=&fc=&mc=&rs=ALL&sc=0&is=11&et=&tl=&pn=undefined&ol=0&sl=0&bs=&g1=1&g2=0&g3=0&g4=0&g5=0&g6=0&g7=0&g8=0&g9=0&g10=0&g11=0&g12=0&s1=0&s2=1&s3=0&s4=0&s5=0&s6=0&s7=0&s8=0&s9=0&s10=0&s201=0&s202=0&s203=0&s204=1&s301=0&s302=0&s303=0&s304=0&s305=0&s306=0&s307=0&s401=0&s402=0&s403=0&s404=0&s405=0&s601=0&s602=0&s701=0&s702=0&s703=0&s704=0&s705=0&s706=0&s707=0&s708=0&s709=0&s710=0&s711=0&s801=0&s802=0&s803=0&s804=0&s805=0&s806=0&s807=0&s808=0&s809=0&s810=0&s901=0&s902=0&s903=0&s904=0&s905=0&s906=0&s907=0&s908=0&s909=0&s910=0&s911=0&sf=11001100&allReportingYears=yes&listExport=false')

# Retrieve SWC Excel file data
GET(swc_url, write_disk(local_excel_path, overwrite = TRUE))

# Read from temp file 2023 data
# The top six lines are blank in this worksheet, so we'll skip them 
swc2023 <- read_excel(local_excel_path, sheet = 1, skip = 6) %>%
  # Clean up column names/apply snake-case style. 
  clean_names()

# Read from temp file 2022 data
# The top six lines are blank in this worksheet, so we'll skip them 
swc2022 <- read_excel(local_excel_path, sheet = 2, skip = 6) %>%
  # Clean up column names/apply snake-case style. 
  clean_names()

# Read from temp file 2021 data
# The top six lines are blank in this worksheet, so we'll skip them 
swc2021 <- read_excel(local_excel_path, sheet = 3, skip = 6) %>%
  # Clean up column names/apply snake-case style. 
  clean_names()

# Read from temp file 2020 data
# The top six lines are blank in this worksheet, so we'll skip them 
swc2020 <- read_excel(local_excel_path, sheet = 4, skip = 6) %>%
  # Clean up column names/apply snake-case style. 
  clean_names()

# Read from temp file 2019 data
# The top six lines are blank in this worksheet, so we'll skip them 
swc2019 <- read_excel(local_excel_path, sheet = 5, skip = 6) %>%
  # Clean up column names/apply snake-case style. 
  clean_names()

# Read from temp file 2018 data
# The top six lines are blank in this worksheet, so we'll skip them 
swc2018 <- read_excel(local_excel_path, sheet = 6, skip = 6) %>%
  # Clean up column names/apply snake-case style. 
  clean_names()

# Read from temp file 2017 data
# The top six lines are blank in this worksheet, so we'll skip them 
swc2017 <- read_excel(local_excel_path, sheet = 7, skip = 6) %>%
  # Clean up column names/apply snake-case style. 
  clean_names()

# Read from temp file 2016 data
# The top six lines are blank in this worksheet, so we'll skip them 
swc2016 <- read_excel(local_excel_path, sheet = 8, skip = 6) %>%
  # Clean up column names/apply snake-case style. 
  clean_names()

# Read from temp file 2015 data
# The top six lines are blank in this worksheet, so we'll skip them 
swc2015 <- read_excel(local_excel_path, sheet = 9, skip = 6) %>%
  # Clean up column names/apply snake-case style. 
  clean_names()

# Read from temp file 2014 data
# The top six lines are blank in this worksheet, so we'll skip them 
swc2014 <- read_excel(local_excel_path, sheet = 10, skip = 6) %>%
  # Clean up column names/apply snake-case style. 
  clean_names()

# Read from temp file 2013 data
# The top six lines are blank in this worksheet, so we'll skip them 
swc2013 <- read_excel(local_excel_path, sheet = 11, skip = 6) %>%
  # Clean up column names/apply snake-case style. 
  clean_names()

# Read from temp file 2012 data
# The top six lines are blank in this worksheet, so we'll skip them 
swc2012 <- read_excel(local_excel_path, sheet = 12, skip = 6) %>%
  # Clean up column names/apply snake-case style. 
  clean_names()

# Read from temp file 2011 data
# The top six lines are blank in this worksheet, so we'll skip them 
swc2011 <- read_excel(local_excel_path, sheet = 13, skip = 6) %>%
  # Clean up column names/apply snake-case style. 
  clean_names()

# Combine yearly data into one data file
swc_all_years <- bind_rows(swc2011, swc2012, swc2013, swc2014, swc2015, swc2016, swc2017, swc2018, swc2019, swc2020, swc2021, swc2022, swc2023)

# Read in FFC subpart C unit data from GHGRP and Clean up column names/apply snake-case style
subpart_c_unit_data <- read_excel("data/emissions_by_unit_and_fuel_type_c_d_aa.xlsx", sheet = 1, skip = 6) |>
  clean_names()

# Read in FFC subpart C fuel data from GHGRP, note col types to avoid warnings and Clean up column names/apply snake-case style
subpart_c_fuel_data <- read_excel("data/emissions_by_unit_and_fuel_type_c_d_aa.xlsx", sheet = 2, col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "text", "text", "guess", "guess"), skip = 5) |>
  clean_names()

# filter the fuel data to only MSW
fuel_pivot <- filter(subpart_c_fuel_data, specific_fuel_type == "Municipal Solid Waste")

# Define emission factors to get data into mmBtu of MSW (factors are kg / mmBtu)
msw_ch4_ef <- 0.032
msw_n2o_ef <- 0.0042
ch4_gwp <- 25
n2o_gwp <- 298

# Calculate MMBtu of MSW combusted based on CH4 and N2O emissions
fuel_pivot <- mutate(fuel_pivot, ch4_mmbtu = fuel_methane_ch4_emissions_mt_co2e / ch4_gwp * 1000 / msw_ch4_ef)
fuel_pivot <- mutate(fuel_pivot, n2o_mmbtu = fuel_nitrous_oxide_n2o_emissions_mt_co2e / n2o_gwp * 1000 / msw_n2o_ef)

# Define factor to get MMBtu of MSW into short tons
msw_hhv <- 9.95

# Calculate short tons based on MMBtu values (take average)  
fuel_pivot <- fuel_pivot |> rowwise() |> mutate(msw_short_tons = mean(c(ch4_mmbtu, n2o_mmbtu)) / msw_hhv)

# Create summary of state MSW sort tons and pivot to wide format
state_summary <- fuel_pivot |> group_by(state, reporting_year) |> summarize(short_tons = sum(msw_short_tons)/1000)
state_summary <- state_summary |> pivot_wider (names_from = reporting_year, values_from = short_tons)

# Create a table of all state names
state_names <- data.frame("state" = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",	"HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY"))

# Add all states to state MSW short ton summary
state_summary <- left_join(state_names, state_summary, by = "state")

# Export state MSW short ton data to compare with existing approach
write_csv(state_summary, "state_data.csv")

# Add/create CO2 data by facility and unit
msw_co2_data <- left_join(fuel_pivot, subpart_c_unit_data, by = c("reporting_year", "facility_id", "unit_name", "state"))

# Clean up CO2 data file to get rid of unneeded columns and arrange by state
msw_co2_data <- msw_co2_data |> select(state, facility_id, unit_name, reporting_year, unit_co2_emissions_non_biogenic, unit_biogenic_co2_emissions_metric_tons)
msw_co2_data <- msw_co2_data |> arrange(state)

# Combine CO2 data by facility
msw_co2_data_facility <- msw_co2_data |> group_by(state, facility_id, reporting_year) |> summarize(unit_co2_emissions_non_biogenic = sum(unit_co2_emissions_non_biogenic), unit_biogenic_co2_emissions_metric_tons = sum(unit_biogenic_co2_emissions_metric_tons))

# Add CO2 data to all year facility list
swc_all_years_data <- left_join(swc_all_years, msw_co2_data_facility, by = c("reporting_year", "ghgrp_id"="facility_id", "state"))

# Combine short ton data by facility
fuel_pivot_facility <- fuel_pivot |> group_by(state, facility_id, reporting_year) |> summarize(msw_short_tons = sum(msw_short_tons))

# Add short ton data to all year facility list
swc_all_years_data <- left_join(swc_all_years_data, fuel_pivot_facility, by = c("reporting_year", "ghgrp_id"="facility_id", "state"))

# Create summary data for all years used for calculations 
data_for_years <- swc_all_years_data |>
  group_by(reporting_year) |>
  summarize(
    flight_co2 = sum(ghg_quantity_metric_tons_co2e, na.rm = TRUE),
    calc_co2 = sum(unit_co2_emissions_non_biogenic, na.rm = TRUE),
    calc_bio_co2 = sum(unit_biogenic_co2_emissions_metric_tons, na.rm = TRUE),
    calc_short_tons = sum(msw_short_tons, na.rm = TRUE)
    ) 

# Create total ton of MSW data for all years used for calculations 
calc_ton_data <- fuel_pivot_facility |> group_by(reporting_year) |> summarize(calc_total_tons = sum(msw_short_tons))

# Combine data needed for calculations
results <- left_join(data_for_years, calc_ton_data, by = "reporting_year")

# Add result calculations
results <- results |> mutate(fossil_co2 = flight_co2/calc_short_tons * calc_total_tons, biomass_co2 = calc_bio_co2/calc_short_tons * calc_total_tons)

