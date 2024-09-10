# IBF CORRECTIONS DATA

# Calculate State-Level CO2 Emissions
# International Bunker Fuels Data

# Note: Source for original data is: 
# 'International Bunker Fuels 90-21_12-02-22_PR.xls'. Until we implement
# data retrieval scripts, we will simply pull data from the FFC workbook.

print("Retrieving IBF data for for transportation adjustments.")

# Objects Created--------------------------------------------------------



# List of objects created in the global environment:

# Read Excel Data--------------------------------------------------------

# Read in IBF data from FFC excel workbook. Only need one line:
ibf_corrections <- read_excel("data/national_inventory_CO2_data.xlsx", 
                                   sheet = "International Bunker Fuels", 
                                   skip = 0, range = "C7:AJ10") %>%
  clean_names() %>%
  # Make data long; i.e., one row per year
  pivot_longer(cols = -1, names_to = "year", values_to = "ibf_value") %>%
  # Rename source column
  rename(source_description = gas_mode_and_fuel_type) %>%
  # Remove letters from 'year' column and standardize source descriptions
  mutate(year = str_remove(year, "[a-z]"), 
         source_description = case_when(
           str_detect(source_description, "viation") ~ "jet fuel", 
           str_detect(source_description, "istillate") ~ "distillate fuel oil",
           str_detect(source_description, "esidual") ~ "residual fuel oil"))


# Cleanup-----------------------------------------------------------------

# Append the new tibble to the existing 'corrections' list
corrections <- append(corrections, lst(ibf_corrections))

rm(ibf_corrections)
