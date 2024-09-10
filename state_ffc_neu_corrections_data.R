# NEU CORRECTIONS

# Calculate State-Level CO2 Emissions
# Non-Energy Use

# Note: Source for original data is: 
# 'EIA_outputs_to_EPA 1990-2021_FR.xlsx'. Until we implement
# data retrieval scripts, we will simply pull data from the FFC workbook.


print("Retrieving NEU data for industrial and transportation adjustments.")

# Objects Created--------------------------------------------------------



# List of objects created in the global environment:

# Notes on Non-Energy Fuels----------------------------------------------

# For these fuels, we assume 100% of consumption is for non-energy uses. 
# This is similar (but not exact) to EIA assumptions. 
# 100% NEU: asphalt & road oil, lubricants, naphtha, other oil, special 
# naphtha, waxes, misc products.

# Read Excel Data--------------------------------------------------------

# Read in NEU data from FFC excel workbook
neu_corrections <- read_excel("data/national_inventory_CO2_data.xlsx", 
                              sheet = "Non-Energy Use", 
                              skip = 0, range = "D5:AK24") %>%
  clean_names() %>%
  # Rename to match column names in SEDS
  rename(source_description = sector_fuel_type) %>%
  # Add sector description based on source_description text
  mutate(sector_description = case_when(
    source_description == "Transportation" ~ "transportation sector",
    source_description == "Industry" ~ "industrial sector",
    .default = NA_character_)) %>%
  # Fill sector_description empty values from previous entry
  fill(sector_description, .direction = "down") %>%
  # Move sector_description to the first column in order to pivot
  relocate(sector_description) %>%
  # Make data long; i.e., one row per year
  pivot_longer(cols = -c(1, 2), names_to = "year", 
               values_to = "neu_factor") %>%
  # Remove letters from year column 
  mutate(year = str_remove(year, "[a-z]"), 
         # Make source lowercase
         source_description = str_to_lower(source_description) %>% 
           # Remove asterisks and the word "industrial" from source
           str_remove_all("\\*|industrial") %>% 
           # Remove extra spaces from source
           str_squish()) 


# Cleanup-----------------------------------------------------------------

# Append the new tibble to the existing 'corrections' list
corrections <- append(corrections, lst(neu_corrections))

rm(neu_corrections)
