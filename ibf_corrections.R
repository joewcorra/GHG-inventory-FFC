# IBF CORRECTIONS

# Calculate State-Level CO2 Emissions
# International Bunker Fuels Data

# Note: Source for original data is: 
# 'International Bunker Fuels 90-21_12-02-22_PR.xls'. Until we implement
# data retrieval scripts, we will simply pull data from the FFC workbook.

# Objects Created--------------------------------------------------------



# List of objects created in the global environment:

# Read Excel Data--------------------------------------------------------

# Read in IBF data from FFC excel workbook. Only need one line:
ibf_corrections <- read_excel("national_inventory_CO2_data.xlsx", 
                                   sheet = "International Bunker Fuels", 
                                   skip = 0, range = "C7:AI8") %>%
  clean_names() %>%
  # Make data long; i.e., one row per year
  pivot_longer(cols = -1, names_to = "year", values_to = "ibf_factor") %>%
  # Remove letters from 'year' column
  mutate(year = str_remove(year, "[a-z]"))
