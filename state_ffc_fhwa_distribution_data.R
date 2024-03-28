# FEDERAL HIGHWAY ADMINISTRATION DATA

# FHWA Gasoline and Diesel Fuel Consumption Data


# Objects Created--------------------------------------------------------


# List of objects created in the global environment:
# 

# Used by 


# Read Excel Data--------------------------------------------------------

# Read in gasoline data from FHWA excel workbook
gasoline_distribution <- read_excel(
  "fhwa_mf226_gasoline_2021.xlsx", 
  sheet = 1, 
  skip = 0, range = "B2:AH53") %>%
  clean_names() %>%
  # Rename the state code column
  rename(state = x1) %>%
  # Make data long; i.e., one row per year
  pivot_longer(cols = -1, names_to = "year", 
               values_to = "gasoline_percent") %>%
  # Remove letters from year column 
  mutate(year = str_remove(year, "[a-z]"),
         # Get national total for each year by insta-grouping
         national_total = sum(gasoline_percent), .by = year) %>%
  # Get gasoline percentage for each state 
  mutate(gasoline_percent = gasoline_percent / national_total) %>%
  # No longer need national total
  select(-national_total)

diesel_distribution <- read_excel(
  "fhwa_mf225_diesel_fuel_2021.xlsx", 
  sheet = 1, 
  skip = 0, range = "B2:AH53") %>%
  clean_names() %>%
  # Rename the state code column
  rename(state = x1) %>%
  # Make data long; i.e., one row per year
  pivot_longer(cols = -1, names_to = "year", 
               values_to = "diesel_percent") %>%
  # Remove letters from year column 
  mutate(year = str_remove(year, "[a-z]"),
         # Get national total for each year by insta-grouping
         national_total = sum(diesel_percent), .by = year) %>%
  # Get diesel percentage for each state 
  mutate(diesel_percent = diesel_percent / national_total) %>%
  # No longer need national total
  select(-national_total)
