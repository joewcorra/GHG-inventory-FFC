# Calculate State-Level CO2 Emissions
# Step 1: Total Fuel Consumption by Fuel Type and Sector


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:


# msn_lookup: vector; all MSNs used in the state summaries
# seds: tibble; all SEDS data



# Read old SEDS data-------------------------------------------------------

# Read in SEDS data from use_all_btu_csv
seds_original <- read_csv("data/use_all_btu.csv") %>%
        clean_names() %>%
  # Change 'year' to a column
  pivot_longer(cols = starts_with("x") , names_to = "year") %>%
  # Remove unneeded columns
  select(-data_status) %>%
  # Get rid of leading 'x' in years
  mutate(year = str_remove(year, "x")) %>%
  # Retain data from 1990 onward
  filter(year > "1989") %>%   
  # Retain rows with MSN matching our msn_lookup data
  filter(msn %in% msn_lookup, 
         # Retain rows with states or DC
         state %in% states_and_dc) %>%
  left_join(msn, by = "msn") %>%
  # Convert to millions of BTUs, round to 2 places
  mutate(value = round(value / 1000, 2)) 

# Read API-derived SEDS data---------------------------------------------

# Alternatively, read SEDS data from EIA API file pulled with epa_api.R
seds <- read_csv("data/api_seds.csv") %>%
  clean_names() %>%
  select( state = state_id, year = period, msn = series_id, value, unit) %>%
  filter(unit == "Billion Btu") %>%
  filter(msn %in% msn_lookup) %>%
  mutate(unit = str_to_lower(unit), 
         year = as.character(year)) %>%
  left_join(msn %>% select(-unit), by = "msn") %>%
  # Remove any duplicates caused by appending new annual data
  distinct()








