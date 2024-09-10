# Calculate State-Level CO2 Emissions

# Step 1: Total Fuel Consumption by Fuel Type and Sector


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:

# seds: tibble; all SEDS data



# Read API-derived SEDS data---------------------------------------------

# Alternatively, read SEDS data from EIA API file pulled with epa_api.R
seds <- read_csv("data/api_seds.csv") %>%
  clean_names() %>%
  select( state = state_id, year = period, msn = series_id, value, unit) %>%
  filter(unit == "Billion Btu") %>%
  filter(msn %in% msn_names$msn_lookup) %>%
  mutate(unit = str_to_lower(unit), 
         year = as.character(year)) %>%
  left_join(msn_names$msn %>% select(-unit), by = "msn") %>%
  # Remove any duplicates caused by appending new annual data
  distinct() %>%
  # Convert to millions of BTUs
  mutate(value = value / 1000) 









