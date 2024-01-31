# Calculate State-Level CO2 Emissions
# Carbon Factors

# Objects Created--------------------------------------------------------

# Ratio of the molecular weight of carbon dioxide to carbon
carbon_ratio = 44/12

# List of objects created in the global environment:

#I12:AO28
# Read Excel Data-----------------------------------------------

# Read in variable carbon factors data from FFC excel workbook
carbon_factors_variable <- read_excel("national_inventory_CO2_data.xlsx", 
                                      sheet = "Factors", 
                                      skip = 0, range = "I12:AO28") %>%
  clean_names() %>%
  rename(source_description = fuel_type) %>%
  # Make sources lowercase and standardize sources
  mutate(source_description = str_to_lower(source_description), 
         source_description = case_when(
           source_description == "lpg (propane)" ~ "lpg",
           .default = source_description))


# Read in carbon factors data from FFC excel workbook
carbon_factors <- read_excel("national_inventory_CO2_data.xlsx", 
                              sheet = "Factors", 
                              skip = 0, range = "B9:D58") %>%
  clean_names() %>%
  # Remove middle column, rename other columns
  select(source_description = coal, carbon_factor = x3) %>%
  # NA = not applicable, NC = not calculated. Remove all NA & NC
  filter(!is.na(carbon_factor), 
         carbon_factor != "NC", 
         # Remove territories for now
         !str_detect(source_description, "erritor")) %>%
  # Make sources lowercase and standardize sources
  mutate(source_description = str_to_lower(source_description), 
         source_description = case_when(
           source_description == "naphtha (<401 deg. f)" ~ "naphtha", 
           source_description == "other oil (>401 deg. f)" ~ "other oils",
           source_description == "lpg (propane)" ~ "lpg",
           source_description == "jet fuel (kerosene)" ~ "jet fuel", 
           str_detect(source_description, "utility coal") ~ "electric power coal", 
           .default = source_description)) %>%
  # Join with annually variable carbon factor data
  left_join(carbon_factors_variable, by = "source_description") %>%
  # Copy non-variable factors across all years
  mutate(across(starts_with("x"), 
                ~ifelse(carbon_factor == "variable", ., carbon_factor))) %>%
  # First factor column no longer needed
  select(-carbon_factor) %>%
  # Pivot longer 
  pivot_longer(cols = starts_with("x"), 
               names_to = "year", values_to = "carbon_factor") %>%
  # Remove x and make values numeric
  mutate(year = str_remove(year, "x"),
    carbon_factor = as.numeric(carbon_factor))



# Notes from Excel Workbook----------------------------------------------

# Hard-coded numbers for various sources; differ by year

# Residential: coal, natural gas, distillate fuel, kerosene, LPG (propane)
# Commercial: coal, natural gas, distillate fuel, kerosene, LPG (propane), 
# all identical to 'residential' factors; 
# also motor gasoline, residual fuel, petroleum coke
# Industrial: natural gas, distillate fuel, kerosene, 
# all identical to 'residential' factors; 
# motor gasoline, residual fuel, identical to 'commercial' factors;
# also coking coal, other coal, asphalt, hgl (fuel), hgl (NEU), 
# lubricants, avgas blend, crude oil, mogas blend, misc products, 
# naphtha, other oil, petro coke, still gas, still gas (NEU), 
# special naphtha, unfinished oils, waxes
# Transportation: coal, natural gas, distillate fuel, LPG (propane), 
# all identical to 'residential' factors; 
# motor gasoline, residual fuel, identical to 'commercial' factors; 
# lubricants, identical to 'industrial' factor
# also aviation gas, jet fuel
# Electrical Power: 
# natural gas, distillate fuel identical to 'residential' factors; 
# dist fuel (light) i
# petro coke, identical to 'industrial' factor;
# Also, coal

# NEU Storage: coking coal, other coal, natural gas, asphalt, lpg, 
# ind lubricants, pentanes plus, naphtha, other oil, still gas, 
# petrol gas, special naphtha, dist fuel, waxes, misc products, 
# trans lubricants
# Note: other coal, natural gas, lpg, pentanes plus, other oil, 
# still gas, and special naphtha are all identical

