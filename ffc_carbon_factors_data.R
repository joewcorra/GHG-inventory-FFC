# Calculate State and National CO2 Emissions
# Carbon Factors

# Objects Created--------------------------------------------------------

# Ratio of the molecular weight of carbon dioxide to carbon
carbon_ratio = 44/12

# List of objects created in the global environment:

# carbon_factors: tibble; factors for carbon content of fuels

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

# Cleanup------------------------------------------------------------------

rm(carbon_factors_variable)



# In work: Computing the Carbon Coefficients------------------------------

# Mogas and Dist fuel
  # Source: Motor Gasoline and distillate fuel oil carbon contents from EPA 
 # (2020) Memo: "Updated Gasoline and Diesel Fuel CO2 Emission Factors". 

# Natural Gas
# =((J31*$J$74)+$J$73)/J31*1000
# J31 = nat gas annually variable heat content
# J74 = X Variable 1 Source: Natural Gas Carbon Content Updates_3_5_20.xls
# J73 = Intercept Source: Natural Gas Carbon Content Updates_3_5_20.xls

# Residential Coal
# [Annually Variable C Contents_Coal_12-12-2023.xls]Summary Coal'

# Commercial Coal
# [Annually Variable C Contents_Coal_12-12-2023.xls]Summary Coal'

# Industrial Other Coal
# [Annually Variable C Contents_Coal_12-12-2023.xls]Summary Coal'

# Industrial Coking Coal
# [Annually Variable C Contents_Coal_12-12-2023.xls]Summary Coal'

# Electric Power Coal
# [Annually Variable C Contents_Coal_12-12-2023.xls]Summary Coal'

# LPG (Propane)
# [HGL Factors_Update Methodology_12_12_23.xlsx]Weighted Factors'

# HGL (Energy Use)
# [HGL Factors_Update Methodology_12_12_23.xlsx]Weighted Factors'

# HGL (Non-Energy Use)
# [HGL Factors_Update Methodology_12_12_23.xlsx]Weighted Factors'

# Jet Fuel
# hard-coded into 'Factors' sheet

# MoGas Blend Components
# hard-coded into 'Factors' sheet

# Misc. Products
# hard-coded into 'Factors' sheet

# Unfinished Oils
# hard-coded into 'Factors' sheet

# Crude Oil
# hard-coded into 'Factors' sheet




