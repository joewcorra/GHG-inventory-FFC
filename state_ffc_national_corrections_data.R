# Calculate State-Level CO2 Emissions
# National Corrections Data

print("Retrieving national FFC data required to adjust SEDS data.")

# Objects Created--------------------------------------------------------



# List of objects created in the global environment:

# adjustments: tibble; adjustment factors derived from national data (i think?)

# Read Excel Data--------------------------------------------------------

# Read in adjustment factors data, derived from national inventory
adjustments <- read_csv("data/us_compare.csv") %>%
  clean_names() %>%
  # Change 'year' to a column
  pivot_longer(cols = starts_with("x"), 
               names_to = "year", values_to = "national_value") %>%
  # Get rid of leading 'x' in years
  mutate(year = str_remove(year, "x"), 
         # Standardize sector descriptions   
         sector_description = str_c(sector_description, " sector"), 
         # Standardize source descriptions
         source_description = if_else(
           source_description == "hydrocarbon gas liquids", 
           "hgl", source_description)) 
# This is excessively complex! Source file needs to be formatted differently


national_corrections <- read_excel("data/national_inventory_CO2_data.xlsx", 
                                   sheet = "Corrections", 
                                   skip = 0, range = "B5:AI60",
                                   col_names = FALSE) %>%
  clean_names() %>%
  rename(categories = x1) %>%
  filter(!is.na(x2)) %>%
  mutate(categories = case_when(
    categories == "(TBtu)" ~ "year", 
    .default = categories %>% 
      str_to_lower() %>% 
      str_replace_all(" ", "_") %>%
      str_remove_all("\\(|\\)|\\.|>"))) %>%
  distinct(categories, .keep_all = TRUE) %>%
  mutate(across(starts_with("x"), ~as.numeric(.))) %>% 
  pivot_longer(cols = -1) %>%
 pivot_wider(names_from = categories) %>%
  mutate(year = as.character(year)) %>%
  select(year, sng_correction = dakota_gas,
     nat_gas_ammonia_factor = ammonia_production,
     ippu = coking_coal, 
     cb_factor = residual_fuel, 
     is_gas_factor = natural_gas, 
     is_distillate_fuel_factor = distillate_fuel, 
     is_coal_factor = coal)


# Consumption input is the 'US compare' data with corrections factors applied. 
# It applies only to industrial coal, nat gas, resid fuel, & dist fuel.


consumption_input <- read_excel("data/national_inventory_CO2_data.xlsx", 
                                sheet = "Consumption Input", 
                                skip = 0, range = "C5:AK131") %>%
  clean_names() %>%
  rename(sector_description = t_btu, 
         source_description = x2) %>%
  mutate(sector_description = if_else(
    str_detect(sector_description, "ource"), 
    NA_character_, sector_description), 
    # Standardize source descriptions for joins in industrial_adjustments.R
    source_description = case_when(
      str_detect(source_description, "istillate") ~ "distillate fuel oil", 
      str_detect(source_description, "esidual") ~ "residual fuel oil",
      .default = source_description)) %>%
  fill(sector_description) %>%
  pivot_longer(cols = !c(sector_description, source_description), 
               values_to = "consumption_value", names_to = "year") %>%
  filter(!is.na(source_description), 
         !str_detect(year, "percent")) %>%
  mutate(year = parse_number(year) %>% as.character(), 
         # NAs are okay in the next line; we won't be using those values
         consumption_value = as.numeric(consumption_value),
         source_description = str_to_lower(source_description), 
         sector_description = str_to_lower(sector_description) %>% 
           str_c(" sector")) %>%
  # Only used for ind: resid fuel, dist fuel, nat gas, & coal. Remove others
  filter(sector_description == "industrial sector", 
         source_description %in% c("residual fuel oil", "other coal", 
                                   "distillate fuel oil", "natural gas")) %>%
  # Change other coal = coal for consistent joins in industrial_adjustments.R
  mutate(source_description = 
           if_else(source_description == "other coal", "coal", 
                   source_description))
  
  
  
  
  print("This generates a warning about NA values.")
  print("Ignore this warning. These values are not used.")
