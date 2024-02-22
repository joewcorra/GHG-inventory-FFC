# Calculate State-Level CO2 Emissions
# National Corrections Data

# Objects Created--------------------------------------------------------



# List of objects created in the global environment:

# Read Excel Data--------------------------------------------------------

# This is excessively complex! Source file needs to be formatted differently


national_corrections <- read_excel("national_inventory_CO2_data.xlsx", 
                                   sheet = "Corrections", 
                                   skip = 0, range = "b5:AH60",
                                   col_names = FALSE) %>%
  clean_names() %>%
  rename(categories = x1) %>%
  filter(!is.na(x2)) %>%
  mutate(categories = case_when(
    categories == "(TBtu)" ~ "year", 
    is.na(categories) ~ "ammonia_natural_gas",
    .default = categories %>% 
      str_to_lower() %>% 
      str_replace_all(" ", "_") %>%
      str_remove_all("\\(|\\)|\\.|>"))) %>%
  distinct(categories, .keep_all = TRUE) %>%
  mutate(across(starts_with("x"), ~as.numeric(.))) %>% 
  pivot_longer(cols = -1) %>%
 pivot_wider(names_from = categories) %>%
  mutate(year = as.character(year)) %>%
  select(-name) %>%
   rename(
       trans_mogas_ethanol_factor = transportation,
       ind_mogas_ethanol_factor = industrial,
        com_mogas_ethanol_factor = commercial,
     sng_correction = dakota_gas,
     # eastman gas isn't used?
     nat_gas_ammonia_factor = ammonia_natural_gas, 
     blast_furnace_gas_factor = blast_furnace_gas, 
     coke_oven_gas_factor = coke_oven_gas, 
     ippu = coking_coal, 
     #    industrial_other_coal  , ??????
     #    aluminum  , # petroleum coke corrections
     #    ferroalloys  , # petroleum coke corrections
     #    titanium_dioxide  , # petroleum coke corrections
     #    ammonia  =  ammonia_factor , # petroleum coke corrections
     #    silicon_carbide_petroleum_coke  , # petroleum coke corrections
     #    other_oil_401_deg_f  , # carbon black corrections
     cb_factor = residual_fuel, 
     is_gas_factor = natural_gas, 
     is_distillate_fuel_factor = distillate_fuel, 
     is_coal_factor = coal)


# Consumption input data is largely similar to the adjustments data from 
# 'US compare', with the exception of resid fuel, dist fuel, nat gas, & coal.
# Unclear how those 4 sources were adjusted between the two data sets. 

consumption_input <- read_excel("national_inventory_CO2_data.xlsx", 
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
