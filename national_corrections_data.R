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
     sng_factor = dakota_gas,
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





