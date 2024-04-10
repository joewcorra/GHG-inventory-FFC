power_user <- read_csv("power_user.csv") %>%
  clean_names() %>%
  filter(state == "NY", econ_sector == "Transportation") %>%
  select(econ_source, ghg, starts_with("y")) %>%
  pivot_longer(cols = starts_with("y"), names_to = "year", values_to = "value") %>% 
  # mutate(econ_source = case_when(
  #   econ_source == "Rice Cultivation" ~ "Crop cultivation",
  #   econ_source == "CO2 from Fossil Fuel Combustion" ~ "Fuel combustion",
  #   econ_source == "Field Burning of Agricultural Residues" ~ "Crop cultivation",
  #   econ_source == "Liming" ~ "Crop cultivation",
  #   econ_source == "Manure Management" ~ "Livestock",
  #   econ_source == "Mobile Combustion" ~ "Fuel combustion",
  #   econ_source == "N2O from Agricultural Soil Management" ~ "Crop cultivation",
  #   econ_source == "Stationary Combustion" ~ "Fuel combustion",
  #   econ_source == "Urea Fertilization" ~ "Crop cultivation",
  #   econ_source == "Enteric Fermentation" ~ "Livestock",
  #   .default = econ_source
  # )) %>%
  group_by(year) %>% 
  summarise(total = sum(value, na.rm = TRUE)) %>% 
  pivot_wider(names_from = year, values_from = total)
