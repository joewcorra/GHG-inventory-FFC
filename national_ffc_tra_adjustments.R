# Calculate National-Level CO2 Emissions
# Transportation Sector


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:


# Collate Consumption Data------------------------------------------------

us_tra <- lst(
  
  # Lubricants (NEU adjustment)
  lubricants = us_consumption %>%
    filter(msn == "LUACB"),
  
  # Aviation Gasoline
  aviation_gasoline = us_consumption %>%
    filter(msn == "AVACB"),
  
  # Distillate Fuel (IBF adjustment, mogas/df adjustment)
  distillate_fuel = us_consumption %>%
    filter(msn == "DFACB") %>%
    mutate(adjusted_value = value - ibf_dist_fuel_adjustment), 
  
  # Jet Fuel (IBF adjustment)
  jet_fuel = us_consumption %>%
    filter(msn == "JFACB") %>%
    mutate(adjusted_value = value - ibf_jet_fuel_adjustment),
  
  # LPG (Propane) AKA HGL
  lpg = us_consumption %>%
    filter(msn == "HLACB"),
  
  # Motor Gasoline (mogas/df adjustment)
  aviation_gasoline = us_consumption %>%
    filter(msn == "MGACB"),
  
  # Residual Fuel (IBF adjustment)
  residual_fuel = us_consumption %>%
    filter(msn == "RFACB") %>%
    mutate(adjusted_value = value - residual_fuel_adjustment)) %>%
  
  # Collapse list into a single data frame
  list_rbind()
  