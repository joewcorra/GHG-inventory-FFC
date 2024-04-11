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
    filter(msn == "DFACB"), 
  
  # Jet Fuel (IBF adjustment)
  jet_fuel = us_consumption %>%
    filter(msn == "JFACB"),
  
  # LPG (Propane) AKA HGL
  lpg = us_consumption %>%
    filter(msn == "HLACB"),
  
  # Motor Gasoline (mogas/df adjustment)
  aviation_gasoline = us_consumption %>%
    filter(msn == "MGACB"),
  
  # Residual Fuel (IBF adjustment)
  aviation_gasoline = us_consumption %>%
    filter(msn == "RFACB")) %>%
  
  # Collapse list into a single data frame
  list_rbind()
  