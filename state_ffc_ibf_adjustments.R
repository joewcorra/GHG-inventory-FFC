# Calculate State-Level CO2 Emissions
# IBF Adjustments

print("Performing adjustments for international bunker fuels.")

# Objects Created--------------------------------------------------------

# List of objects created in the global environment:


# Notes on IBF Adjustments-----------------------------------------------

# In the state breakouts, IBF adjusted values are subtracted from the 
# adjusted values of the following sources: 
# TRANSPORTATION: distillate fuel, jet fuel, residual fuel

# IBF--------------------------------------------------------------------



# Break SEDS data into list based on MSNs
seds_ibf_adjusted <- lst(
  
  distillate_fuel = foks_diesel_distribution %>%
    left_join(ibf_corrections %>% filter(
      source_description == "distillate fuel oil"), 
      by = "year") %>% 
     # Calculate adjusted value (factor * percent)
    mutate(ibf_adjusted_value = ibf_value * diesel_percent,
           # Add the MSN & sector  for transportation distillate fuel
           msn = "DFACB", 
           sector_description = "transportation sector"),
  
  residual_fuel = foks_residual_distribution %>%
    left_join(ibf_corrections %>% filter(
      source_description == "residual fuel oil"), 
      by = "year") %>% 
    # Calculate adjusted value (factor * percent)
    mutate(ibf_adjusted_value = ibf_value * residual_percent, 
           # Add the MSN & sector for transportation residual fuel
           msn = "RFACB", 
           sector_description = "transportation sector"),
  
  jet_fuel = seds %>%
    filter(msn == "JFACB") %>%
    left_join(ibf_corrections,
      by = c("year", "source_description")) %>% 
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(ibf_adjusted_value = ibf_value * 
             (value / states_sum_value))) %>%
  
  # Collapse list into a single data frame
  list_rbind() %>%
  # Remove nonessential columns to simplify joins in state_breakouts.R
  select(state, year, sector_description, source_description, 
         msn, ibf_adjusted_value)


# Notes from Review of Excel Workbook------------------------------------

# residual_fuel_factor: FFC CO2 file IBF input; assume percentage from 
# FOKS bunker
# distillate_fuel_factor: FFC CO2 file IBF input; assume percentage from 
# FOKS bunker
# jet_fuel_factor: FFC CO2 file IBF input; assume percentage from SEDS total

# residual_fuel = residual_fuel_factor * mystery percentage

# distillate_fuel = distillate_fuel_factor * mystery percentage

# jet_fuel = state's JFACB btu/1000
# sum_jet_fuel = sum of all states'jet_fuel
# jet_fuel_adj = jet_fuel_factor * (jet_fuel / sum_jet_fuel)
