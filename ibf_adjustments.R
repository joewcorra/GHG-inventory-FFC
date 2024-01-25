# Calculate State-Level CO2 Emissions
# IBF Adjustments


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:


# Notes on IBF Adjustments-----------------------------------------------

# In the state breakouts, IBF adjusted values are subtracted from the 
# adjusted values of the following sources: 
# TRANSPORTATION: distillate fuel, jet fuel, residual fuel

# IBF--------------------------------------------------------------------



# Break SEDS data into list based on MSNs
seds_ibf_adjusted <- lst(
  
  distillate_fuel = adjustments %>% 
    filter(sector_description == "transportation", 
           source_description == "distillate fuel oil") %>%
    mutate(ibf_value = adjustment_factor * 1), # mystery % 
  # the mystery %s have the state code data
  
  residual_fuel = adjustments %>% 
    filter(sector_description == "transportation", 
           source_description == "residual fuel") %>%
    mutate(ibf_value = adjustment_factor * 1), # mystery % 
  # the mystery %s have the state code data
  
  jet_fuel = seds %>%
    filter(msn == "JFACB") %>%
    left_join(ibf_corrections %>% select(-gas_mode_and_fuel_type), 
              by = "year") %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(ibf_value = ibf_factor * 
             (value / states_sum_value)))
  
  # Collapse list into a single data frame
  list_rbind()




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
