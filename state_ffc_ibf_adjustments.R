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
  
  distillate_fuel = corrections$foks_diesel_distribution %>%
    left_join(corrections$ibf_corrections %>% filter(
      source_description == "distillate fuel oil"), 
      by = "year") %>% 
     # Calculate adjusted value (factor * percent)
    mutate(ibf_adjusted_value = ibf_value * foks_diesel_percent,
           # Add the MSN & sector  for transportation distillate fuel
           msn = "DFACB", 
           sector_description = "transportation sector"),
  
  residual_fuel = corrections$foks_residual_distribution %>%
    left_join(corrections$ibf_corrections %>% filter(
      source_description == "residual fuel oil"), 
      by = "year") %>% 
    # Calculate adjusted value (factor * percent)
    mutate(ibf_adjusted_value = ibf_value * foks_residual_percent, 
           # Add the MSN & sector for transportation residual fuel
           msn = "RFACB", 
           sector_description = "transportation sector"),
  
  jet_fuel = seds %>%
    filter(msn == "JFACB") %>%
    left_join(corrections$ibf_corrections,
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


# Cleanup=---------------------------------------------------------------

seds_adjusted <- append(seds_adjusted, lst(seds_ibf_adjusted))

rm(seds_ibf_adjusted)

