# Calculate State-Level CO2 Emissions
# NEU Adjustments

print("Adjusting industrial and transportation data for non-energy uses.")

# Objects Created--------------------------------------------------------

# List of objects created in the global environment:

# Notes on NEU Adjustments-----------------------------------------------

# Adjustments for the following NEU sources are compiled in this script:
# INDUSTRIAL: other coal, natural gas, distillate fuel, LPG, 
# pentanes plus, petroleum coke, still gas


# The following sources have already been compiled in the industry or 
# transportation scripts, so they are not compiled here. 
# For these sources, we assume 100% of consumption is for non-energy uses. 
# 100% NEU: asphalt & road oil, coking coal, lubricants (both industrial and 
# transportation), naphtha, other oil, special naphtha, waxes, misc products. 
# For these sources, 100% NEU adjustments are made in state_breakouts.R.

# NEU Adjustments--------------------------------------------------------




# Break SEDS data into list based on MSNs
seds_neu_adjusted <- lst(
  
  # Other coal
  # NEU Correction applies to Tennessee only (Eastman Gas Plant)
  other_coal = neu_corrections %>%
    filter(source_description == "other coal", 
           sector_description == "industrial sector") %>%
   mutate(neu_adjusted_value = neu_factor,
          # Add industrial other coal MSN
          msn = "CLOCB", 
          # Tennessee only
          state = "TN"),
  
  # Natural gas
  natural_gas = neu_corrections %>%
    # Natural gas has a very long source name in the NEU data
    filter(str_detect(source_description, "natural gas")) %>%
    left_join(petrochemicals_distribution, by = "year") %>%
    # NEU value = NEU factor * distribution (will be zero for most states)
    mutate(neu_adjusted_value = neu_factor * petrochemical_percent, 
           # Standardize MSN & source to match seds_ind_adjusted
           msn = "net natural gas", 
           source_description = "natural gas"), 
  
  # Distillate fuel
  distillate_fuel = seds_ind_adjusted %>% 
    filter(msn == "DFICB") %>%
    # Join with NEU corrections data
    left_join(neu_corrections,
              by = c("year", "source_description", "sector_description")) %>%
    mutate(neu_adjusted_value = neu_factor * 
             (distillate_fuel_is_adj / states_sum_value)) %>%
    # These columns no longer needed
    select(-distillate_fuel_is_adj, -adjusted_value),
    
    # LPG
    lpg = seds %>%
    filter(msn %in% c("HLICB", "PPICB")) %>%
    # Subtract pentanes plus from HGL
    mutate(value = abs(diff(value)), .by = c(state, year)) %>%
    # Group_size shows that each group has exactly two rows. Good!
    # Pentanes plus computed below, so remove from this element:
    filter(msn != "PPICB") %>%
    # Change source description to reflect new value
    mutate(source_description = "hgl") %>%
    # Join with neu corrections to get neu factor
    left_join(neu_corrections,
              by = c("year", "source_description", "sector_description")) %>%
    # Get sum of all states' lpg
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Rename MSN and calculate adjusted value
    mutate(msn = "net lpg", 
           neu_adjusted_value = neu_factor * (value / states_sum_value)),
  
  # Pentanes plus
  pentanes_plus = seds %>%
    filter(msn == "PPICB") %>%
    # Join with neu corrections to get neu factor
    left_join(neu_corrections,
              by = c("year", "source_description", "sector_description")) %>%
    # Get sum of all states' pentanes plus
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Calculate adjusted value
    mutate(neu_adjusted_value = neu_factor * (value / states_sum_value)),
  
  # Petroleum coke
  petroleum_coke = seds %>%
    filter(msn == "PCICB") %>%
    # Join with neu corrections to get neu factor
    left_join(neu_corrections,
              by = c("year", "source_description", "sector_description")) %>%
    # Get sum of all states' petroleum coke
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Calculate adjusted value
    mutate(neu_adjusted_value = neu_factor * (value / states_sum_value)),
    
    # Still gas
    still_gas = seds %>%
    filter(msn == "SGICB") %>%
    # Join with neu corrections to get neu factor
    left_join(neu_corrections,
              by = c("year", "source_description", "sector_description")) %>%
    # Get sum of all states' still gas
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Calculate adjusted value
    mutate(neu_adjusted_value = neu_factor * (value / states_sum_value))) %>%
  
  # Collapse list into a single data frame
  list_rbind() %>%
  # Remove nonessential columns to simplify joins in state_breakouts.R
  select(sector_description:year, neu_adjusted_value, state, msn)

