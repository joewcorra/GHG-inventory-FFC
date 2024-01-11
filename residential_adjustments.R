# Calculate State-Level CO2 Emissions
# Residential Adjustments


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:
# seds_res_adjusted: tibble; adjusted values for residential sector emissions

# Residential Adjustments-------------------------------------------------

# coal (CLRCB), natural gas (NGRCB, SFRCB), distillate fuel (DFRCB)
# unadjusted: kerosene (KSRCB), lpg = hgl (HLRCB) + propane (PQRCB)

# Break SEDS data into list based on MSNs
seds_res_adjusted <- lst(
  
  coal = seds %>%
    filter(msn == "CLRCB") %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(adjustments %>% 
                filter(sector_description == "residential"), 
              by = c("source_description", "year")) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(adjusted_value = adjustment_factor * 
             (value / states_sum_value)), 
  
  natural_gas = seds %>% 
    # Separate list element required to find net natural gas
    filter(msn %in% c("NGRCB", "SFRCB")) %>%
    # Subtract supplemental gas from total natural gas
    mutate(value = abs(diff(value)), .by = c(state, year)) %>%
    # Group_size shows that each group has exactly two rows. Good!
    # Supplemental gas no longer needed (and value is now duplicative)
    filter(msn != "SFRCB") %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(adjustments %>% 
                filter(sector_description == "residential"), 
              by = c("source_description", "year")) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(adjusted_value = adjustment_factor * 
             (value / states_sum_value)) %>%
  # Change MSN identifier. old MSN distinction no longer needed(?)
  # However, MSN can be reconstituted from other _code fields if needed.
    mutate(msn = "net_natural_gas"),
  
  distillate_fuel = seds %>%
    filter(msn == "DFRCB") %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(adjustments %>% 
                filter(sector_description == "residential"), 
              by = c("source_description", "year")) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(adjusted_value = adjustment_factor * 
             (value / states_sum_value)), 
  
  # All other sources go in the last list element
  other_residential = seds %>% 
    filter(msn %in% c("KSRCB", "HLRCB", "PQRCB")) %>%
    # Adjusted = original value. Consider using a different variable name here
    mutate(adjusted_value = value)) %>%

  # Collapse list into a single data frame
  list_rbind()
