# Calculate State-Level CO2 Emissions
# Commercial Adjustments


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:

# seds_com_adjusted: tibble; adjusted values for commercial sector emissions

## Commercial Adjustments-------------------------------------------------

# coal (CLCCB), natural gas (NGCCB, SFCCB), distillate fuel (DFCCB), 
# gasoline (MGCCB, EMCCB)
# Not adjusted: kerosene (KSCCB), petroleum coke (PCCCB), 
# residual fuel (RFCCB), lpg = hgl (HLCCB) + propane (PQCCB)

# Break SEDS data into list based on MSNs
seds_com_adjusted <- lst(
  
  coal = seds %>%
    filter(msn == "CLCCB") %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(adjustments %>% 
                filter(sector_description == "commercial"), 
              by = c("source_description", "year")) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(adjusted_value = adjustment_factor * 
             (value / states_sum_value)), 
  
  distillate_fuel = seds %>%
    filter(msn == "DFCCB") %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(adjustments %>% 
                filter(sector_description == "commercial"), 
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
                filter(sector_description == "commercial"), 
              by = c("source_description", "year")) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(adjusted_value = adjustment_factor * 
             (value / states_sum_value)) %>%
    # Change MSN identifier. old MSN distinction no longer needed(?)
    # However, MSN can be reconstituted from other _code fields if needed.
    mutate(msn = "net_natural_gas"),
  
  gasoline = seds %>% 
    # Separate list element required to find net gasoline
    filter(msn %in% c("MGCCB", "EMCCB")) %>%
    # Subtract ethanol from total gasoline
    mutate(value = abs(diff(value)), .by = c(state, year)) %>%
    # Group_size shows that each group has exactly two rows. Good!
    # Ethanol no longer needed (and value is now duplicative)
    filter(msn != "EMCCB") %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(adjustments %>% 
                filter(sector_description == "commercial"), 
              by = c("source_description", "year")) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(adjusted_value = adjustment_factor * 
             (value / states_sum_value)) %>%
    # Change MSN identifier. old MSN distinction no longer needed(?)
    # However, MSN can be reconstituted from other _code fields if needed.
    mutate(msn = "net_gasoline"),

  # All other sources go in the last list element
  other_commercial = seds %>% 
    filter(msn %in% c("KSCCB", "PCCCB", "RFCCB", "HLCCB", "PQCCB")) %>%
    # Adjusted = original value. Consider using a different variable name here
    mutate(adjusted_value = value)) %>%
  
  # Collapse list into a single data frame
  list_rbind()
