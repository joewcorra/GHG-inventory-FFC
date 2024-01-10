# Calculate State-Level CO2 Emissions
# Commercial Adjustments


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:

# seds_com_adjusted: tibble; adjusted values for commercial sector emissions

## Commercial Adjustments-------------------------------------------------

# CLCCB, NGCCB, SFCCB, DFCCB, MGCCB, EMCCB "commercial sector"
# coal, natural gas, distillate fuel, gasoline

# Break SEDS data into 3-element list based on MSNs
seds_com_adjusted <- list(
  natural_gas = seds %>% 
    # Separate list element required to find net natural gas
    filter(msn %in% c("NGRCB", "SFRCB")) %>%
    # Subtract supplemental gas from total natural gas
    mutate(value = abs(diff(value)), .by = c(state, year)) %>%
    # Group_size shows that each group has exactly two rows. Good!
    # Supplemental gas no longer needed (and value is now duplicative)
    filter(msn != "SFRCB") %>%
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
    # Change MSN identifier. old MSN distinction no longer needed(?)
    # However, MSN can be reconstituted from other _code fields if needed.
    mutate(msn = "net_gasoline"),
  # All non-gas & non-gasoline sources go in the third list element
  no_gas = seds %>% 
    filter(!msn %in% c("NGRCB", "SFRCB", "MGCCB", "EMCCB"))) %>%
  # Map to both list elements
  map(\(.x) filter(.x, sector_code == "CC", state %in% states_and_dc) %>%
        # SEDS values must be corrected to match national data.
        # Join with the adjustment factor data (from national inventory)
        left_join(adjustments %>% 
                    filter(sector_description == "commercial"), 
                  by = c("source_description", "year")) %>%
        # Get sum of all states' value for each source
        mutate(states_sum_value = sum(value), .by = c(msn, year), 
               # Multiply adjustment factor by states's value / the above sum
               adjusted_value = adjustment_factor * 
                 (value / states_sum_value))) %>%
  # Collapse list into a single data frame
  list_rbind()
