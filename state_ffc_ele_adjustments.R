# Calculate State-Level CO2 Emissions
# Electric Power Adjustments# 

print("Performing adjustments to electric power sector consumption.")

# Objects Created--------------------------------------------------------

# List of objects created in the global environment:


## Electric Power Adjustments-----------------------------------------


# coal (CLEIB), net nat gas = natural gas (NGEIB) - natural gas supp (SFEIB),
# distillate fuel (DFEIB)

# Break SEDS data into list based on MSNs
seds_ele_adjusted <- lst(
  
  coal = seds %>%
    filter(msn == "CLEIB") %>%
    # Make sector names match to complete the next join
    mutate(sector_description = word(sector_description, 
                                     start = 1, end = 3)) %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(adjustments, 
              by = c("source_description", "year", "sector_description")) %>%
    # Rename for clarity
    rename(coal_factor = national_value) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(adjusted_value = coal_factor * 
             (value / states_sum_value)), 
  
  natural_gas = seds %>% 
    # find net natural gas
    filter(msn %in% c("NGEIB", "SFEIB")) %>%
    # Subtract supplemental gas from total natural gas
    mutate(value = abs(diff(value)), .by = c(state, year)) %>%
    # Group_size shows that each group has exactly two rows. Good!
    # Supplemental gas no longer needed (and value is now duplicative)
    filter(msn != "SFEIB") %>%
    # Make sector names match to complete the next join
    mutate(sector_description = word(sector_description, 
                                     start = 1, end = 3)) %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(adjustments, 
              by = c("source_description", "year", "sector_description")) %>%
    # Rename for clarity
    rename(natural_gas_factor = national_value) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(adjusted_value = natural_gas_factor * 
             (value / states_sum_value)) %>%
    # Change MSN identifier. old MSN distinction no longer needed(?)
    # However, MSN can be reconstituted from other _code fields if needed.
    mutate(msn = "net natural gas", 
           source_description = "natural gas"),
  
  residual_fuel = seds %>%
    filter(msn == "RFEIB") %>%
    # Make sector names match to complete the next join
    mutate(sector_description = word(sector_description, 
                                     start = 1, end = 3)) %>%
    mutate(adjusted_value = value),
  
  petroleum_coke = seds %>%
    filter(msn == "PCEIB") %>%
    # Make sector names match to complete the next join
    mutate(sector_description = word(sector_description, 
                                     start = 1, end = 3)) %>%
    mutate(adjusted_value = value),
  
  distillate_fuel = seds %>%
    filter(msn == "DFEIB") %>%
    # Make sector names match to complete the next join
    mutate(sector_description = word(sector_description, 
                                     start = 1, end = 3)) %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(adjustments, 
              by = c("source_description", "year", "sector_description")) %>%
    # Rename for clarity
    rename(distillate_fuel_factor = national_value) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(adjusted_value = distillate_fuel_factor * 
             (value / states_sum_value))) %>%
  
  # Collapse list into a single data frame
  list_rbind()

# Notes from Review of Excel Workbook------------------------------------

# coal = state's CLEIB btu/1000
# sum_coal = sum of all states' coal
# coal_factor = value from us compare elec row 54
# coal_adj = coal_factor * (coal / sum_coal)

# natural_gas_inc_supplemental = state's NGEIB btu/1000
# supplemental_gas = state's SFEIB btu/1000
# natural_gas = natural_gas_incl_supplemental - supplemental_gas
# sum_natural_gas = sum of all states' natural_gas
# natural_gas_factor = value from us compare elec row 55
# natural_gas_adj = natural_gas_factor * 
# (natural_gas / sum_natural_gas, zero if <0)

# distillate_fuel = state's DFEIB btu/1000
# distillate_fuel = sum of all states' distillate_fuel
# distillate_fuel_factor = value from us compare elec row 56
# distillate_fuel_adj = distillate_fuel_factor * 
# (distillate_fuel / sum_distillate_fuel)