# Calculate State-Level CO2 Emissions
# Transportation Adjustments


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:

## Transportation Adjustments-------------------------------------------


# distillate fuel (), gasoline (), natural gas (NGACB) 


# Break SEDS data into list based on MSNs
seds_tra_adjusted <- lst(
  
  distillate_fuel = diesel_distribution %>% 
    # Join with adjustments data
    left_join(adjustments %>% 
                # Can only join by 'year', so a filter is required
                filter(source_description == "distillate fuel oil", 
                       sector_description == "transportation sector"), 
              by = c("year")) %>%
    mutate(adjusted_value = national_value * diesel_percent), 
  
  gasoline = gasoline_distribution %>%
    # Join with adjustments data
    left_join(adjustments %>% 
                # Can only join by 'year', so a filter is required
                filter(source_description == "motor gasoline", 
                       sector_description == "transportation sector"), 
              by = c("year")) %>%
    mutate(adjusted_value = national_value * gasoline_percent), 
  
  lubricants = seds %>%
    filter(msn == "LUACB") %>%
    # Adjusted = original value / 1000
    mutate(adjusted_value = value / 1000), 
  
  jet_fuel = seds %>%
    filter(msn == "JFACB") %>% 
    # Adjusted = original value / 1000
    mutate(adjusted_value = value / 1000), 
  
  aviation_gasoline = seds %>%
    filter(msn == "AVACB") %>%
  # Adjusted = original value / 1000
  mutate(adjusted_value = value / 1000), 
  
  natural_gas = seds %>%
    filter(msn == "NGACB") %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(adjustments, 
              by = c("source_description", "year", "sector_description")) %>%
    # rename for clarity
    rename(natural_gas_factor = national_value) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states' value / the above sum
    mutate(adjusted_value = natural_gas_factor * 
             (value / states_sum_value), 
           source_description = "natural gas")) %>%
  
  # Collapse list into a single data frame
  list_rbind()


# Notes from Review of Excel Workbook------------------------------------

# distillate_fuel_factor = value from us compare trans row 47
# distillate_fuel_adj = distillate_fuel_factor * mystery percentage

# gasoline_factor = value from us compare trans row 51
# gasoline_adj = gasoline_factor * mystery percentage

# natural_gas_factor = value from us compare trans row 45
# natural_gas = state's NGACB btu/1000
# sum_natural_gas = sum of all states' natural_gas
# natural_gas_adj = natural_gas_factor * (natural_gas / sum_natural_gas)