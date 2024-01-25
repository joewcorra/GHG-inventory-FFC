# Calculate State-Level CO2 Emissions
# Transportation Adjustments


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:

## Transportation Adjustments-------------------------------------------


# distillate fuel (), gasoline (), natural gas (NGACB) 


# Break SEDS data into list based on MSNs
seds_tra_adjusted <- lst(
  
  distillate_fuel = adjustments %>% 
    filter(sector_description == "transportation", 
           source_description == "distillate fuel oil") %>%
    mutate(adjusted_value = adjustment_factor * 1), # mystery % 
  # the mystery %s have the state code data
  
  gasoline = adjustments %>% 
    filter(sector_description == "transportation", 
           source_description == "aviation gasoline") %>%
    mutate(adjusted_value = adjustment_factor * 1), # mystery % 
  # the mystery %s have the state code data
  
  lubricants = seds %>%
    filter(msn == "LUACB"), 
  
  natural_gas = seds %>%
    filter(msn == "NGACB") %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(adjustments %>% 
                filter(sector_description == "transportation"), 
              by = c("source_description", "year")) %>%
    # rename for clarity
    rename(natural_gas_factor = adjustment_factor) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states' value / the above sum
    mutate(adjusted_value = natural_gas_factor * 
             (value / states_sum_value))) %>%
  
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