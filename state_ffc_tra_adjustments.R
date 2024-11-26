# Calculate State-Level CO2 Emissions
# Transportation Adjustments


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:

## Transportation Adjustments-------------------------------------------


# distillate fuel (), gasoline (), natural gas (NGACB) 


# Break SEDS data into list based on MSNs
seds_tra_adjusted <- lst(
  
  distillate_fuel = fhwa_scraped$diesel_distribution %>% 
    # Join with adjustments data
    left_join(corrections$adjustments %>% 
                # Can only join by 'year', so a filter is required
                filter(source_description == "distillate fuel oil", 
                       sector_description == "transportation sector"), 
              by = c("year")) %>%
    mutate(adjusted_value = national_value * diesel_percent, 
           msn = "DFACB"), 
  
  gasoline = fhwa_scraped$gasoline_distribution %>%
    # Join with adjustments data
    left_join(corrections$adjustments %>% 
                # Can only join by 'year', so a filter is required
                filter(source_description == "motor gasoline", 
                       sector_description == "transportation sector"), 
              by = c("year")) %>%
    mutate(adjusted_value = national_value * gasoline_percent, 
           msn = "net gasoline"), 
  
  lubricants = seds %>%
    filter(msn == "LUACB") %>%
    # Adjusted = original value 
    mutate(adjusted_value = value), 
  
  jet_fuel = seds %>%
    filter(msn == "JFACB") %>% 
    # Adjusted = original value
    mutate(adjusted_value = value), 
  
  residual_fuel = seds %>%
    filter(msn == "RFACB") %>% 
    # Adjusted = original value
    mutate(adjusted_value = value), 
  
  coal = seds %>%
    filter(msn == "CLACB") %>% 
    # Adjusted = original value 
    mutate(adjusted_value = value), 
  
  # LPGs (propane and/or HGL)
  lpg = seds %>%
    filter(case_when(as.integer(year) < 2010 ~ msn == "HLACB", 
                     as.integer(year) >= 2010 ~ msn == "PQACB")) %>% 
    # Adjusted = original value
    mutate(msn = "combined lpg", 
           adjusted_value = value), 
  
  aviation_gasoline = seds %>%
    filter(msn == "AVACB") %>%
  # Adjusted = original value
  mutate(adjusted_value = value), 
  
  natural_gas = seds %>%
    filter(msn == "NGACB") %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(corrections$adjustments, 
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


# Cleanup=---------------------------------------------------------------

seds_adjusted <- append(seds_adjusted, lst(seds_tra_adjusted))

rm(seds_tra_adjusted)
