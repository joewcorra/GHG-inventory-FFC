# Calculate State-Level CO2 Emissions
# NEU Adjustments


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



# Notes from Review of Excel Workbook------------------------------------

# there are hard-coded numbers
# other_coal_factor: coal to gas from Eastman gas plant from FFC CO2 
# file SNG corrections; assume all in TN
# natural_gas_factor: from FFC CO2 file NEU NG to chemical plants, other uses;
# assume Assume percentage based on Petrochemical from IPPU adj
# lpg_factor: from FFC CO2 file NEU input; 
# assume same percent as SEDS use
# pentanes_plus_factor: : from FFC CO2 file NEU input; 
# assume same percent as SEDS use
# still_gas_factor: from FFC CO2 file NEU input; 
# assume same percent as SEDS use
# petroleum_coke_factor: from FFC CO2 file NEU input; 
# assume same percent as SEDS use
# distillate_fuel_factor: from FFC CO2 file NEU input; 
# assume same percent as IPPU adjusted

# coking_coal_adjusted (from 'ind adj')

# other_coal = other_coal_factor * mystery percentage

# natural_gas = natural_gas_factor * mystery percentage

# asphalt = state's ARICB btu/1000

# hgl = state's HLICB btu/1000

# pentanes_plus = state's PPICB btu/1000
# sum_pentanes_plus = sum of all states' pentanes_plus
# pentanes_plus_adj = pentanes_plus_factor * 
# (pentanes_plus / sum_pentanes_plus)

# lpg = hgl - pentanes_plus
# sum_lpg = sum of all states' lpg
# lpg_adj = lpg_factor * (lpg / sum_lpg)

# Industrial
# lubricants = state's LUICB btu/1000

# naphtha = state's FNICB btu/1000

# other_oil = state's FOICB btu/1000

# still_gas = state's SGICB btu/1000
# sum_still_gas = sum of all states' still_gas
# still_gas_adjusted = still_gas_factor * (still_gas / sum_still_gas)

# petroleum_coke = state's PCICB btu/1000
# sum_petroleum_coke = sum of all states' petroleum_coke
# petroleum_coke_adj = petroleum_coke_factor * 
# (petroleum_coke / sum_petroleum_coke)

# special_naphtha = state's SNICB btu/1000

# distillate_fuel = distillate_fuel_factor * distillate_fuel_adjusted_1

# waxes = state's WXICB btu/1000

# misc_products = state's MSICB btu/1000

# Transportation
# lubricants = state's LUACB btu/1000