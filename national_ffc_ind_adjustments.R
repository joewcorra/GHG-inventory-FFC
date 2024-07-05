# Calculate National-Level CO2 Emissions
# Industrial Sector


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:


# Collate Consumption Data------------------------------------------------
# coking coal

us_ind <- lst(
  
  # Asphalt & Road Oil (NEU adjustment: 100%) 
  asphalt = us_consumption %>%
    filter(msn == "ARICB") %>%
    # NEU adjustment is 100% of total
    mutate(adjusted_value = value - value),
  
  # Coking Coal (IPPU adjustment)
  coking_coal = us_consumption %>%
    # What is the MSN for coking coal?
  filter(msn == "") %>%
    # Subtract IPPU adjustment
    mutate(adjusted_value = value - ippu_adj, 
           # Adjusted value no lower than zero 
           adjusted_value = if_else(adjusted_value < 0, 0, adjusted_value)),
  
  # Other Coal (NEU adjustment: Eastman Gas coal gasification; 
  # synthetic natural gas adjustment, coking coal adjustment, 
  # i & s adjustment
  other_coal = us_consumption %>%
    filter(msn == "CLICB") %>%
    # Subtract adjustments
    mutate(adjusted_value = value - sum(
      synth_gas_adj, coke_adj, is_adj, eastman_gas_adj)),
  
  # Natural Gas (NEU adjustment: special; blast furnace adjustment, 
  # coke oven adjustment, biogas adjustment, 
  # ammonia adjustment, and i & s adjustment)
  # Supplemental gas already excluded
  natural_gas = us_consumption %>%
    filter(msn == "NNICB") %>%
    # Subtract adjustments
    # Blast furnace, coke oven, and biogas are always zero?
    mutate(adjusted_value = value - sum(
      blast_furnace_adj, coke_oven_adj, biogas_adj, 
      ammonia_adj, is_adj)),
  
  # Residual Fuel (carbon black adjustment) 
  residual_fuel = us_consumption %>%
    filter(msn == "RFICB") %>%
    # Subtract carbon black correction
    mutate(adjusted_value = value - cb_adj, 
           # Adjusted value no lower than zero 
           adjusted_value = if_else(adjusted_value < 0, 0, adjusted_value)),
  
  # Distillate Fuel (i&s adjustment, mogas/df adjustment)
  distillate_fuel = us_consumption %>%
    filter(msn == "DFICB") %>%
    # Subtract iron & steel correction
    mutate(adusted_value = value - is_adj),
  
  # Motor gasoline (mogas/df adjustment)
  motor_gasoline = us_consumption %>%
    filter(msn %in% c("MGICB", "EMICB")),
  
  # Kerosene (no adjustment)
  kerosene = us_consumption %>%
    filter(msn == "KSICB"),
  
  # Petroleum Coke (NEU adjustment: special)
  petroleum_coke = us_consumption %>%
    filter(msn == "PCICB"),
  
  # LPG (AKA Propane) (no adjustment)
  lpg = us_consumption %>%
    filter(msn == "HLICB"),
  
  # PQICB     PYICB (NEU adjustment: special)
  # Propane and Propylene: Included w/ HLICB ?
  
  # Lubricants (NEU adjustment: 100%) 
  lubricants = us_consumption %>%
    filter(msn == "LUICB"),
  
  # Misc Products (NEU adjustment: 100%) 
  misc_products = us_consumption %>%
    filter(msn == "MSICB") %>%
    # NEU adjustment is 100% of total
    mutate(adjusted_value = value - value),
  
  # Naphtha (<401 deg. F) (NEU adjustment: 100%) 
  naphtha = us_consumption %>%
    filter(msn == "FNICB") %>%
    # NEU adjustment is 100% of total
    mutate(adjusted_value = value - value),
  
  # Other Oil (>401 deg. F) (NEU adjustment: 100%) 
  other_oil = us_consumption %>%
    filter(msn == "FOICB") %>%
    # NEU adjustment is 100% of total
    mutate(adjusted_value = value - value),
  
  # Pentanes Plus (NEU adjustment: special)
  pentanes_plus = us_consumption %>%
    filter(msn == "PPICB"),
  
  # Still Gas (NEU adjustment: special)
  still_gas = us_consumption %>%
    filter(msn == "SGICB"), 
  
  # Special Naphtha (NEU adjustment: 100%) 
  special_naphtha = us_consumption %>%
    filter(msn == "SNICB") %>%
    # NEU adjustment is 100% of total
    mutate(adjusted_value = value - value), 
  
  # Waxes (NEU adjustment: 100%) 
  waxes = us_consumption %>%
    filter(msn == "WXICB") %>%
    # NEU adjustment is 100% of total
    mutate(adjusted_value = value - value), 
  
  # Unfinished Oils (no adjustment)   
  unfinished_oils = us_consumption %>%
    filter(msn == "UOICB")) %>%

  # Collapse list into a single data frame
  list_rbind()
