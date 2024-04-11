# Calculate National-Level CO2 Emissions
# Industrial Sector


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:


# Collate Consumption Data------------------------------------------------


us_ind <- lst(
  
  # Asphalt & Road Oil (NEU adjustment: 100%) 
  asphalt = us_consumption %>%
    filter(msn == "ARICB"),
  
  # Coking Coal 
  # ???
  
  # Other Coal (NEU adjustment: Eastman Gas coal gasification)
  other_coal = us_consumption %>%
    filter(msn == "CLICB"),
  
  # Natural Gas (NEU adjustment: special)
  # Supplemental gas already excluded
  natural_gas = us_consumption %>%
    filter(msn == "NNICB"),
  
  # Residual Fuel (no adjustment)
  residual_fuel = us_consumption %>%
    filter(msn == "RFICB"),
  
  # Distillate Fuel (mogas/df adjustment)
  distillate_fuel = us_consumption %>%
    filter(msn == "DFICB"),
  
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
    filter(msn == "MSICB"),
  
  # Naphtha (<401 deg. F) (NEU adjustment: 100%) 
  naphtha = us_consumption %>%
    filter(msn == "FNICB"),
  
  # Other Oil (>401 deg. F) (NEU adjustment: 100%) 
  other_oil = us_consumption %>%
    filter(msn == "FOICB"),
  
  # Pentanes Plus (NEU adjustment: special)
  pentanes_plus = us_consumption %>%
    filter(msn == "PPICB"),
  
  # Still Gas (NEU adjustment: special)
  still_gas = us_consumption %>%
    filter(msn == "SGICB"), 
  
  # Special Naphtha (NEU adjustment: 100%) 
  special_naphtha = us_consumption %>%
    filter(msn == "SNICB"), 
  
  # Waxes (NEU adjustment: 100%) 
  waxes = us_consumption %>%
    filter(msn == "WXICB"), 
  
  # Unfinished Oils (no adjustment)   
  unfinished_oils = us_consumption %>%
    filter(msn == "UOICB")) %>%

  # Collapse list into a single data frame
  list_rbind()