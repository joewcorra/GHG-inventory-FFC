# Calculate National-Level CO2 Emissions
# Industrial Sector


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:


# Collate Consumption Data------------------------------------------------

seds_ind <- us_consumption %>%
  filter(sector_description == "industrial sector")




lst(
  
  # Asphalt & Road Oil
  asphalt = us_consumption %>%
    filter(msn == "ARICB"),
  
  # Coking Coal
  coking_coal = us_consumption %>%
    filter(msn == "CLKCB"),
  
  # Other Coal
  other_coal = us_consumption %>%
    filter(msn == "CLOCB"),
  
  # Natural Gas
  # Supplemental gas already excluded
  natural_gas = us_consumption %>%
    filter(msn == "NNICB"),
  
  # Residual Fuel
  residual_fuel = us_consumption %>%
    filter(msn == "RFICB"),
  
  # Distillate Fuel
  distillate_fuel = us_consumption %>%
    filter(msn == "DFICB"),
  
  # Gasoline
  gasoline = us_consumption %>%
    filter(msn %in% c("MGICB", "EMICB")),
  
  # Kerosene
  kerosene = us_consumption %>%
    filter(msn == "KSICB"),
  
  # Petroleum Coke
  petroleum_coke = us_consumption %>%
    filter(msn == "PCICB"),
  
  # LPG
  lpg = us_consumption %>%
    filter(msn == "HLICB"),

  
  # PQICB     PYICB
  
  
  # Lubricants
  lubricants = us_consumption %>%
    filter(msn == "LUICB"),
  
  # Misc Products
  misc_products = us_consumption %>%
    filter(msn == "MSICB"),
  
  # Naphtha (<401 deg. F)
  naphtha = us_consumption %>%
    filter(msn == "FNICB"),
  
  # Other Oil (>401 deg. F)
  other_oil = us_consumption %>%
    filter(msn == "FOICB"),
  
  # Pentanes Plus
  pentanes_plus = us_consumption %>%
    filter(msn == "PPICB"),
  
  # Still Gas
  still_gas = us_consumption %>%
    filter(msn == "SGICB"), 
  
  # Special Naphtha
  special_naphtha = us_consumption %>%
    filter(msn == "SNICB"), 
  
  # Waxes
  waxes = us_consumption %>%
    filter(msn == "WXICB"), 
  
  # Unfinished Oils   
  unfinished_oils = us_consumption %>%
    filter(msn == "UOICB"))

  # Collapse list into a single data frame
  list_rbind()