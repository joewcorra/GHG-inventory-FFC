# Calculate State-Level CO2 Emissions
# National Corrections Data -- All
# This script replaces all of the other scripts that read in Excel 
# data from the national FFC calculations. 

print("Retrieving national FFC data required to adjust SEDS data.")

# Objects Created--------------------------------------------------------



# List of objects created in the global environment:


# National FFC Data------------------------------------------------------

# adjustments are pulled from unadjusted national data 
adjustments <- us_consumption %>%
  # Data is only used to adjust the following sources
  filter(source_descrption %in% c("distillate fuel oil", "motor gasoline", 
                                  "natural gas", "coal", 
                                  "petroleum coke", "lpg")) %>%
  # Rename value field to distinguish from state-level values during joins
  select(year, sector_description, source_description, national_value = value) 

# consumption_input is pulled from adjusted national data 
consumption_input <- us_all_adjusted %>% 
  # Data is only used to adjust the following industrial sources
  filter(source_description %in% c("coal", "natural gas", 
                                   "distillate fuel oil", 
                                   "residual fuel oil"), 
         sector_description == "industrial sector") %>%
  # Rename value field to distinguish from state-level values during joins
  select(year, sector_description, source_description, national_value = value) 
  

# REMINDER
# Need to figure out the HGL/LPG grouping. Calling it 'lpg' for now
print("Note: correct the lpg/hgl grouping issues across scripts.")
print("And maybe deal with variable names ('value' vs 'adjusted value'.")


national_corrections
# provides sng_correction, nat_gas_ammonia_factor, ippu,
# cb_factor, is_gas_factor, is_distillate_fuel_factor, is_coal_factor

ibf_corrections <- jet_fuel_consumption

neu_corrections # required for other coal, nat gas, dist fuel, hgl/lpg,
  # pentanes plus, petrol coke, stil gas

is_distribution # these four distribution lists come from IPPU data
# Which, in turn, comes from inaccessible sources. Need to ask Vince.
ammonia_distribution

petrochemicals_distribution

petrochemicals_cb_distribution

carbon_factors 

