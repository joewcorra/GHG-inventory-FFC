# Calculate State-Level CO2 Emissions
# IBF Adjustments


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:



# IBF-------------------------------------------------------------------

# there are hard-coded numbers
# residual_fuel_factor: FFC CO2 file IBF input; assume percentage from 
# FOKS bunker
# distillate_fuel_factor: FFC CO2 file IBF input; assume percentage from 
# FOKS bunker
# jet_fuel_factor: FFC CO2 file IBF input; assume percentage from SEDS total

# residual_fuel = residual_fuel_factor * mystery percentage

# distillate_fuel = distillate_fuel_factor * mystery percentage

# jet_fuel = state's JFACB btu/1000
# sum_jet_fuel = sum of all states'jet_fuel
# jet_fuel_adj = jet_fuel_factor * (jet_fuel / sum_jet_fuel)
