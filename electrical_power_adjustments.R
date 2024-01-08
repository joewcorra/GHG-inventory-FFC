# Calculate State-Level CO2 Emissions
# Electric Power Adjustments# 


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:


## Electrical Power Adjustments-----------------------------------------

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

