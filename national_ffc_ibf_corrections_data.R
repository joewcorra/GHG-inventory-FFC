# Calculate IBF (International Bunker Fuels) Adjustments
# Applies to Transportation


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:


# IBF Adjustments---------------------------------------------

# Requires heat_content from jet fuel which is CONSTANT at 5.670
# Correction: no, it doesn't need this
# jet_fuel_heat_content <- 5.670 

# Aviation jet fuel (tbtu) = intl comm aviation (hard coded number in Transport workbook) +
                              # military aircraft (hidden worksheet)

# Marine residual fuel = ??? (hidden worksheet)


# Marine distillate fuel = ??? (hidden worksheet)

# Adjust tra consumption by subtracting these

ibf_jet_fuel_adjustment

ibf_residual_fuel_adjustment

ibf_dist_fuel_adjustment


