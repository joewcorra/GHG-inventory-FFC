# Calculate IBF (International Bunker Fuels) Adjustments
# Applies to Transportation


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:

# Fuel Densities----------------------------------------------

# These are fixed (I think)

fuels <- tibble(fuel = c("jet fuel", "JP8", "JP5", "JP4", "JAA", "JA1", "JAB", 
                         "distillate fuel", "commerce marine", 
                         "military marine", "residual fuel", 
                "aviation gasoline", "intermediate fuel oil (IFO)"),
                fuel_density = c(3.002, 3.04, 3.08, 2.90, 3.08, 3.04, NA, NA, 
                3.1916, 3.18, 3.575, 2.72, 3.81))


# Military Jet Fuel---------------------------------------------




# jet_fuel_military = sum of all jet fuels: JAB, JAA, JA1, JP4, JP5, JP8
# For each, account for both Navy and Air Force (what about the Army?)

# each fuel = millions of gallons * fuel_density * 1000000
# jet_fuel_military = sum(all of these)

# each fuel's gallons data = black box 

# Civilian Jet Fuel----------------------------------------------

# data from Int'l Commercial Aviation (AEDT ) (TBTU)
# "Personal Communciation" data ie black box


# Marine Residual Fuel-------------------------------------------

residual_fuel_marine <- residual_fuel_all * heat_content   / 1000

residual_fuel_all <- (residual_vessels_american + residual_vessels_foreign) *  42  /  1000

# diesel_vessels_american & diesel_vessels_foreign are black box data

# They're using a fixed value for heat content instead of the annually variable 
# Heat Content data. Need to look into that

# Marine Distillate Fuel-----------------------------------------

dist_fuel_marine <- commerce_marine + military_marine


commerce_marine <- diesel_fuel_all * heat_content / 1000

diesel_fuel_all = (diesel_vessels_american + diesel_vessels_foreign) *  42  /  1000

# diesel_vessels_american & diesel_vessels_foreign are black box data

military_marine <- MGO + F76 + IFO * heat_content
# Military Marine = MGO + F76 + IFO * heat_content 
# MGO, F76, IFO are black box data

# They're using a fixed value for heat content instead of the annually variable 
# Heat Content data. Need to look into that



# IBF Adjustments---------------------------------------------


jet_fuel_consumption <- jet_fuel_civilian + jet_fuel_military

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
