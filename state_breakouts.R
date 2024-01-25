# Calculate State-Level CO2 Emissions
# State Data Breakouts

# Objects Created--------------------------------------------------------

# List of objects created in the global environment:



# State Breakouts (Final)-----------------------------------------------

# Split into list elements by state
dataobjectname %>% # TBD
group_by(state) %>%
  group_split() 

 # final value = adjusted value - NEU or IBF (if applicable)

# FOR THE FOLLOWING SOURCES, we adjust for NEU:
# Coking coal, other coal, natural gas, distillate fuel, LPG, pentanes plus, 
# petroleum coke, still gas.

# FOR THE FOLLOW SOURCES, WE ASSUME we assume 100% of consumption is NEU; 
# therefore, NEU adjusted value = value - value (i.e., zero).
# asphalt & road oil, cpking coal, lubricants (both ind & tra), naphtha, 
# other oil, special naphtha, waxes, misc products.

# FOR THE FOLLOWING TRANSPORTATION SOURCES, we adjust for IBF:
# distillate fuel, residual fuel, jet fuel.



# Notes from Review of Excel Workbook-----------------------------------

# Adjusted Residential, Commercial, Industrial, Transportation, Elec Power

# Additional Adjustments: minus NEU or IBF (Ind and Trans) for selected sources

# Carbon calcularions: 
# Adjustments: MMT CO2 (multiply by 'factors' and carbon_ratio)
# Residential: coal, natural gas, dist fuel, kerosene = 
# adjusted value * (foo_factor / 1000) * carbon_ratio
# petroleum = dist fuel + kerosene + lpg
# Commercial: coal, natural gas, dist fuel, kerosene, motor gas, resid fuel,
# petro coke = adjusted value * (foo_factor / 1000) * carbon_ratio
# petroleum = distillate_fuel + kerosene + lpg + motor_gasoline + 
# residual_fuel + petroleum_coke
# Industrial: coking coal, other coal, natural gas, asphalt, dist fuel, 
# kerosene, lpg, lubricants, motor gas, resid fuel. avgas blend, 
# crude oil, mo gas blend, misc products, naphtha, other oil, pentanes plus,
# petro coke, still gas, special naphtha, unfinished oils, waxes = 
# adjusted value * (foo_factor / 1000) * carbon_ratio
# coal = coking coal + other coal
# petroleum = sum(everything except coal and gas)
# Transportation: coal, natural gas, aviation gas, dist fuel, jet fuel, 
# lpg, lubricants, motor gas, resid fuel = 
# adjusted value * (foo_factor / 1000) * carbon_ratio
# petroleum = sum(everything except coal and gas)
# Electrical Power: coal, natural gas, dist fuel (light), resid fuel (heavy) = 
# adjusted value * (foo_factor / 1000) * carbon_ratio
# petroleum = dist fuel (light) + resid fuel (heavy) + petro coke

# THEN summary -> state_summary -> invdb, -> trans_summary (separate branch)