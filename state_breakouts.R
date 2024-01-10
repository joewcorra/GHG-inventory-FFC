# Calculate State-Level CO2 Emissions
# State Data Breakouts

# Objects Created--------------------------------------------------------

# List of objects created in the global environment:



# State Breakouts (Final)-----------------------------------------------

# Split into list elements by state
dataobjectname %>% # TBD
group_by(state) %>%
  group_split() 

# Unadjusted Residential, Commercial, Industrial, Transportation, Elec Power
# find all btu values (again!) in the SEDS 'se all btu' data
# When applicable, value c = value a - value b 

# Adjusted Residential, Commercial, Industrial, transportation, Elec Power
# Residential
# Lookup in 'res adj': coal, nat gas, dist fuel
# Copy from unadjusted above: kerosene, lpg
# Commercial
# Lookup in 'com adj': coal, nat gas, dist fuel, motor gasoline
# Copy from unadjusted above: kerosene, lpg, resid fuel, petro coke
# Industrial
# Lookup in 'ind adj': coking coal, other coal, nat gas, dist fuel, 
# lpg, motor gasoline, resid fuel, petro coke
# Copy from unadjusted above: asphalt, kerosene, lubricants, 
# avgas blend, crude oil, mogas blend, misc prod, naphtha, other oil, 
# pentanes plus, still gas, special naphtha, unfinished oils, waxes
# Transportation
# Lookup in 'trans adj': nat gas, dist fuel, motor gasoline, 
# Copy from unadjusted above: coal, av gas, jet fuel, lpg, 
# lubricants, resid fuel 
# Electrical Power
# Lookup in 'trans adj':  coal, nat gas, dist fuel (light)
# Copy from unadjusted above: Resid fuel (heavy), petro coke

# Adjustments: minus NEU and IBF (Ind and Trans only)
# TO DO
# NEU: IND coking coal, other coal, nat gas, asphalt, hgl, pentanes, lpg, 
# lubricants, napththa, other oil, still gas, petro coke, spec naphtha, 
# dist fuel, waxes, misc products, TRANS lubricants
# IBF: resid fuel, dist fuel, jet fuel

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