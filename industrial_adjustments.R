# Calculate State-Level CO2 Emissions
# Industrial Adjustments


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:

# Industrial Adjustments-------------------------------------------------

# NGICB, RFICB, DFICB, MGICB, EMICB, PCICB, 
# HLICB, PPICB, "industrial sector"
# FINB "industrial sector (supplemental gaseous fuels)"
# CLKCB "coke plants (coal only)"
# CLOCB "industrial consumption, excluding coke plants"
# See below for specifics

### Coal------------------------------------------------------------------

# there are hard-coded numbers
# ippu (IPPU correction total): from FFC CO2 file, corrections
# sng_factor (SNG adjustment): from FFC CO2 file, corrections; assume in ND
# is_coal_factor (i & s adjustment for coal): from FFC CO2 file, corrections; 
# sssume distributed over percent of GHGRP I&S emissions
# other_coal_adj national total??? see below: From FFC CO2 file adj input;
# assume same percent as IPPU adjusted

# coking_coal = state's CLKCB btu/1000
# sum_coking_coal = sum of all states' coking_coal 
# ippu_percent = ippu / sum_coking_coal (if ippu < sum_coking_coal, else 100%)
# coking_coal_adjusted = state_coking_coal - (state_coking_coal * ippu_percent)

# other_coal = state's CLOCB btu/1000
# coke_factor = ippu - sum_coking_coal (if < 0, = 0)
# other_coal_coke_adj  = (coking_coal/ sum_coking_coal) * coke_factor
# other_coal_sng_adj  = sng_factor * another mysterious hard-coded %
# other_coal_is_adj = is_coal_factor * another mysterious hard-coded %
# other_coal_adjusted_1 = other_coal - other_coal_coke_adj -
# other_coal_sng_adj - other_coal_is_adj (if negative, then 0)
# other_coal_ippu_total = sum(other_coal_adjusted_1)

# another other coal national total = From FFC CO2 file adj input

# other_coal_adjusted_2 (or, other_coal_adjusted??) = 
# this new mystery total * (other_coal_adjusted_1 / other_coal_ippu_total)


### Natural Gas----------------------------------------------------------

# there are hard-coded numbers
# blast_furnace_gas_factor: From FFC CO2 file corrections
# Assume percent of I&S from GHGRP
# coke_oven_gas_factor: From FFC CO2 file corrections
# Assume percent of I&S from GHGRP
# ammonia_factor: From FFC CO2 file corrections;
# Assume same percent as GHGRP ammonia emissions
# is_gas_factor (i & s adjustment for natural gas): From FFC CO2 
# corrections; assume same percent as I&S GHGRP
# natural gas national total: From FFC CO2 file corrected input
# Assume same percentage as adj totals

# natural_gas_inc_supplemental = state's NGICB btu/1000 
# supplemental_gas = state's SFINB btu/1000 
# natural_gas = natural_gas_inc_supplemental - 
# supplemental_gas 

# total_furnace_factor = blast_furnace_gas_factor + coke_oven_gas_factor
# natural_gas_furnace_adj = total_furnace_factor * mysterious hard-coded  
# % used in the other_coal_is_adj, above

# natural_gas_ammonia_adj = ammonia_factor * another mysterious hard-coded %

# natural_gas_is_adj = is_gas_factor * mysterious hard-coded % used 
# in the other_coal_is_adj, above

# natural_gas_adjusted_1 = natural_gas - natural_gas_furnace_adj -
# natural_gas_ammonia_adj- natural_gas_is_adj (if negative, then 0)
# natural_gas_ippu_total = sum(natural_gas_adjusted_1)

# another natural gas national total: From FFC CO2 file corrected input

# natural_gas_adjusted_2 =
# this new mystery total * (natural_gas_adjusted_1 / natural_gas_ippu_total)

### Residual Fuels-------------------------------------------------------

# there are hard-coded numbers
# cb_factor: From FFC CO2 file carbon black corrections;
# Assume same percent as petrochemicals GHGRP emissions
# residual fuel national total: From FFC CO2 file input corrected;
# Assume percent based on adjusted IPPU tota

# residual_fuel = state's RFICB btu / 1000

# residual_fuel_cb_adj = cb_factor * mysterious hard-coded % 

# residual_fuel_adjusted_1 = residual_fuel - 
# residual_fuel_cb_adj (if negative, then 0)
# residual_fuel_ippu_total = sum(residual_fuel_adjusted_1)

# residual fuel national total: From FFC CO2 file input corrected

# residual_fuel_adjusted_2 = 
# this new mystery total * 
# (residual_fuel_adjusted_1 / residual_fuel_ippu_total)


### Distillate Fuel------------------------------------------------------

# there are hard-coded numbers
#  is_distillate_fuel_factor: From FFC CO2 corrections;
# assume same percent as I&S GHGRP
# distillate fuel national total: From FFC CO2 file diesel fuel adjusted input;
# Assume same percent as IPPU adjusted.

# distillate_fuel = state's DFICB btu / 1000

# distillate_fuel_is_adj = is_distillate_fuel_factor * 
# mysterious hard-coded % used in the other_coal_is_adj, above

# distillate_fuel_adjusted_1 = distillate_fuel - 
# distillate_fuel_is_adj (if negative, then 0)
# distillate_fuel_ippu_total = sum(distillate_fuel_adjusted_1)

# distillate fuel national total: From FFC CO2 file diesel fuel adjusted input;

# distillate_fuel_adjusted_2 =  
# this new mystery total * 
# (distillate_fuel_adjusted_1 / distillate_fuel_ippu_total)


### Gasoline--------------------------------------------------------


# gasoline_inc_ethanol = state's MGICB btu/1000 
# ethanol_only = state's EMICB btu/1000 
# gasoline = gas_with_ethanol - ethanol_only
# sum_gasoline: sum of all states' gasoline

# motor_gasoline_factor = US Compare--Industrial--motor gasoline 
# gasoline_adj = motor_gasoline_factor * (gasoline / sum_gasoline)



### Petroleum Coke---------------------------------------------------

# petroleum_coke = state's PCICB btu/1000 
# sum_petroleum_coke: sum of all states' petroleum coke

# ind_petroleum_coke_factor = US Compare--Industrial--petroleum coke

# petroleum_coke_adj = ind_petroleum_coke_factor * 
# (petroleum_coke / sum_petroleum_coke)




### LPG----------------------------------------------------------------

# hgl = state's HLICB btu / 1000
# pentanes_plus = state's PPICB btu / 1000
# lpg = hgl - pentanes_plus
# sum_lpg = sum of all states' lpg

# ind_lpg_factor = US SEDS Total--LPG (state's HLICB - PPICB)
# lpg_adj = ind_lpg_factor * (lpg / sum_lpg)








