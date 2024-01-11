# Calculate State-Level CO2 Emissions
# Industrial Adjustments


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:

# seds_ind_adjusted: tibble; adjusted values for industrial sector emissions

# Adjustment Factors Specific to Industrial Sources---------------------

# Placeholders: set all values to '1' for now
# These will probably be data frames or vectors. Will require a join

## Coking Coal
### ippu (IPPU correction total): from FFC CO2 file, corrections
ippu <- 1
### sng_factor (SNG adjustment): from FFC CO2 file, corrections; ND only
sng_factor <- 1
### cb_factor: From FFC CO2 file carbon black corrections;
### Assume same percent as petrochemicals GHGRP emissions
cb_factor <- 1
### residual fuel national total: From FFC CO2 file input corrected
residual_fuel_national <- 1
### is_distillate_fuel_factor: From FFC CO2 corrections;
### assume same percent as I&S GHGRP
is_distillate_fuel_factor <- 1
### distillate fuel national total: From FFC CO2 file input corrected
distillate_fuel_national <- 1

# Industrial Adjustments-------------------------------------------------

# coking coal (CLKCB), other coal (CLOCB), ?net coke imports (CCNIB)?, 
# natural gas = gas w supplemental (NGICB) - supplemental gas (SFINB), 
# distillate fuel (DFICB), 
# lpg = hgl (HLICB) + propane (PQICB) + propylene (PYICB) + ethane (EQICB) + 
# ethylene (EYICB) + normal butane (BQICB) + butylene (BYICB) + 
# isobutane (IQICB) + isobutylene (IYICB), 
# motor gasoline = gas w ethanol (MGICB) + ethanol (EMICB), 
# residual fuel (RFICB), petroleum coke (PCICB)
# unadjusted: asphalt (ARICB), kerosene (KSICB), lubricants (LUICB), 
# avgas blend (ABICB), crude oil (COICB), mogas blend (MBICB), 
# misc products (MSICB), naphtha (FNICB), other oil (FOICB), 
# pentanes plus (PPICB), still gas (SGICB), special naphtha (SNICB), 
# unfinished oils (UOICB), waxes (WXICB)


# NOTE: Might be necessary to delete adjustment variables before list_rbind()

# Break SEDS data into -element list based on MSNs
seds_ind_adjusted <- lst(
  
  # Coking Coal
  coking_coal = seds %>% 
    filter(msn == "CLKCB") %>%
    mutate(states_sum_value = sum(value), .by = c(msn, year), 
           # ippu percent = ippu / sum_states_value as long as ippu is greater
           adjustment_factor = if_else(ippu > states_sum_value, 
                                       ippu / states_sum_value, 100), 
           coking_coal_adjusted = value - (value * adjustment_factor)),
  
  # Other Coal
  # INCOMPLETE
  # This needs a lot of work. Gotta pass coking coal values to this element
  other_coal = seds %>%
    filter(msn == "CLOCB") %>%
    left_join(coking_coal %>% 
                select(sum_coking_coal = states_sum_value, 
                       coking_coal_value = value, year, state), 
              by = c("year", "state")) %>%
    mutate(adjustment_factor = if_else(ippu < sum_coking_coal, 0,
                                       ippu - sum_coking_coal),
           value = adjustment_factor * (coking_coal_value / sum_coking_coal)),
    # other_coal_coke_adj  = (coking_coal/ sum_coking_coal) * coke_factor
    # other_coal_sng_adj  = sng_factor * another mysterious hard-coded %
    # other_coal_is_adj = is_coal_factor * another mysterious hard-coded %
    # other_coal_adjusted_1 = other_coal - other_coal_coke_adj -
    # other_coal_sng_adj - other_coal_is_adj (if negative, then 0)
    # other_coal_ippu_total = sum(other_coal_adjusted_1)
    # another other coal national total = From FFC CO2 file adj input
    # other_coal_adjusted_2 (or, other_coal_adjusted??) = 
    # this new mystery total * (other_coal_adjusted_1 / other_coal_ippu_total)
  
  # Natural Gas
  # INCOMPLETE
  # This needs a lot of work. Many additional adjustment factors
  natural_gas = seds %>% 
    # Separate list element required to find net natural gas
    filter(msn %in% c("NGICB", "SFINB")) %>%
    # Subtract supplemental gas from total natural gas
    mutate(value = abs(diff(value)), .by = c(state, year)) %>%
    # Group_size shows that each group has exactly two rows. Good!
    # Supplemental gas no longer needed (and value is now duplicative)
    filter(msn != "SFICB") %>%
    # Change MSN identifier. old MSN distinction no longer needed(?)
    # However, MSN can be reconstituted from other _code fields if needed.
    mutate(msn = "net_natural_gas"),
  
  # Residual Fuel
  residual_fuel = seds %>%
    filter(msn == "RFICB") %>%
    # adjust for cb factor = cb_factor * mysterious hard-coded % 
    # adjusted value = value - cb adjusted value (minimum = 0)
    mutate(adjustment_factor = cb_factor,
           value = if_else(value - (cb_factor * 1) < 0, 0, 
                           # 1 is a placeholder for the mystery percentage
                           value - (cb_factor * 1))) %>% 
    # Get sum of all states' cb adjusted values
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>% 
    # now adjust by residual fuel national total
    mutate(value = residual_fuel_national * (value / states_sum_value)),
  
  # Distillate Fuel
  distillate_fuel = seds %>%
    filter(msn == "DFICB") %>%
    # distillate_fuel_is_adj = is_distillate_fuel_factor * 
    # mysterious hard-coded % used in the other_coal_is_adj, above
    # distillate_fuel_is_adj (if negative, then 0)
    mutate(adjustment_factor = is_distillate_fuel_factor,
           value = if_else(value - (is_distillate_fuel_factor * 1) < 0, 0, 
                           # 1 is a placeholder for the mystery percentage
                           value - (is_distillate_fuel_factor * 1))) %>%
    # Get sum of all states' cb adjusted values
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>% 
    # now adjust by distillate fuel national total
    mutate(value = distillate_fuel_national * (value / states_sum_value)),
  
  # Gasoline
  gasoline = seds %>% 
    # All gasoline - ethanol = net gasoline
    filter(msn %in% c("MGICB", "EMICB")) %>%
    # Subtract ethanol from total gasoline
    mutate(value = abs(diff(value)), .by = c(state, year)) %>%
    # Group_size shows that each group has exactly two rows. Good!
    # Supplemental gas no longer needed (and value is now duplicative)
    filter(msn != "EMICB") %>%
    # Get sum of all states' net gasoline
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # get adjustment factor for motor gasoline
    left_join(adjustments %>% 
                filter(sector_description == "industrial"), 
              by = c("source_description", "year")) %>%
    # Change MSN identifier. old MSN distinction no longer needed(?)
    # However, MSN can be reconstituted from other _code fields if needed.
    mutate(msn = "net_gasoline", 
           # adjusted net gasoline = motor gas factor * gasoline - sum
    value = adjustment_factor * (value / states_sum_value)),
  # motor_gasoline_factor = US Compare--Industrial--motor gasoline 

  # Petroleum Coke
  petroleum_coke = seds %>%
    filter(msn == "PCICB") %>%
    # Get sum of all states' petroleum_coke
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # get adjustment factor for petroleum coke
    left_join(adjustments %>% 
                filter(sector_description == "industrial"), 
              by = c("source_description", "year")) %>%
    # adjusted petro coke = petro coke factor * (petro coke / sum of states)
    mutate(value = adjustment_factor * (value / states_sum_value)),
  
  # All other sources go in this list element
  other_industrial = seds %>% 
    filter(msn %in% c("ARICB", "KSICB", "LUICB", "ABICB", "COICB", 
                      "MBICB", "MSICB", "FNICB", "FOICB", "PPICB", 
                      "SGICB", "SNICB", "UOICB", "WXICB")) %>%
    # Adjusted = original value. Consider using a different variable name here
    mutate(adjusted_value = value)) %>%
  
  # Collapse list into a single data frame
  list_rbind()

# Notes from Review of Excel Workbook------------------------------------
## Coal------------------------------------------------------------------

# there are hard-coded numbers
# sng_factor (SNG adjustment): from FFC CO2 file, corrections; assume in ND
# ippu (IPPU correction total): from FFC CO2 file, corrections
# is_coal_factor (i & s adjustment for coal): from FFC CO2 file, corrections; 
# assume distributed over percent of GHGRP I&S emissions
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


## Natural Gas----------------------------------------------------------

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

## Residual Fuels-------------------------------------------------------

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


## Distillate Fuel------------------------------------------------------

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


## Gasoline--------------------------------------------------------


# gasoline_inc_ethanol = state's MGICB btu/1000 
# ethanol_only = state's EMICB btu/1000 
# gasoline = gas_with_ethanol - ethanol_only
# sum_gasoline: sum of all states' gasoline

# motor_gasoline_factor = US Compare--Industrial--motor gasoline 
# gasoline_adj = motor_gasoline_factor * (gasoline / sum_gasoline)



## Petroleum Coke---------------------------------------------------

# petroleum_coke = state's PCICB btu/1000 
# sum_petroleum_coke: sum of all states' petroleum coke

# ind_petroleum_coke_factor = US Compare--Industrial--petroleum coke

# petroleum_coke_adj = ind_petroleum_coke_factor * 
# (petroleum_coke / sum_petroleum_coke)




## LPG----------------------------------------------------------------

# hgl = state's HLICB btu / 1000
# pentanes_plus = state's PPICB btu / 1000
# lpg = hgl - pentanes_plus
# sum_lpg = sum of all states' lpg

# ind_lpg_factor = US SEDS Total--LPG (state's HLICB - PPICB)
# lpg_adj = ind_lpg_factor * (lpg / sum_lpg)



x<-seds_ind_adjusted[[1]]



