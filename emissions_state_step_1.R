# Calculate State-Level CO2 Emissions
# Step 1: Total Fuel Consumption by Fuel Type and Sector
# State-Level Source: Based on EIA SEDS (adjusted to match national totals 
# as applicable)

# Objects Created--------------------------------------------------------

# List of objects created in the global environment:

# adjustments: tibble; adjustment factors derived from national data (i think?)
# msn_lookup: vector; all MSNs used in the state summaries
# seds: tibble; all files in the 'data' subfolder
# seds_by_state: 


# EIA SEDS---------------------------------------------------------------

# EIA’s State Energy Data System (SEDS)
# Those data are broken out by fuel type and sector (residential, commercial,
# industrial, transportation, and electric power) and are available for the 
# years 1960–2021

# Read in SEDS data
seds <- list.files(path = "data", pattern = "\\.csv$", 
                   full.names = TRUE) %>%
  # Read all .csv files in the /data folder
  map(\(.x) read.csv(.x) %>% 
        clean_names()) %>%
  # Collapse all list elements into one data frame
  list_cbind() %>%
  # Change 'year' to a column
  pivot_longer(cols = starts_with("x") , names_to = "year") %>%
  # Remove unneeded columns
  select(-data_status) %>%
  # Get rid of leading 'x' in years
  mutate(year = str_remove(year, "x")) %>%
  # Retain data from 1990 onward
  filter(year > "1989")

# Might not keep this (?)
subsources <- c("residential", "commercial", "industrial", "transportation",
                "electricity generation", "us territories")

# Vector of MSNs to look up in the state summaries:
# These MSNs are all in billions of BTUs; converted to millions below. 
msn_lookup <- c("CLRCB", "NGRCB", "SFRCB", "DFRCB", "KSRCB", "HLRCB", "PQRCB", 
          "CLCCB", "NGCCB", "SFCCB", "DFCCB", "KSCCB", "PQCCB", "HLCCB", 
          "MGCCB", "EMCCB", "RFCCB", "PCCCB", "CLKCB", "CLOCB", "CCNIB", 
          "NGICB", "SFINB", "ARICB", "DFICB", "KSICB", "HLICB", "PQICB", 
          "PYICB", "EQICB", "EYICB", "BQICB", "BYICB", "IQICB", "IYICB", 
          "LUICB", "MGICB", "EMICB", "RFICB", "ABICB", "COICB", "MBICB", 
          "MSICB", "FNICB", "FOICB", "PPICB", "PCICB", "SGICB", "SNICB", 
          "UOICB", "WXICB", "CLACB", "NGACB", "AVACB", "DFACB", "BDACB", 
          "JFACB", "HLACB", "PQACB", "LUACB", "MGACB", "EMACB", "RFACB", 
          "CLEIB", "NGEIB", "SFEIB", "DFEIB", "RFEIB", "PCEIB", "EMTCB", 
          "BDTCB")


# Aggregate by State----------------------------------------------------

# For each state and year, create a data frame with the following columns:
# state, year, and values for all codes in 'msn_lookup'. Divide all 
# resulting values by 1000. 

seds_by_state <- seds %>%
  # Retain rows with matching MSNs
  filter(msn %in% msn_lookup) %>%
  left_join(msn, by = "msn") %>%
  # Convert to millions of BTUs, round to 2 places
  mutate(value = round(value / 1000, 2)) %>%
  # Split into list elements by state
  group_by(state) %>%
  group_split() 


# Adjust to Match National Totals----------------------------------------

# If SEDS data totals matched the national totals and there were no further
# adjustments needed (as per Steps 2–7), use the SEDS data

# Recursive: Requires data from Steps 2 and 5

# For fuels where the SEDS totals did not match the national totals (i.e., 
# coal, natural gas, and petroleum coke), adjust fuel use in each sector 
# to match the national totals. This calculation is based on the percentage of
# each fuel used in each state from the SEDS data. For the industrial
# sector, this adjustment was made after subtracting for uses in the
# IPPU sector (see Step 2 below).

# For other fuels where sector totals did not match up (e.g., gasoline 
# and diesel fuel), totals for each fuel type were generally taken from the 
# national Inventory (see Step 5), and the SEDS data or other proxy data 
# sources were used to determine state-level percentages of each fuel use.

# SEDS data -> adjust fuel use -> adjust industrial fuel use after Step 2 ->
# adjust other fuels after Step 5

# Read in adjustment factors data (derived from national inventory?)

adjustments <- read_csv("us_compare.csv") %>%
  clean_names() %>%
  # Change 'year' to a column
  pivot_longer(cols = starts_with("x"), 
               names_to = "year", values_to = "adjustment_factor") %>%
  # Get rid of leading 'x' in years
  mutate(year = str_remove(year, "x")) 

# JOINING PROBLEMS: 'Adjustments' lumps all LPGs ("butylene", "propane", 
# "propylene", "isobutane", "normal butane") together as 'lpg' in the 
# source_description, while SEDS is specific. 
  # Solution 1: create a new column in both tibbles 
  # Solution 2: in the csv, create additional identical rows for all of the 
  # sources.
# Other adjustments source_description without corresponding match in 
# SEDS: 'other coal', 'other hydrocarbon gas liquids'
  # 'other hydrocarbon gas liquids' is correct, but no matches in SEDS
  # 'other coal' has no MSN match or SEDS match 
# other SEDS source_description without corresponding match in 
# adjustments: 'biodiesel', 'ethane', 'isobutylene', 'supplemental fuels', 
# 'ethylene', 'fuel ethanol, exluding denaturant'
  # Biodiesel is addressed in a later step
  # LPGs: butylene, propane, propylene, isobutane, normal butane
  # 3 ethanol fuels = ???
  # Isobutylene = ???
  # Supplemental gaseous fuels = ???

## Residential Adjustments-------------------------------------------------

# CLRCB, NGRCB, SFRCB, DFRCB "residential sector"
# coal, natural gas, distillate fuel

# NOTE 1/2/24: need to join adjustments with seds_by_state; not sure if it 
# makes more sense to do this before or after the group_split()

# Any adjusted values = adjustment from US Compare * 
  # (state btu / total of all states' btu)

# natural gas = NGRCB - SFRCB (i.e., subtract the supplemental)

## Commercial Adjustments-------------------------------------------------

# CLCCB, NGCCB, SFCCB, DFCCB, MGCCB, EMCCB "commercial sector"
# coal, natural gas, distillate fuel, gasoline

# Any adjusted values = adjustment from US Compare * 
# (state btu / total of all states' btu)

# natural gas = NGRCB - SFRCB (i.e., subtract the supplemental)
# gasoline = MGCCB - EMCCB (i.e., subtract the ethanol)

## Industrial Adjustments-------------------------------------------------

# NGICB, RFICB, DFICB, MGICB, EMICB, PCICB, 
# HLICB, PPICB, "industrial sector"
# FINB "industrial sector (supplemental gaseous fuels)"
# CLKCB "coke plants (coal only)"
# CLOCB "industrial consumption, excluding coke plants"
# See below for specifics

### Coal------------------------------------------------------------------

# there are hard-coded numbers
# ippu (IPPU correction total)
# sng_adj (SNG adjustment)
# is_coal_adj (i & s adjustment for coal)
# other_coal_adj national total??? see below

# coking_coal = state's CLKCB btu/1000
# sum_coking_coal: sum of all states' coking_coal 
# ippu_percent = ippu / sum_coking_coal (if ippu < sum_coking_coal, else 100%)
# coking_coal_adjusted = state_coking_coal - (state_coking_coal * ippu_percent)

# other_coal = state's CLOCB btu/1000
# coke_adj = ippu - sum_coking_coal (if < 0, = 0)
# other_coal_coke_adj  = (coking_coal/ sum_coking_coal) * coke_adj
# other_coal_sng_adj  = sng_adj * another mysterious hard-coded %
# other_coal_is_adj = is_coal_adj * another mysterious hard-coded %
# other_coal_adjusted_1 = other_coal - other_coal_coke_adj -
  # other_coal_sng_adj - other_coal_is_adj (if negative, then 0)
# other_coal_ippu_total = sum(other_coal_adjusted_1)

# another other coal national total now appears as a hard-coded total???

# other_coal_adjusted_2 (or, other_coal_adjusted??) = 
  # this new mystery total * (other_coal_adjusted_1 / other_coal)ippu_total)


### Natural Gas----------------------------------------------------------

# there are hard-coded numbers
# blast_furnace_gas_adj
# coke_oven_gas_adj
# ammonia_adj
# is_gas_adj (i & s adjustment for natural gas)
# natural gas national total??? see below

# natural_gas_inc_supplemental = state's NGICB btu/1000 
# natural_gas_supplemental_only = state's SFINB btu/1000 
# natural_gas = natural_gas_inc_supplemental - 
  # natural_gas_supplemental_only

# total_furnace_adj = blast_furnace_gas + coke_oven_gas
# natural_gas_furnace_adj = total_furnace_adj * mysterious hard-coded % used 
  # in the other_coal_is_adj, above

# natural_gas_ammonia_adj = ammonia_adj * another mysterious hard-coded %

# natural_gas_is_adj = is_gas_adj * mysterious hard-coded % used 
# in the other_coal_is_adj, above

# natural_gas_adjusted_1 = natural_gas - natural_gas_furnace_adj -
  # natural_gas_ammonia_adj- natural_gas_is_adj (if negative, then 0)
# natural_gas_ippu_total = sum(natural_gas_adjusted_1)

# another natural gas national total now appears as a hard-coded total???

# natural_gas_adjusted_2 =
  # this new mystery total * (natural_gas_adjusted_1 / natural_gas_ippu_total)

### Residual Fuels-------------------------------------------------------

# there are hard-coded numbers
# cb_abj 
# residual fuel national total??? see below

# residual_fuel = state's RFICB btu / 1000

# residual_fuel_cb_adj = cb_adj * mysterious hard-coded % 

# residual_fuel_adjusted_1 = residual_fuel - 
  # residual_fuel_cb_adj (if negative, then 0)
# residual_fuel_ippu_total = sum(residual_fuel_adjusted_1)

# another residual fuel national total now appears as a hard-coded total???

# residual_fuel_adjusted_2 = 
  # this new mystery total * 
  # (residual_fuel_adjusted_1 / residual_fuel_ippu_total)


### Distillate Fuel------------------------------------------------------

# there are hard-coded numbers
#  is_distillate_fuel_adj
# distillate fuel national total??? see below

# distillate_fuel = state's DFICB btu / 1000

# distillate_fuel_is_adj = is_distillate_fuel_adj * 
  # mysterious hard-coded % used in the other_coal_is_adj, above

# distillate_fuel_adjusted_1 = distillate_fuel - 
  # distillate_fuel_is_adj (if negative, then 0)
# distillate_fuel_ippu_total = sum(distillate_fuel_adjusted_1)

# another distillate fuel national total now appears as a hard-coded total???

# distillate_fuel_adjusted_2 =  
  # this new mystery total * 
  # (distillate_fuel_adjusted_1 / distillate_fuel_ippu_total)


### Gasoline--------------------------------------------------------


# gasoline_inc_ethanol = state's MGICB btu/1000 
# ethanol_only = state's EMICB btu/1000 
# gasoline = gas_with_ethanol - ethanol_only
# sum_gasoline: sum of all states' gasoline

# motor_gasoline_adj = US Compare--Industrial--motor gasoline 
# gasoline_adj = motor_gasoline_adj * (gasoline / sum_gasoline)



### Petroleum Coke---------------------------------------------------

# petroleum_coke = state's PCICB btu/1000 
# sum_petroleum_coke: sum of all states'petroleum coke

# ind_petroleum_coke_adj = US Compare--Industrial--petroleum coke

# petroleum_coke_adj = ind_petroleum_coke_adj * 
    # (petroleum_coke / sum_petroleum_coke)




### LPG----------------------------------------------------------------

# hgl = state's HLICB btu / 1000
# pentanes_plus = state's PPICB btu / 1000
# lpg = hgl - pentanes_plus
# sum_lpg = sum of all states' lpg

# ind_lpg_adj = US SEDS Total--LPG (state's HLICB - PPICB)
# lpg_adj = ind_lpg_adj * (lpg / sum_lpg)




## Transportation Adjustments-------------------------------------------




## Electrical Power Adjustments-----------------------------------------






# State Breakouts (Final)-----------------------------------------------

# Each state has its own worksheet

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

# Adjustments: MMT CO2 (coal only; sum nat gas and petroleum) 
# TO DO



# Cleanup----------------------------------------------------------------

# Remove unneeded data objects
rm(msn_lookup)
seds_not_in_adj <- setdiff(seds_by_state$source_description, adjustments$source_description)
adj_not_in_sed <- setdiff(adjustments$source_description, seds_by_state$source_description)
