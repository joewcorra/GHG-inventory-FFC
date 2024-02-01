# Calculate State-Level CO2 Emissions
# State Data Breakouts

# Objects Created--------------------------------------------------------

# List of objects created in the global environment:



# State Breakouts (Final)-----------------------------------------------

# Split into list elements by state
seds_all_adjusted %>% # TBD
  group_by(state) %>%
  group_split() 


# 1/29/2024 this might be a good place to remove unneeded columns
seds_all_adjusted <- bind_rows(
  seds_com_adjusted, seds_ele_adjusted, seds_ind_adjusted,
  seds_res_adjusted, seds_tra_adjusted) %>%
  # 1/29/2024 START WITH A SINGLE STATE to get the formatting right
  filter(state == "NY") %>%
  select(state:adjusted_value, -type, -msn_description) %>%
  
  # NOTE: maybe move this to carbon calculations, below
  # standardize source descriptions for join with carbon_factors
  mutate(source_description = case_when(
    str_detect(source_description, "naphtha less than") ~ "naphtha", 
    str_detect(source_description, "other oils") ~ "other oils", 
    str_detect(source_description, " and ") ~ 
      str_replace(source_description, " and ", " & "),
    str_detect(source_description, "aviation") ~ "avgas blend components",
    source_description == "motor gasoline blending components" ~ 
      "mogas blend components",
    str_detect(source_description, "pentanes") ~ "pentanes plus",
    str_detect(source_description, "hydrocarbon") ~ "hgl",
    str_detect(source_description, "miscellaneous") ~ "misc. products",
    str_detect(source_description, "residual") ~ "residual fuel",
    .default = source_description)) %>%
  # Add sector to each coal source
  mutate(source_description = case_when(
    source_description == "coal" & sector_code == "CC" ~ 
      "commercial coal", 
    source_description == "coal" & sector_code == "EI" ~ 
      "electric power coal",
    source_description == "coal" & sector_code == "OC" ~ 
      "industrial other coal",
    source_description == "coal" & sector_code == "KC" ~ 
      "industrial coking coal",
    source_description == "coal" & sector_code == "RC" ~ 
      "residential coal",
    .default = source_description)) %>%
  
  # Subtract NEU and IBF adjustments
  # Join with NEU adjusted data
  left_join(seds_neu_adjusted, 
            by = c("sector_description", "source_description", "year", 
                   "state", "msn")) %>%
  # Join with IBF adjusted data
  left_join(seds_ibf_adjusted, 
            by = c("sector_description", "source_description", "year", 
                   "state", "msn")) %>%
  # Get adjusted value - NEU and IBD values = final adjusted tBtu 
    mutate(neu_ibf_adjusted_value = if_else(
      # Subtract IBF only if IBF applies (i.e., isn't NA)
    !is.na(ibf_adjusted_value), 
    adjusted_value - ibf_adjusted_value, 
    adjusted_value), 
    neu_ibf_adjusted_value = if_else(
      # Subtract NEU only if NEU applies (i.e., isn't NA)
      !is.na(neu_adjusted_value), 
      neu_ibf_adjusted_value - neu_adjusted_value, 
      neu_ibf_adjusted_value))
  

# Calculate Carbon Emissions--------------------------------------------

carbon <- seds_all_adjusted %>% 
  left_join(carbon_factors, 
            by =c("source_description", "year")) %>%
  mutate(mmt_co2 = neu_ibf_adjusted_value * 
           (carbon_factor/1000) * carbon_ratio)


# Remove unneeded data objects
rm(carbon_factors_variable)

# Notes from Review of Excel Workbook-----------------------------------

# Adjusted Residential, Commercial, Industrial, Transportation, Elec Power

# Additional Adjustments: minus NEU or IBF (Ind and Trans) for selected sources

# Carbon calculations: 
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