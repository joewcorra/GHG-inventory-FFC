# Calculate State-Level CO2 Emissions
# State Data Breakouts

print("Collating final data set and computing emissions.")

# Objects Created--------------------------------------------------------

# List of objects created in the global environment:



# State Breakouts (Final)-----------------------------------------------


seds_all_adjusted <- bind_rows(
  seds_com_adjusted, seds_ele_adjusted, seds_ind_adjusted,
  seds_res_adjusted, seds_tra_adjusted) %>%
  # 1/29/2024 START WITH A SINGLE STATE to get the formatting right
  # filter(state == "NY") %>%
  select(state:adjusted_value, -msn_description) %>%
  
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
  # Add sector to each coal source--required for carbon factors & NEU
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
    # Some MSNs are 100% NEU. For these, NEU value = 100% of adjusted value
    neu_adjusted_value = if_else(msn %in% c("ARICB", "LUICB", "FNICB", 
                                            "FOICB", "SNICB", "WXICB", 
                                            "MSICB", "LUACB"), # CLKCB???
                                 adjusted_value, neu_adjusted_value),  
    neu_ibf_adjusted_value = if_else(
      # Subtract NEU only if NEU applies (i.e., isn't NA)
      !is.na(neu_adjusted_value), 
      neu_ibf_adjusted_value - neu_adjusted_value, 
      neu_ibf_adjusted_value))
  

# Calculate Carbon Emissions--------------------------------------------

carbon <- seds_all_adjusted %>% 
  left_join(carbon_factors, 
            by =c("source_description", "year")) %>%
  # MMT CO2  = btu * carbon factor/1000 * 44/12
  mutate(mmt_co2 = neu_ibf_adjusted_value * 
           (carbon_factor/1000) * carbon_ratio, 
         # Restore original coal source descriptions
         source_description = case_when(
           str_detect(source_description, "coking coal" ) ~ "coking coal", 
           str_detect(source_description, "(?<!coking )coal" ) ~ "coal", 
           .default = source_description))


