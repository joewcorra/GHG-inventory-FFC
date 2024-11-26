# Calculate State-Level CO2 Emissions
# State Data Breakouts

print("Collating final data set and computing emissions.")

# Objects Created--------------------------------------------------------

# List of objects created in the global environment:



# State Breakouts (Final)-----------------------------------------------


seds_all_adjusted <- list_rbind(seds_adjusted %>% 
                                  # Remove IBF and NEU data for now
                                  discard(names(.) %in% 
                                            c("seds_ibf_adjusted", 
                                              "seds_neu_adjusted"))) %>%
  select(state:adjusted_value, -eia_description) %>%
  # NOTE: maybe move this to carbon calculations, below
  # standardize source descriptions for join with carbon_factors
  mutate(source_description = case_when(
    str_detect(source_description, "naphtha less than") ~ "naphtha", 
    str_detect(source_description, "other oils") ~ "other oils", 
    str_detect(source_description, " and ") ~ 
      str_replace(source_description, " and ", " & "),
    str_detect(source_description, "aviation gasoline c") ~ "aviation gasoline",
    source_description == "aviation gasoline blending components" ~ 
      "avgas blend components",
    source_description == "motor gasoline blending components" ~ 
      "mogas blend components",
    str_detect(source_description, "hydrocarbon|propane") ~ "lpg",
    str_detect(source_description, "miscellaneous") ~ "misc. products",
    str_detect(source_description, "distillate ") ~ "distillate fuel oil",
    str_detect(source_description, "residual") ~ "residual fuel oil",
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
    source_description == "coal" & sector_code == "AC" ~ 
      "transportation coal",
    source_description == "coal" & sector_code == "RC" ~ 
      "residential coal",
    .default = source_description)) %>%
  
  # Subtract NEU and IBF adjustments
  # Join with NEU adjusted data
  left_join(seds_adjusted$seds_neu_adjusted, 
            by = c("sector_description", "source_description", "year", 
                   "state", "msn")) %>%
  # Join with IBF adjusted data
  left_join(seds_adjusted$seds_ibf_adjusted, 
            by = c("sector_description", "source_description", "year", 
                   "state", "msn")) %>%
  # Get adjusted value - NEU and IBD values = final adjusted tBtu 
  mutate(neu_ibf_adjusted_value = if_else(
    # Subtract IBF only if IBF applies (i.e., isn't NA)
    !is.na(ibf_adjusted_value), 
    adjusted_value - ibf_adjusted_value, 
    adjusted_value), 
    # Some MSNs are 100% NEU. For these, NEU value = 100% of adjusted value
    neu_adjusted_value = if_else(msn %in% c("ARICB", "LUICB", "FNICB", "CLKCB",
                                            "FOICB", "SNICB", "WXICB", 
                                            "MSICB", "LUACB"), 
                                 adjusted_value, neu_adjusted_value),  
    neu_ibf_adjusted_value = if_else(
      # Subtract NEU only if NEU applies (i.e., isn't NA)
      !is.na(neu_adjusted_value), 
      neu_ibf_adjusted_value - neu_adjusted_value, 
      neu_ibf_adjusted_value)) %>% 
  
  # Finally, ZERO OUT all pentanes plus and unfinished oils
  mutate(neu_ibf_adjusted_value = case_when(
    source_description == "pentanes plus" ~ 0, 
    source_description == "unfinished oils" ~ 0, 
    .default = neu_ibf_adjusted_value))

# Apply labels to variables
seds_all_adjusted <- apply_variable_labels(seds_all_adjusted)

# Calculate Carbon Emissions--------------------------------------------

carbon_emissions <- seds_all_adjusted %>% 
  left_join(carbon$carbon_factors, 
            by =c("source_description", "year")) %>%
  # MMT CO2  = btu * carbon factor/1000 * 44/12
  mutate(mmt_co2 = neu_ibf_adjusted_value * 
           (carbon_factor/1000) * carbon$carbon_ratio, 
         # Restore original coal source descriptions
         source_description = case_when(
           str_detect(source_description, "coking coal" ) ~ "coking coal", 
           str_detect(source_description, "(?<!coking )coal" ) ~ "coal", 
           .default = source_description))

# Apply labels to variables
carbon_emissions <- apply_variable_labels(carbon_emissions)

# Cleanup----------------------------------------------------------------

state_ffc_results <- append(state_ffc_results, lst(seds_all_adjusted,
                                                   carbon_emissions))

rm(list = c("seds_all_adjusted", "carbon_emissions"))

