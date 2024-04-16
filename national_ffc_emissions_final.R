# Calculate National-Level CO2 Emissions
# National Final Emissions Data

# Objects Created--------------------------------------------------------

# List of objects created in the global environment:



# National FFC Emissions (Final)-----------------------------------------

us_all_adjusted <- bind_rows(us_res_com_ele, us_ind, us_tra) %>%
  # standardize source descriptions for join with carbon_factors
  mutate(source_description = case_when(
    # str_detect(source_description, "naphtha less than") ~ "naphtha", 
    .default = source_description)) %>%
  # Add sector to each coal source--required for carbon factors & NEU
  mutate(source_description = case_when(
    # source_description == "coal" & sector_code == "CC" ~ 
    #   "commercial coal", 
    .default = source_description))


# Calculate Carbon Emissions--------------------------------------------

carbon <- us_all_adjusted %>% 
  left_join(carbon_factors, 
            by =c("source_description", "year")) %>%
  # MMT CO2  = btu * carbon factor/1000 * 44/12
  mutate(mmt_co2 = adjusted_value * 
           (carbon_factor/1000) * carbon_ratio, 
         # Restore original coal source descriptions
         source_description = case_when(
           # str_detect(source_description, "coking coal" ) ~ "coking coal", 
           # str_detect(source_description, "(?<!coking )coal" ) ~ "coal", 
           .default = source_description))
