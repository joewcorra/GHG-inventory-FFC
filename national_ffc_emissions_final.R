# Calculate National-Level CO2 Emissions
# National Final Emissions Data

# Objects Created--------------------------------------------------------

# List of objects created in the global environment:



# National FFC Emissions (Final)-----------------------------------------

us_all_adjusted <- bind_rows(national_ffc_adjusted) %>%
  # standardize source descriptions for join with carbon_factors
  mutate(source_description = case_when(
    # str_detect(source_description, "naphtha less than") ~ "naphtha", 
    .default = source_description)) %>%
  # Add sector to each coal source--required for carbon factors & NEU
  mutate(source_description = case_when(
    # source_description == "coal" & sector_code == "CC" ~ 
    #   "commercial coal", 
    .default = source_description))


# Metadata Labeling----------------------------------------------------

var_label(us_all_adjusted) <- list(
  year = "year of SEDS data",
  msn = "SEDS five-character mnemonic series name: Energy source + Energy sector + Data type",
  value = "Value of the observed MSN data in units ",
  msn_description = "Full description of MSN",
  unit = "Units of this MSN's value",
  source_code = "Energy source code; i.e., first and second digits of MSN",
  sector_code = "Energy sector code; i.e., third and fourth digits of MSN",
  source_description = "description of MSN energy source code",
  sector_description = "description of MSN energy sector code")

gt(head(us_all_adjusted)) %>% opt_stylize(style = 6, color = "red")

# Calculate Carbon Emissions--------------------------------------------

carbon_emissions <- us_all_adjusted %>% 
  left_join(carbon$carbon_factors, 
            by =c("source_description", "year")) %>%
  # MMT CO2  = btu * carbon factor/1000 * 44/12
  mutate(mmt_co2 = adjusted_value * 
           (carbon_factor/1000) * carbon_ratio, 
         # Restore original coal source descriptions
         source_description = case_when(
           # str_detect(source_description, "coking coal" ) ~ "coking coal", 
           # str_detect(source_description, "(?<!coking )coal" ) ~ "coal", 
           .default = source_description))

var_label(carbon_emissions) <- list(
  mmt_co2 = "Calculated CO2 emissions (millions of metric tons)")
