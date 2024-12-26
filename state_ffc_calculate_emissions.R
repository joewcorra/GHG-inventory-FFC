# Carbon emissions


# Calculate Carbon Emissions--------------------------------------------


state_ffc_calculate_emissions <- function(seds_all_adjusted, carbon)  {
  
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

return(carbon_emissions)

}
