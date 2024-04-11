# Calculate National-Level CO2 Emissions
# National Final Emissions Data

# Objects Created--------------------------------------------------------

# List of objects created in the global environment:



# National FFC Emissions (Final)-----------------------------------------

us_all_adjusted <- bind_rows(us_res_com_ele, us_ind, us_tra) 


# Calculate Carbon Emissions--------------------------------------------

carbon <- left_join(us_all_adjusted, carbon_factors)