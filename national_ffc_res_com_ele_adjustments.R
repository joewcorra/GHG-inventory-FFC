# Calculate National-Level CO2 Emissions
# Sectors: Residential, Commercial, Electric Power


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:


# Collate Consumption Data------------------------------------------------

# No adjustments EXCEPT dist fuel and mogas (see those scripts).

us_res_com_ele <- lst(
  
  res = national_ffc_data$us_consumption %>%
    filter(msn %in% c("CLRCB", "NNRCB", "DFRCB", "HLRCB", "KSRCB")), 
  
  com = national_ffc_data$us_consumption %>% 
    filter(msn %in% c("CLCCB", "NNCCB", "DFCCB", "EMCCB", "HLCCB", 
                      "KSCCB", "MGCCB", "PCCCB", "RFCCB")),
           
  ele = national_ffc_data$us_consumption %>% # NNEIB   
    filter(msn %in% c("CLEIB", "NNEIB", "DKEIB", "PCEIB", "RFEIB")),   
           
           
           ) %>%
  
  # ISSUES 3/28/24
  # Electric power needs: distillate fuel
  # Need to adjust for distillate fuel oil (com & res, but not electric?)
  # need to adjust motor gas (com)
  # commercial has ethanol in the dataset but not in the spreadsheet
  

# Collapse list into a single data frame
list_rbind() 

# Cleanup-----------------------------------------------------------------

national_ffc_adjusted <- lst(us_res_com_ele)

rm(us_res_com_ele)
